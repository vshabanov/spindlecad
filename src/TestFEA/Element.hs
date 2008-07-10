-- | 
-- Module      :  TestFEA.Element
-- Copyright   :  Vladimir Shabanov 2008
-- License     :  GPL (see the LICENSE file in the distribution)
--
-- Maintainer  :  vshabanoff@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Finite element description.
--
module Element (
    -- * Types
    E,
    RenderParameters (..),
    ElementRender,
    -- * Creation
    linear, nonlinear, composite,
    -- * Queries
    freedomIndices, stiffnessMatrix, render,
    -- * Utils
    noRender
    ) where

import qualified ElementMatrix
import qualified Node
import qualified Graphics.Rendering.Cairo as Cairo
import Data.List
import Data.Maybe

-- | Parameters necessary to render element
data RenderParameters =
    RenderParameters
    { displacementsScale :: ElementMatrix.D
    }

-- | Render element using render parameters and node displacements
type ElementRender = RenderParameters -> [Node.C] -> Cairo.Render ()
    -- TODO: тут пока передаем смещения, надо посмотреть,
-- м.б. правильнее передавать координаты? будет понятно после
-- реализации нелинейных элементов.

-- | Empty element render
noRender :: ElementRender
noRender _rp _d = return ()

-- | Finite element description
data E =
    E
    { freedomIndices :: ElementMatrix.FI,
      stiffnessMatrix :: [Node.C] -> ElementMatrix.M,
      render :: ElementRender
    }         

-- | Linear element creation,
-- @linear stiffnessMatrix freedomIndices renderer@
linear :: ElementMatrix.M -> ElementMatrix.FI -> ElementRender -> E
linear sm fi r =
    nonlinear (\ _ -> sm) fi r

-- | Non-linear element creation, stiffness matrix depends on node
-- displacements (which are in the same order as freedom indices list).
nonlinear :: ([Node.C] -> ElementMatrix.M) -> ElementMatrix.FI
          -> ElementRender -> E
nonlinear sm fi r =
    E
    { freedomIndices = fi,
      stiffnessMatrix = sm,
      render = r
    }

-- | Composite element
composite :: [E] -> E
composite elts =
    E
    { freedomIndices = fis,
      stiffnessMatrix = \ nc ->
          fst $ ElementMatrix.assemble $
              zip (mapnc nc stiffnessMatrix elts)
                  (map freedomIndices elts),
      render = \ rp nc ->
          sequence_ $ mapnc nc (\ elt nc' -> render elt rp nc') elts
    }
    where fis = ElementMatrix.mergeFIs $ map freedomIndices elts
          -- | map which remaps composite element node displacements to
          -- element's node displacements
          mapnc nc f = map (\ elt -> f elt (nc' elt))
              where fiToNC = zip fis nc
                    nc' elt = map (fromJust . flip lookup fiToNC) $
                              freedomIndices elt
