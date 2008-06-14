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
    linear,
    -- * Queries
    freedomIndices, stiffnessMatrix, render,
--    displacementsScale,
    -- * Utils
    noRender
    ) where

import qualified ElementMatrix
import qualified Node
import qualified Graphics.Rendering.Cairo as Cairo

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
      -- TODO: м.б. здесь ноды оставить, а не индексы?
      -- для описания элемента наверное проще ноды
      -- а для общей работы проще индексы
      -- потом посмотрим, чего больше и, возможно, переделаем.
      stiffnessMatrix :: ElementMatrix.M,
      render :: ElementRender
    }         

-- | Linear element creation, @linearElement stiffnessMatrix freedomIndices@
linear :: ElementMatrix.M -> ElementMatrix.FI -> ElementRender -> E
linear sm fi r =
    E
    { freedomIndices = fi,
      stiffnessMatrix = sm,
      render = r
    }
