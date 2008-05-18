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
    ElementRender,
    -- * Creation
    linearElement,
    -- * Queries
    freedomIndices, stiffnessMatrix, render
    ) where

import ElementMatrix
import Node
import Graphics.Rendering.Cairo

type ElementRender = [Node.C] -> Render ()

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
linearElement :: ElementMatrix.M -> ElementMatrix.FI -> ElementRender -> E
linearElement sm fi r =
    E
    { freedomIndices = fi,
      stiffnessMatrix = sm,
      render = r
    }
