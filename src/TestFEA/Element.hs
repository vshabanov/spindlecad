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
    -- * Creation
    linearElement,
    -- * Queries
    freedomIndices, stiffnessMatrix
    ) where

import ElementMatrix

-- | Finite element description
data E = E
    { freedomIndices :: ElementMatrix.FI,
      stiffnessMatrix :: ElementMatrix.M 
    }
    deriving Show

-- | Linear element creation, @linearElement stiffnessMatrix freedomIndices@
linearElement :: ElementMatrix.M -> ElementMatrix.FI -> E
linearElement sm fi = E { freedomIndices = fi,
                          stiffnessMatrix = sm 
                        }