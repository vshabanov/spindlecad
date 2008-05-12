-- | Finite element matrices utilities
module ElementMatrix (
    -- * Types
    D,
    EM,
    -- * Functions
    matrix
    ) where

import Numeric.LinearAlgebra as LA

-- | Matrix element type
type D = Double

-- | Finite element matrix
type EM = LA.Matrix D

-- | Makes fresh square finite element matrix
matrix :: Int -> [D] -> EM
matrix n e = (n><n) e
