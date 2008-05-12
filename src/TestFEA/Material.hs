-- | 
-- Module      :  TestFEA.Material
-- Copyright   :  Vladimir Shabanov 2008
-- License     :  GPL (see the LICENSE file in the distribution)
--
-- Maintainer  :  vshabanoff@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Material description
--
module Material (
    -- * Types
    Material,
    -- * Predefined materials
    steel,
    -- * Queries
    name,
    materialE, modulusOfElasticity, youngModulus,
    materialG, modulusOfRigidity, shearModulus,
    materialnu, poissonRatio    
    ) where

import ElementMatrix

-- | Material parameters
data Material = IsotropicMaterial
    { name                :: String,
      modulusOfElasticity :: D, -- Value Pascal -- E
      modulusOfRigidity   :: D,
      poissonRatio        :: D
    }
    deriving (Eq, Ord, Show)

-- | Default steel material
steel :: Material
steel = IsotropicMaterial
    { name = "steel",
      modulusOfElasticity = 2.1*10^5,
      modulusOfRigidity   = 0.8*10^5,
      poissonRatio        = 0.3 
    }

-- | same as modulusOfElasticity
materialE :: Material -> D
materialE = modulusOfElasticity

-- | same as modulusOfElasticity
youngModulus :: Material -> D
youngModulus = modulusOfElasticity

-- | same as modulusOfRigidity
materialG :: Material -> D
materialG = modulusOfRigidity

-- | same as modulusOfRigidity
shearModulus :: Material -> D
shearModulus = modulusOfRigidity

-- | same as poissonRatio
materialnu :: Material -> D
materialnu = poissonRatio
