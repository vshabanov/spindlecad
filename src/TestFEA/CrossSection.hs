-- | 
-- Module      :  TestFEA.CrossSection
-- Copyright   :  Vladimir Shabanov 2008
-- License     :  GPL (see the LICENSE file in the distribution)
--
-- Maintainer  :  vshabanoff@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Some common crossection descriptions
--
module CrossSection (
    -- * Types
    CrossSection,
    -- * Creation
    circle, ring,
    -- * Queries
    area, areaMomentOfInertia,
    timoshenko_k,
    timoshenko_A_s,
    ) where

import ElementMatrix
import Material

-- | Cross section description (only rings and circles are currently supported)
data CrossSection =
    Ring
    { dIn :: D,
      dOut :: D 
    }

-- | @ring dIn dOut@
ring :: D -> D -> CrossSection
ring = Ring

-- | @circle diameter@
circle :: D -> CrossSection
circle = ring 0

-- | cross section's area
area :: CrossSection -> D
area cs =
    pi * (dOut cs ^ 2 - dIn cs ^ 2) / 4

-- | cross section's area moment of intertia (second moment of area)
areaMomentOfInertia :: CrossSection -> D
areaMomentOfInertia cs =
    pi * (dOut cs ^ 4 - dIn cs ^ 4) / 64

-- | calculates Timoshenko's /k/ coefficient
timoshenko_k :: Material -> CrossSection -> D
timoshenko_k mat cs =
        6*(1+nu)*(1+m^2)^2 / ((7+6*nu)*(1+m^2)^2 + (20+12*nu)*m^2)
    where m = dIn cs / dOut cs
          nu = poissonRatio mat

-- | calculates Timoshenko's /A_s/ coefficient
timoshenko_A_s :: Material -> CrossSection -> D
timoshenko_A_s mat cs  =
    timoshenko_k mat cs * area cs
