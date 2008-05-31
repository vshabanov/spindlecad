-- | 
-- Module      :  TestFEA.Elements.BernoulliEulerBeam2D
-- Copyright   :  Vladimir Shabanov 2008
-- License     :  GPL (see the LICENSE file in the distribution)
--
-- Maintainer  :  vshabanoff@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- 2D plane Bernoulli-Euler beam, currently only linear element
--
module Elements.BernoulliEulerBeam2D (
    bernoulliEulerBeam2D,
    ) where

import Element
import Node
import Material
import CrossSection
import Elements.TimoshenkoBeam2D

-- | Currently Y & C components must be equal to zero. Force on X axis
-- is ignored (1 in matrix diagonal).
-- Actually it's the same code as timoshenkoBeam2D except /f/
-- component is equal to zero.
bernoulliEulerBeam2D :: Node.XYC -> Node.XYC -> Material -> CrossSection -> E
bernoulliEulerBeam2D n1 n2 mat cs =
    timoshenkoBeam2D n1 n2 (mat { modulusOfRigidity = 1/0 }) cs
