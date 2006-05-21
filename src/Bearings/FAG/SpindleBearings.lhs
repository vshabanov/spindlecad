--
--  Copyright (C) 2006 Vladimir Shabanov
--
--  This file is part of SpindleCAD.
--
--  SpindleCAD is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  SpindleCAD is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with Foobar; if not, write to the Free Software
--  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
--

List of FAG hi-precision spindle bearings and their parameters

> module Bearings.FAG.SpindleBearings (
>     bearingsList,
>     findBearingByCode,
>     spindleBearingType
>   ) where

> import Bearing
> import TypeLevelPhysicalValue
> import TypeLevelPhysicalUnitsList
> import Text.ParserCombinators.Parsec
> import Data.Ratio
> import Data.List
> import qualified Data.Map as Map
> import qualified Bearings.FAG.SpindleBearingsCatalogue


List of all bearings from catalogue

> bearingsList =
>     map (\ s -> case parse bearingStringParser "" s of
>                     Left err -> error $ "Can't parse bearing description: "
>                                         ++ s ++ ":" ++ show err
>                     Right b  -> b)
>     Bearings.FAG.SpindleBearingsCatalogue.list

Utility for finding bearing using its code

> findBearingByCode =
>     let codemap = Map.fromList $ map (\ b -> (code b, b)) bearingsList
>     in
>     \ code -> Map.findWithDefault (error $ "bearing " ++ code ++ " not found")
>                 code codemap


Bearing description string parsing routines.

> bearingStringParser :: Parser Bearing
> bearingStringParser = do
>      input <- getInput
>      let code = head $ words input
>      -- bearing code
>      btype <- parsePrefix spindleBearingType
>      bdser <- parsePrefix spindleBearingDimensionSeries
>      bbore <-     try (parsePrefix spindleBearingBoreReferenceNumbers2)
>               <|> try (do d1 <- digit
>                           d2 <- digit
>                           return $ fromInteger (read [d1,d2] * 5) .* mm)
>               <|> parsePrefix spindleBearingBoreReferenceNumbers1
>      cangl <- parsePrefix spindleBearingContactAngle
>      optional (parsePrefix spindleBearingExternalForm)
>      optional (parsePrefix spindleBearingSealing)
>      parsePrefix spindleBearingCage
>      parsePrefix spindleBearingAccuracy
>      -- dimensions
>      d <- num mm
>      dD <- num mm
>      bwidth <- num mm
>      rsmin <- num mm
>      r1smin <- num mm
>      -- abutment dimensions
>      da <- num mm
>      dDa <- num mm
>      ra <- num mm
>      rb <- num mm
>      -- DLR dimensions
>      bN <- num mm
>      sN <- num mm
>      sB <- num mm
>      -- Etk
>      etk <- num mm
>      -- load ratings
>      cdyn <- num kN
>      c0stat <- num kN
>      -- attainable speeds
>      speedGrease <- num rpm
>      speedOil <- num rpm
>      -- preloading forces
>      fvl <- num newton
>      fvm <- num newton
>      fvh <- num newton
>      -- unloading forces
>      kael <- num newton
>      kaem <- num newton
>      kaeh <- num newton
>      -- axial rigidities
>      sal <- num (newton /. micro meter)
>      sam <- num (newton /. micro meter)
>      sah <- num (newton /. micro meter)
>      -- sealed desing avalability
>      space
>      oneOf ['\150', '\149']
>      -- weight
>      weight <- num kilogram
>      -- bearing code duplicated
>      many1 anyChar
>      -- return the result bearing description
>      return $ Bearing
>                 { manufacturer = "FAG",
>                   code = code,
>                   designation = code ++ ".UL",
>                   bearingType = btype,
>                   dimensionSeries = bdser,
>                   contactAngle = cangl,
>                   innerDiameter = bbore,
>                   outerDiameter = dD,
>                   width = bwidth,
>                   attainableSpeedGrease = speedGrease,
>                   attainableSpeedOil = speedOil,
>                   radialRigidity = (if cangl == 15*degree then 6/2 else 2/2) .* sal,
>                   axialRigidity = (1/2) .* sal
>                 }

> num :: Value a -> Parser (Value a)
> num v = do space
>            d <- many1 digit
>            (try (do char '.'
>                     d2 <- many1 digit
>                     return $ fromRational ((read (d++d2) :: Integer)
>                                            % (10 ^ length d2)) .* v)
>             <|>
>             do return $ fromInteger (read d :: Integer) .* v)

> parsePrefix :: [(String, a)] -> Parser a
> parsePrefix [] = error "parsePrefix: empty list given"
> parsePrefix [(s,r)] = try (do string s
>                               return r)
> parsePrefix ((s,r):xs) = try (do string s
>                                  return r)
>                          <|>
>                          parsePrefix xs

> spindleBearingType =
>     [("B"  , "Standard bearing. Steel balls."),
>      ("HCB", "Hybrid standard. Ceramic balls."),
>      ("XCB", "X-life ultra. Ceramic balls."),
>      ("HSS", "High-speed bearing. Steel balls, sealed."),
>      ("HS" , "High-speed bearing. Steel balls."),
>      ("HCS", "High-speed bearing. Ceramic balls, sealed."),
>      ("HC" , "High-speed bearing. Ceramic balls."),
>      ("XCS", "X-life ultra. High-speed bearing. Ceramic balls, sealed."),
>      ("XC" , "X-life ultra. High-speed bearing. Ceramic balls.")]

> spindleBearingDimensionSeries =
>     [("718", UltraLightweightDimensionSeries),
>      ("719", LightweightDimensionSeries),
>      ("70",  MediumDimensionSeries),
>      ("72",  HeavyDimensionSeries)]

> spindleBearingBoreReferenceNumbers2 =
>     [("00", 10.*mm),
>      ("01", 12.*mm),
>      ("02", 15.*mm),
>      ("03", 17.*mm)]

> spindleBearingBoreReferenceNumbers1 =
>     [("6",  6.*mm),
>      ("7",  7.*mm),
>      ("8",  8.*mm),
>      ("9",  9.*mm)]

> spindleBearingContactAngle =
>     [("C", 15*degree),
>      ("E", 25*degree)]

> spindleBearingExternalForm =
>     [("DLR", "DIRECT LUBE. Direct lubrication via integral O-rings."),
>      (""   , "Standard.")]

> spindleBearingSealing =
>     [(".2RSD", "Seals at both sides and greased."),
>      (""     , "No sealing.")]

> spindleBearingCage =
>     [(".TPA", "Textile laminated phenolic resin. Series B718."),
>      (".T", "Textile laminated phenolic resin.")]

> spindleBearingAccuracy =
>     [(".P4S", "P4S FAG standard accuracy."),
>      (".P4" , "P4 FAG standard accuracy.")]
