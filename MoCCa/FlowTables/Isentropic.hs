module MoCCa.FlowTables.Isentropic where

import qualified Data.List as List
import qualified MoCCa.Util.Maths as Maths

data Row = Row { machNumber :: Double
               , machAngle :: Double
               , prandtlMeyerFunction :: Double
               }

instance Show Row where
    show = asCSVDegrees 2

-- | Calculate the column values of an isentropic flow table using the
-- given values of gamma and M.
calculateTableRow :: Double -> Double -> Row
calculateTableRow gamma m =
    let mu = calculateMachAngle m
        nu = calculatePrandtlMeyerFunction gamma m
    in Row m mu nu

-- | Calculate the Mach angle, in radians, that corresponds
-- to the given Mach number.
calculateMachAngle :: Double -> Double
calculateMachAngle m = asin (1 / m)

-- | Calculate the value of the Prandtly Meyer function for the
-- given value of gamma and the Mach number.
calculatePrandtlMeyerFunction :: Double -> Double -> Double
calculatePrandtlMeyerFunction gamma m =
    let sqrtGammaTerm = sqrt $ (gamma + 1) / (gamma - 1)
        sqrtMTerm = sqrt $ (m * m) - 1
    in sqrtGammaTerm * (atan $ sqrtMTerm / sqrtGammaTerm) - (atan sqrtMTerm)

-- | Convert the given row to a series of comma-separated values,
-- rounding each one to the given number of decimal places, with the
-- Mach and Prandtl-Meyer angles represented as degrees.
asCSVDegrees :: Int -> Row -> String
asCSVDegrees decimalPlaces row =
    let m = machNumber row
        mu = Maths.toDegrees . machAngle $ row
        nu = Maths.toDegrees . prandtlMeyerFunction $ row
    in List.intercalate "," . map (Maths.roundAndFormat decimalPlaces) $ [m, mu, nu]
