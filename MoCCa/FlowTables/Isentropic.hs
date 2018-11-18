module MoCCa.FlowTables.Isentropic
( machNumber
, machAngle
, prandtlMeyerFunction
, generateFlowTable
, lookupMachNumber
, lookupMachAngle
, lookupPrandtlMeyerFunction
) where

import qualified Data.List as List
import qualified MoCCa.Util.Maths as Maths
import qualified MoCCa.Util.Lists as ListUtils

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

type IsentropicFlowTable = [Row]

-- | Generate an infinite flow table starting at the given
-- Mach number and continuing in steps.
generateFlowTable :: Double -> Double -> Double -> IsentropicFlowTable
generateFlowTable gamma m0 step = (calculateTableRow gamma m0) :
    (generateFlowTable gamma (m0 + step) step)

-- | Find the first consecutive rows in the flow table for which the given
-- condition gives different truth values.
searchFlowTable :: IsentropicFlowTable -> (Row -> Bool) -> Maybe (Row, Row)
searchFlowTable table condition = ListUtils.boundPredicateSwitch condition table

-- | Interpolate linearly between two rows of a flow table based on their distance
-- between the two values produced when applying the given extractor function.
linearInterpolateRows :: (Row -> Double) -> Double -> (Row, Row) -> Row
linearInterpolateRows extract desiredValue (r0@(Row m0 mu0 nu0), r1@(Row m1 mu1 nu1)) =
    Row (between m0 m1) (between mu0 mu1) (between nu0 nu1)
    where
        between x0 x1 = Maths.linearInterpolate (extract r0, extract r1) desiredValue (x0, x1)

-- | Look up a Mach number in the given isentropic flow table, and
-- interpolate between the two closest rows.
lookupMachNumber :: IsentropicFlowTable -> Double -> Maybe Row
lookupMachNumber table m = lookupValue (\r -> machNumber r > m) (machNumber) table m

-- | Look up a Mach angle in the given isentropic flow table, and
-- interpolate between the two closest rows.
lookupMachAngle :: IsentropicFlowTable -> Double -> Maybe Row
lookupMachAngle table mu = lookupValue
    (\r -> machAngle r < mu) (machAngle) table mu

-- | Look up a Prandtl-Meyer angle in the given isentropic flow table, and
-- interpolate between the two closest rows.
lookupPrandtlMeyerFunction :: IsentropicFlowTable -> Double -> Maybe Row
lookupPrandtlMeyerFunction table nu = lookupValue
    (\r -> prandtlMeyerFunction r > nu) (prandtlMeyerFunction) table nu

-- | Look up a value in the given flow table, selecting the rows between which to
-- interpolate based on a predicate and then using the extracted values as
-- bounds for the interpolation.
lookupValue :: (Row -> Bool) -> (Row -> Double)
    -> IsentropicFlowTable -> Double -> Maybe Row 
lookupValue predicate extractor table desiredValue =
    let rows = searchFlowTable table predicate
    in fmap (linearInterpolateRows extractor desiredValue) rows
