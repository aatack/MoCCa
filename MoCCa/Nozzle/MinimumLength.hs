module MoCCa.Nozzle.MinimumLength where

import qualified MoCCa.FlowTables.Isentropic as IFT

type Coordinate = (Double, Double)
data PointType = Throat | Flow | Centreline | Wall deriving (Show)
data Point = Point { riemannInvariants :: (Double, Double)
                   , machNumber :: Double
                   , machAngle :: Double
                   , flowAngle :: Double
                   , prandtlMeyerFunction :: Double
                   , position :: Coordinate
                   , pointType :: PointType
                   } deriving (Show)

-- | Determine the angles of the characteristic lines coming from
-- the given point.
characteristicAngles :: Point -> (Double, Double)
characteristicAngles p = 
    let theta = flowAngle p
        mu = machAngle p
    in (theta + mu, theta - mu)

-- | Generate n equally spaced starting angles for characteristic
-- lines between the two limits.
generateStartingAngles :: Int -> (Double, Double) -> [Double]
generateStartingAngles n (min, max) = 
    let dtheta = (max - min) / (fromIntegral (n - 1))
    in [min + (fromIntegral i) * dtheta | i <- [0..n - 1]]

-- | Find the nozzle angle in radians immediately after the throat such
-- that the flow is uniform at the exit.
nozzleThroatAngle :: IFT.IsentropicFlowTable -> Double -> Double
nozzleThroatAngle table exitMachNumber =
    case IFT.lookupMachNumber table exitMachNumber of
        Just row -> 0.5 * IFT.prandtlMeyerFunction row
        Nothing  -> error "table does not contain the requested Mach number"

-- | Create a point and populate its field, assuming that it is located
-- at the top of the throat and its flow angle is given.
createInitialPoint :: IFT.IsentropicFlowTable -> Double -> Point
createInitialPoint table angle =
    Point { riemannInvariants = (0, 2 * angle)
          , machNumber = IFT.machNumber tableRow
          , machAngle = IFT.machAngle tableRow
          , flowAngle = angle
          , prandtlMeyerFunction = angle
          , position = (0, 1)
          , pointType = Throat
          }
    where
        tableRow = case IFT.lookupPrandtlMeyerFunction table angle of
            Just r  -> r
            Nothing -> error "table does not contain the requested P-M value"
