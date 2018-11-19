module MoCCa.Nozzle.MinimumLength where

import qualified MoCCa.FlowTables.Isentropic as IFT

type Coordinate = (Double, Double)
data PointType = Throat | Flow | Centreline | Wall deriving (Show)
data Point = Point { riemannInvariants :: (Double, Double)
                   , machNumber :: Double
                   , machAngle :: Double
                   , flowAngle :: Double
                   , prandtlMeyerFunction :: Double
                   , characteristicAngles :: (Double, Double)
                   , position :: Coordinate
                   , pointType :: PointType
                   , name :: String
                   } deriving (Show)

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

t = IFT.generateFlowTable 1.4 1.0 0.01
