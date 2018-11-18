module MoCCa.Util.Maths where

-- | Linearly interpolate between two sets of points in the x- and y-directions.
linearInterpolate :: (Double, Double) -> Double -> (Double, Double) -> Double
linearInterpolate (x0, x1) x (y0, y1) =
    let y = y0 + (dy * r) in y
    where
        dy = y1 - y0
        dx = x1 - x0
        r = (x - x0) / dx

-- | Express a Double, rounded to n decimal places, as i + (j / (10 ^ n)),
-- where i and j are the Int outputs of this function.
roundExact :: Int -> Double -> (Int, Int)
roundExact n x = (integerPart, decimalPart)
    where
        integerPart = floor x :: Int
        decimalPart = 
            let base = 10 :: Int
                decimal = x - (fromIntegral integerPart :: Double)
            in round (decimal * fromIntegral (base ^ n)) :: Int
