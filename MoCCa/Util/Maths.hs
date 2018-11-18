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

-- | Round the given value to n decimal places and then convert it to a string.
roundAndFormat :: Int -> Double -> String
roundAndFormat n x =
    let shownJ = show j
        nZeroes = n - (length shownJ)
        shownDecimal = (replicate nZeroes '0') ++ shownJ
    in (show i) ++ "." ++ shownDecimal
    where (i, j) = roundExact n x

-- | Convert the given value from radians to degrees.
toDegrees :: Double -> Double
toDegrees theta = theta * (180 / pi)
