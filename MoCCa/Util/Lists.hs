module MoCCa.Util.Lists where

-- | Find the values of the list either side of the point at which
-- the given predicate first switches from True to False or vice versa,
-- when applied element by element from the beginning of the list.
boundPredicateSwitch :: (a -> Bool) -> [a] -> Maybe (a, a)
boundPredicateSwitch predicate (x:y:zs) =
    if predicate x /= predicate y then Just (x, y)
    else boundPredicateSwitch predicate (y:zs)
boundPredicateSwitch _ _ = Nothing
