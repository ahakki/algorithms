module Math where

-- Sieve of Eratosthenes
sieve :: Integral a => a -> [a]
sieve l =
    sieve' [2..l] []
  where
    sieve' (p:ns) ps =
        sieve' (filter (\x -> rem x p /= 0) ns) (p : ps)
    sieve' [] ps =
        reverse ps

condFoldl cond action (x:y:ys)
    | cond x y =
        x : condFoldl cond action (y : ys)
    | otherwise =
        [action x, y] ++ condFoldl cond action ys

condFoldl _ _ [x] =
    [x]

condFoldl _ _ _ =
    []
