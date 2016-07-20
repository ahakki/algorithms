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

-- this compares each element of the list with the element
-- after it, and if the condition (cond) is met, applies an
-- action to it.
condFoldl cond action (x:y:ys)
    | cond x y =
        action x : y : condFoldl cond action ys
    | otherwise =
        x : condFoldl cond action (y : ys)

condFoldl _ _ [x] =
    [x]

condFoldl _ _ _ =
    []
