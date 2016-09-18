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

-- Turns a Number into a List of it's Digits
numToList :: Integer -> [Integer]
numToList =
    ntl . show
    where
        ntl (x:xs) =
            (read [x] :: Integer) : (ntl xs)
        ntl [] =
            []
