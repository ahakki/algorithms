module Eratosthenes where

sieve n =
    sieve' [2..n] []
  where
    sieve' (p:ns) ps =
        sieve' (filter (\x -> rem x p /= 0) ns) (p : ps)
    sieve' [] ps =
        reverse ps
