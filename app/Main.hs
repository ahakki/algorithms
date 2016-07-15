module Main where

import Eratosthenes
import System.Environment

main :: IO ()
main =
    getArgs >>= \n ->
    (print . sieve) $ (read . head) n
