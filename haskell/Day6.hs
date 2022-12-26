#!/usr/bin/env cabal
{- cabal:
build-depends: base
ghc-options: -O2
-}
module Main where

-- is any element in the list repeated
unique :: Eq a => [a] -> Bool
unique [    ]               = True
unique (x:xs) | x `elem` xs = False
unique (_:xs)               = unique xs 

-- sliding window of n
slide :: Int -> [a] -> [[a]]
slide n = transpose . take n . tails

-- find marker position given unique marker of n elems
getMarker :: Eq a => Int -> [a] -> Int
getMarker n = (+n) . fst . head . filter (unique . snd) . zip [0..] . slide n

main :: IO ()
main = do
    path <- head <$> getArgs
    input <- readFile path
    putStrLn $ mconcat ["part 1: ", show . getMarker 4  $ input]
    putStrLn $ mconcat ["part 2: ", show . getMarker 14 $ input] 
