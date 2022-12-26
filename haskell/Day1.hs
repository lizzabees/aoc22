#!/usr/bin/env cabal
{- cabal:
build-depends: base
             , split
ghc-options: -O2
-}
module Main where
import Data.List.Split (splitOn)

elves :: String -> [[Int]]
elves = map (map read) . splitOn [""] . lines

part1 :: [[Int]] -> Int
part1 = foldr (max . sum) 0

part2 :: [[Int]] -> Int
part2 = sum . take 3 . sortOn Down . map sum

main :: IO ()
main = do
    path <- head <$> getArgs
    input <- elves <$> readFile path
    putStrLn $ mconcat ["part 1: ", show . part1 $ input]
    putStrLn $ mconcat ["part 2: ", show . part2 $ input]
