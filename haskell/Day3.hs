#!/usr/bin/env cabal
{- cabal:
build-depends: base, split
ghc-options: -O2
-}
{-# LANGUAGE LambdaCase #-}
module Main where
import Data.Char (isAsciiUpper,isAsciiLower,ord)
import Data.List (intersect)
import Data.List.Split (chunksOf)
import System.Environment (getArgs)

-- why oh why is this the flip of the ascii order
-- i am so not doing bit math anymore
priority :: Char -> Int
priority = \case
    c | isAsciiUpper c -> ord c - ord 'A'
    c | isAsciiLower c -> 1 + ord c - ord 'a'

-- split a list in half, even on
-- odd lists because trust issues 😌
split :: [a] -> ([a],[a])
split xs = go xs xs []
    where go :: [a] -> [a] -> [a] -> ([a], [a])
          go (x:xs) (  _:[]) acc = (reverse $ x:acc, xs)
          go (x:xs) (_:_:[]) acc = (reverse $ x:acc, xs)
          go (x:xs) (_:_:ys) acc = go xs ys $ x:acc

part1 :: [String] -> Int
part1 = sum . map (priority . head . uncurry intersect . split)

part2 :: [String] -> Int
part2 = sum . map (priority . head . foldr1 intersect) . chunksOf 3

main :: IO ()
main = do
    path <- head <$> getArgs
    input <- lines <$> readFile path
    putStrLn $ mconcat ["part 1: ", show . part1 $ input]
    putStrLn $ mconcat ["part 2: ", show . part2 $ input]
