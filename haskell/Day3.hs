#!/usr/bin/env cabal
{- cabal:
build-depends: base, split
ghc-options: -main-is Day3.main
-}
{-# LANGUAGE LambdaCase #-}
module Day3 where
import Data.Char (ord)
import Data.List (intersect)
import Data.List.Split (chunksOf)
import System.Environment (getArgs)

-- why oh why is this the flip of the ascii order
-- i am so not doing bit math anymore
priority :: Char -> Int
priority = \case
    c | c >= 'A' && c <= 'Z' -> 27 + ord c - ord 'A'
    c | c >= 'a' && c <= 'z' -> 1  + ord c - ord 'a'

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
