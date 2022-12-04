#!/usr/bin/env cabal
{- cabal:
build-depends: base, parsec
ghc-options: -main-is Day4.main
-}
{-# LANGUAGE LambdaCase #-}
module Day4 where
import Data.Char (isDigit)
import Data.Either (either)
import Text.Parsec hiding (try)
import Text.Parsec.Error
import System.Environment (getArgs)

type Parser = Parsec String ()

int :: Parser Int
int = read <$> many1 digit

type Range = (Int,Int)

range :: Parser Range
range = (,) <$> int <* char '-' <*> int

rangePair :: Parser (Range, Range)
rangePair = (,) <$> range <* char ',' <*> range
 
contains :: Range -> Range -> Bool
contains (ll, lh) (rl, rh) = ll <= rl && lh >= rh

eitherContains :: Range -> Range -> Bool
eitherContains l r = contains l r || contains r l 

err :: Show a => Either a b -> b
err = either (error . show) id

part1 :: [String] -> Int
part1 = length . filter (uncurry eitherContains) . map (err . parse rangePair "")

overlaps :: Range -> Range -> Bool
overlaps (ll, lh) (rl, rh) =
  (ll >= rl && ll <= rh) ||
  (lh >= rl && lh <= rh) ||
  (rl >= ll && rl <= lh) ||
  (rh >= ll && rh <= lh)
 

part2 :: [String] -> Int
part2 = length . filter (uncurry overlaps) . map (err . parse rangePair "")

main :: IO ()
main = do
  path <- head <$> getArgs
  input <- lines <$> readFile path
  putStrLn $ mconcat ["part 1: ", show . part1 $ input]
  putStrLn $ mconcat ["part 2: ", show . part2 $ input]