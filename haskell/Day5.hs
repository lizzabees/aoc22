#!/usr/bin/env cabal
{- cabal:
build-depends: base, parsec, vector
ghc-options: -main-is Day5.main
-}
{-# LANGUAGE LambdaCase #-}
module Day5 where

import Control.Monad (forM_)
import Data.List (transpose)
import Data.Maybe (isJust)
import System.Environment (getArgs)
import Text.Parsec
import Text.Parsec.Char

import qualified Control.Monad.ST as ST
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

type Parser a = Parsec String () a

crate :: Parser Char
crate = between (char '[') (char ']') upper

maybeCrate :: Parser (Maybe Char)
maybeCrate = string "   " *> pure Nothing <|> Just <$> crate

crates :: Parser [Maybe Char]
crates = sepBy1 maybeCrate (char ' ')
        
parseErr :: Parser a -> String -> a
parseErr p = either (error . show)  id . parse p ""

-- Move (count) (from) (to)
data Move = Move Int Int Int
  deriving (Eq,Ord,Show)

parseMove :: Parser Move
parseMove = do
  string "move "
  count <- read <$> many1 digit
  string " from "
  from <-  read <$> many1 digit
  string " to "
  to <- read <$> many1 digit
  return $ Move count (from-1) (to-1)
  
parseInput :: Parser ([String], [Move])
parseInput = do
  init <- fmap (map notEmpty . transpose) . count 8 $ crates <* newline
  count 2 $ manyTill anyChar newline
  moves <- endBy1 parseMove newline
  eof
  return (init, moves)
    where notEmpty :: [Maybe a] -> [a]
          notEmpty = foldr go []
          go :: Maybe a -> [a] -> [a]
          go (Just x) xs = x:xs
          go _        xs = xs

type Mover a = Int-> [a] -> [a] -> ([a],[a])

-- diff moving rules for part1 and 2
move1 :: Mover a
move1 count from to =
  let (moved, from') = splitAt count from
   in (from', reverse moved ++ to)

move2 :: Mover a
move2 count from to =
    let (moved, from') = splitAt count from
     in (from', moved ++ to)

runMoves :: Mover Char -> [String] -> [Move] -> [String]
runMoves mover init moves = ST.runST $ do
  stacks <- V.thaw . V.fromList $ init
  forM_ moves $ \(Move count from to) -> do
      from' <- MV.read stacks from
      to'   <- MV.read stacks to
      let (from'', to'') = mover count from' to'
      MV.write stacks from from''
      MV.write stacks to to''
  V.toList <$> V.freeze stacks
    
runMover :: Mover Char -> String -> String
runMover m = map head . uncurry (runMoves m) . parseErr parseInput

main :: IO ()
main = do
  path <- head <$> getArgs
  input <- readFile path
  putStrLn $ mconcat ["part 1: ", runMover move1 $ input]
  putStrLn $ mconcat ["part 2: ", runMover move2 $ input]
