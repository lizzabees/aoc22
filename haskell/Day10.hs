#!/usr/bin/env cabal
{- cabal:
build-depends: base, mtl, optics, parsec
ghc-options: -O2
-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where
import Control.Lens
import Control.Lens.TH
import Data.Functor ((<&>), ($>))
import System.Environment (getArgs)
import Text.Parsec hiding (State,parse)

-- CPU stuff
data Inst = Noop
          | Addx Int
          deriving (Eq,Ord,Show)


cpuc :: Inst -> Int
cpuc (Noop  ) = 1
cpuc (Addx _) = 2

data Cpu = Cpu
    { _regX :: Int
    , _pipe :: [Inst]
    } deriving(Eq,Ord,Show)

$(makeLenses ''Cpu)

-- step one instruction thru the cpu
step :: Inst -> Cpu -> Cpu
step (Noop  ) = id
step (Addx n) = regX %~ (+n) 

-- PARSING
type Parser a = Parsec String () a

parse :: Parser a -> String -> String -> a
parse p n = either (error . show) id . runParser p () n

parseFile :: String -> Parser a -> IO a
parseFile n p = parse p n <$> readFile n

-- 🍺
pint :: Parser Int
pint = do
    sig <- optionMaybe $ char '-'
    num <- many1 digit
    case sig of
      Just  s -> pure $ read (s:num)
      Nothing -> pure $ read    num

inst :: Parser Inst
inst = noop <|> addx
    where noop = string "noop"  $> Noop
          addx = string "addx " *> pint <&> Addx

prog :: Parser [Inst]
prog = manyTill (inst <* newline) eof

-- 
part1 :: Int
part1 = undefined


main :: IO ()
main = do
    path  <- head <$> getArgs
    input <- parseFile path prog
    putStrLn $ mconcat ["part 1: ", show input]
