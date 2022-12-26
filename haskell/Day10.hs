#!/usr/bin/env cabal
{- cabal:
build-depends: base, lens, mtl, parsec, vector
ghc-options: -O2
-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
module Main where
import Data.Vector ((!))
import Text.Parsec hiding (State,parse,(<|>))

import qualified Control.Monad.ST as ST
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector as V

-- CPU
data Insn = Noop
          | Addx Int
          deriving(Eq,Ord,Show)

cycles :: Insn -> Int
cycles  Noop    = 1
cycles (Addx _) = 2

data Cpu = Cpu
    { _cpuX     :: Int
    , _cpuCycle :: Int
    , _cpuIsns  :: [(Int, Insn)]
    } deriving(Eq,Ord,Show)

cpu :: [Insn] -> Cpu
cpu isns = Cpu 1 0 $ zip (map cycles isns) isns

-- exec one insn
exec :: Cpu -> Insn -> Cpu
exec c             Noop    = c
exec s@Cpu{_cpuX} (Addx n) = s { _cpuX = _cpuX + n }

-- step through one cpu cycle
step :: Cpu -> Cpu
step c@Cpu{..} =
    let cycle' = _cpuCycle + 1
     in case _cpuIsns of
          [        ] -> error "step on empty insn list"
          ((1,i):is) -> (exec c i) {_cpuCycle = cycle', _cpuIsns = is}
          ((n,i):is) ->  c         {_cpuCycle = cycle', _cpuIsns = (n-1,i):is}

-- take n steps through the code
stepN :: Cpu -> Int -> Cpu
stepN c 1 = c
stepN c n = stepN (step c) (n-1)

-- get signal strength
signal :: Cpu -> Int
signal Cpu{..} = (_cpuCycle + 1) * _cpuX

-- sample signal strength *on* the listed cycles
sample :: Cpu -> [Int] -> [Int]
sample c = fst . flip runState c . flip go []
    where go :: [Int] -> [Int] -> State Cpu [Int]
          go [    ] acc = return . reverse $ acc
          go (x:xs) acc = do
              Cpu{..} <- get
              let n  = x - _cpuCycle
              let c' = stepN c n
              let s  = signal c'
              put c' 
              go xs (s:acc)

-- for a given crt x is the sprit visible
contains :: Cpu -> Int -> Bool
contains Cpu{_cpuX} x = abs (_cpuX - x) <= 1

runCrt :: Cpu -> V.Vector Bool
runCrt c = ST.runST $ do
    pixels <- MV.replicate (40*6) False
    let loop x y c@Cpu{..} =
            if null _cpuIsns
               then V.freeze pixels
               else do
                   when (c `contains` x) $ MV.write pixels (y*40+x) True
                   let c' = step c
                   let x' = (x + 1) `mod` 40
                   let y' = if x' == 0 then y+1 else y
                   loop x' y' c'
    loop 0 0 c 

printCrt :: V.Vector Bool -> IO ()
printCrt v = go 0
    where sz   = V.length v
          go i = do
            let i' = i +1
            if v ! i then putChar '#' else putChar '.'
            when ((i' `mod` 40) == 0) $ putChar '\n'
            if i' == sz then return () else go i'

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

insn :: Parser Insn
insn = noop <|> addx
    where noop = string "noop"  $> Noop
          addx = string "addx " *> pint <&> Addx

prog :: Parser [Insn]
prog = manyTill (insn <* newline) eof

-- 
part1 :: [Insn] -> Int
part1 insns = sum $ sample (cpu insns) [20,60..220]

part2 :: [Insn] -> IO ()
part2 = printCrt . runCrt . cpu

main :: IO ()
main = do
    path  <- head <$> getArgs
    input <- parseFile path prog
    putStrLn $ mconcat ["part 1: ", show $ part1 input]
    putStrLn $ mconcat ["part 2: --------------------"]
    part2 input
