#!/usr/bin/env cabal
{- cabal:
build-depends: base, microlens
ghc-options: -O2
-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes      #-}
module Main where
import Prelude hiding (Op)
import Control.Monad.Primitive
import Data.Sequence ((|>))
import Text.Parsec hiding (parse, (<|>))

import qualified Control.Monad.ST as ST
import qualified Data.Sequence as S
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

-- Was going to write a whole ass expression lang but:
-- 1. they only assign to 'new'
-- 2. the only var they use is 'old'
-- 3. the only ops i see are +/*
-- 4. it's always a binop, never a bare exp
-- 5. Test is only ever divisible
data Op = Op Val BinOp Val
    deriving (Eq,Ord,Show)

data Val = Old | Lit Int
    deriving (Eq,Ord,Show)

data BinOp = Add | Mul
    deriving (Eq,Ord,Show)

-- MONKE 🙊
data Monkey = Monkey
    { mItems :: S.Seq Int
    , mOp    :: Op
    , mTest  :: Int
    , mTrue  :: Int
    , mFalse :: Int
    , mCount :: Int
    } deriving(Eq,Ord,Show)

-- evaluate the worry level for an item
-- accept a worry management fn
eval :: (Int -> Int) -> Int -> Op -> Int
eval f old (Op l op r) = 
    let l' = val l
        r' = val r
     in f $ evalOp l' op r'
    where evalOp :: Int -> BinOp -> Int -> Int
          evalOp l Add r = l + r
          evalOp l Mul r = l * r
          val :: Val -> Int
          val  Old    = old
          val (Lit i) = i

-- test if divisible by n
test :: Int -> Int -> Bool
test worry by = (worry `mod` by) == 0

addItem :: Int -> Monkey -> Monkey
addItem i m@Monkey{mItems} = m{mItems = mItems |> i}

-- hate to leak ST monad type signatures but wygd
-- having this all in one function was ugly as sin
throwItem :: PrimMonad m => V.MVector (PrimState m) Monkey
          -> Int -> Int -> m ()
throwItem ms to v = MV.modify ms (addItem v) to

-- we have to thread the new worry management
-- function thru the whole stack sigh. i could probably
-- do this with reader monad but i don't want to run a
-- monad stack. also it's still the same thing basically
runTurn :: PrimMonad m => (Int -> Int)
        -> V.MVector (PrimState m) Monkey
        -> Int -> Monkey -> m ()
runTurn f ms i m@Monkey{..} = do
    forM_ mItems $ \old -> do
        let new = eval f old mOp
        if test new mTest
           then throwItem ms mTrue  new
           else throwItem ms mFalse new
    let cnt = mCount + S.length mItems
    MV.write ms i $ m{mItems=S.empty, mCount=cnt}

-- run one round of monkey business
runRound :: (Int -> Int) -> [Monkey] -> [Monkey]
runRound f ms' = ST.runST $ do
    ms <- V.thaw . V.fromList $ ms'
    MV.iforM_ ms $ runTurn f ms
    V.toList <$> V.freeze ms

runRounds :: (Int -> Int) -> Int -> [Monkey] -> [Monkey]
runRounds _ 0 ms = ms
runRounds f n ms = runRounds f (n-1) $ runRound f ms

monkeyBiz :: [Monkey] -> Int
monkeyBiz = product . take 2 . sortOn Down . map mCount

-- PARSING
type Parser a = Parsec String () a

parse :: Parser a -> String -> String -> a
parse p n = either (error . show) id . runParser p () n

parseFile :: String -> Parser a -> IO a
parseFile n p = parse p n <$> readFile n

monkeys :: Parser [Monkey]
monkeys = manyTill (monkey <* skipMany newline) eof
    where monkey = void monke >> Monkey
            <$> fmap S.fromList items 
            <*> oper
            <*> test
            <*> true
            <*> false
            <*> pure 0
          num    = read <$> many1 digit
          comsep = flip sepBy (string ", ")
          monke  = string "Monkey " *> num <* char ':' <* newline
          items  = string "  Starting items: " *> comsep num <* newline
          oper   = string "  Operation: new = " *> binop <* newline
          binop  = Op <$> (val <* char ' ') <*> op <*> (char ' ' *> val)
          val    = (string "old" $> Old) <|> (Lit <$> num)
          op     = (char '+' $> Add) <|> (char '*' $> Mul)
          test   = string "  Test: divisible by " *> num <* newline
          true   = string "    If true: throw to monkey " *> num <* newline
          false  = string "    If false: throw to monkey " *> num

-- MAIN
part1 :: [Monkey] -> Int
part1 = monkeyBiz . runRounds (flip div 3) 20

part2 :: [Monkey] -> Int
part2 ms = let k = product . map mTest $ ms
            in monkeyBiz . runRounds (flip mod k) 10000 $ ms

main :: IO ()
main = do
    path  <- head <$> getArgs
    input <- parseFile path monkeys
    putStrLn $ mconcat ["part 1: ", show $ part1 input]
    putStrLn $ mconcat ["part 2: ", show $ part2 input]
