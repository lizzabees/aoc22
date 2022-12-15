#!/usr/bin/env cabal
{- cabal:
build-depends: base, microlens
ghc-options: -O2
-}
{-# LANGUAGE TemplateHaskell #-}
module Main where
import Control.Monad (forM_,void)
import Data.Functor (($>))
import Lens.Micro
import Lens.Micro.TH (makeLenses)
import System.Environment (getArgs)
import Text.Parsec hiding (parse)

import qualified Data.Sequence as S
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M

-- Was going to write a whole ass expression lang
-- but:
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
    { _mItems :: S.Seq Int
    , _mOp    :: Op
    , _mTest  :: Int
    , _mTrue  :: Int
    , _mFalse :: Int
    , _mCount :: Int
    } deriving(Eq,Ord,Show)

makeLenses ''Monkey

-- evaluate the worry level for an item
eval :: Int -> Op -> Int
eval old (Op l op r) = 
    let l' = val l
        r' = val r
     in evalOp l' op r' `div` 3
    where evalOp :: Int -> BinOp -> Int -> Int
          evalOp l Add r = l + r
          evalOp l Mul r = l * r
          val :: Val -> Int
          val  Old    = old
          val (Lit i) = i

-- test if divisible by n
test :: Int -> Int -> Bool
test worry by = (worry `mod` by) == 0

round :: [Monkey] -> [Monkey]
round = undefined

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


main :: IO ()
main = do
    path  <- head <$> getArgs
    input <- parseFile path monkeys
    forM_ input $ putStrLn . show
