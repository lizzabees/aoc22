#!/usr/bin/env cabal
{- cabal:
build-depends: base, parsec
ghc-options: -O2
-}
module Main where

import Prelude hiding ((<|>))
import Text.Parsec hiding (parse)

data Packet = List [Packet] | Int Int
  deriving (Eq,Show)

instance Ord Packet where
  compare :: Packet -> Packet -> Ordering
  compare (Int l) (Int r)             = compare l r
  compare (Int  l) (List r)           = compare (List [Int l]) (List       r)
  compare (List l) (Int  r)           = compare (List       l) (List [Int r])
  compare (List     []) (List     []) = EQ
  compare (List     []) (List  (_:_)) = LT
  compare (List  (_:_)) (List     []) = GT
  compare (List (l:ls)) (List (r:rs)) = case compare l r of
    LT -> LT
    GT -> GT
    EQ -> compare (List ls) (List rs)

ordered :: Packet -> Packet -> Bool
ordered l r = case compare l r of
  LT -> True
  GT -> False
  EQ -> error "invalid input"

type Parser a = Parsec String () a

int :: Parser Int
int = read <$> many1 digit

packet :: Parser Packet
packet =  Int <$> int
      <|> List <$> between (char '[') (char ']') (sepBy packet (char ','))
    
pair :: Parser (Packet,Packet)
pair = do
  left  <- packet
  right <- newline *> packet <* newline
  return (left, right)

pairs :: Parser [(Packet,Packet)]
pairs = sepEndBy (pair) newline

packets :: Parser [Packet]
packets = sepEndBy (skipMany newline *> packet) newline

parse :: Parser a -> String -> String -> a
parse p n = either (error . show) id . runParser p () n

part1 :: [(Packet,Packet)] -> Int
part1 = foldr iter 0 . zip ([1..]) . map (uncurry ordered)
  where
    iter (i, True) count = count + i
    iter (_,    _) count = count

part2 :: [Packet] -> Int
part2 = uncurry (*) . indices . sort . (marker1:) . (marker2:)
  where
    marker1    = List [List [Int 2]]
    marker2    = List [List [Int 6]]
    index1     = maybe undefined (+1) . elemIndex marker1
    index2     = maybe undefined (+1) . elemIndex marker2
    indices ps = (index1 ps, index2 ps)

main :: IO ()
main = do
    path  <- head <$> getArgs
    input <- readFile path
    putStrLn $ "part 1: " <> (show . part1 $ parse pairs path input)
    putStrLn $ "part 2: " <> (show . part2 $ parse packets path input)
    -- putStrLn $ mconcat ["part1: ", show $ part1 input]
    -- putStrLn $ mconcat ["part2: ", show $ part2 input]

