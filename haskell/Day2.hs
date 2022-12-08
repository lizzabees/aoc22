#!/usr/bin/env cabal
{- cabal:
build-depends: base
ghc-options: -main-is Main.main -O2
-}
module Main where
import System.Environment (getArgs)

class Scoring a where
    score :: a -> Int

data Move = Rock | Paper | Scissors
    deriving(Enum,Eq,Show)

instance Scoring Move where
    score = (+1) . fromEnum

data Winner = You | Cat | Me
    deriving(Enum,Eq,Show)

instance Scoring Winner where
    score = (*3) . fromEnum

type Game = (Move, Move)

-- first column is always their move
parseYou :: Char -> Move
parseYou 'A' = Rock
parseYou 'B' = Paper
parseYou 'C' = Scissors
parseYou  c  = error $ "invalid you move: " <> [c]

-- in part one, second column is our move
parseMe :: Char -> Move
parseMe 'X' = Rock
parseMe 'Y' = Paper
parseMe 'Z' = Scissors
parseMe  c  = error $ "invalid me move: " <> [c]

-- in part two, second column is desired outcome
parseHint :: Char -> Winner
parseHint 'X' = You
parseHint 'Y' = Cat
parseHint 'Z' = Me
parseHint  c  = error $ "invalid hint: " <> [c]

-- in part one we parse "$you $me" as our respective moves
parseGame :: String -> Game
parseGame [you,' ',me] = (parseYou you, parseMe me)
parseGame line         = error $ "invalid move line: " <> line

-- in part two we parse "$you $outcome"
parseCheat :: String -> (Move, Winner)
parseCheat [you,' ',out] = (parseYou you, parseHint out)
parseCheat line           = error $ "invalid cheat line: " <> line

-- there's probably a way i could code golf this
play :: Game -> Winner
play (Rock    ,    Paper) = Me
play (Rock    , Scissors) = You
play (Paper   ,     Rock) = You
play (Paper   , Scissors) = Me
play (Scissors,    Paper) = You
play (Scissors,     Rock) = Me
play _                    = Cat

-- given their move and desired outcome, what move do we make?
hint :: (Move, Winner) -> Move
hint (Rock    ,  Me) = Paper
hint (Rock    , You) = Scissors
hint (Paper   ,  Me) = Scissors
hint (Paper   , You) = Rock
hint (Scissors,  Me) = Rock
hint (Scissors, You) = Paper
hint (move    , Cat) = move

-- we score the game the same in both parts
totalScore :: Game -> Int
totalScore game = move game + winner game
    where move = score . snd
          winner = score . play

-- just a way to swap out the second part of a tuple
-- there is probably some category theory friendly way
-- but hoogle says nothing to me. Bifunctor maybe?
slick :: ((a, b) -> c) -> (a, b) -> (a, c)
slick fn tup@(x, _) = (x, fn tup)

part1 :: [String] -> Int
part1 = foldr ((+) . totalScore . parseGame) 0

part2 :: [String] -> Int
part2 = foldr ((+) . totalScore . slick hint . parseCheat) 0

main :: IO ()
main = do
    path <- head <$> getArgs
    input <- lines <$> readFile path
    putStrLn $ mconcat ["part 1: ", show . part1 $ input]
    putStrLn $ mconcat ["part 2: ", show . part2 $ input]
