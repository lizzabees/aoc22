#!/usr/bin/env cabal
{- cabal:
build-depends: base, containers, mtl parsec
ghc-options: -O2
-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import Text.Parsec hiding (State,parse)
import qualified Data.Set as S

-- CORE LOGIC
data Dir = U | R | D | L
    deriving (Eq,Ord,Show)

type Point = (Int,Int)
type Delta = Point

delta :: Point -> Point -> Delta
delta (hx,hy) (tx,ty) = (hx-tx, hy-ty)

-- ngl i cribbed this one. i didn't know about `on`
dist :: Delta -> Int
dist (x,y) = (max `on` abs) x y

-- move the head given a dir
move :: Dir -> Point -> Point
move U (x,y) = (  x,y-1)
move R (x,y) = (x+1,y  )
move D (x,y) = (x  ,y+1)
move L (x,y) = (x-1,y  )

-- where does tail move given the head position
follow :: Point -> Point -> Point
follow h t@(tx,ty)
  = let d@(dx,dy) = delta h t
     in if dist d <= 1 then t else (tx + signum dx, ty + signum dy)

-- STATE - PART1
data Rope1 = Rope1
    { r1Moved :: S.Set Point -- coords visited by tail
    , r1Head :: Point        -- head of the rope
    , r1Tail :: Point        -- tail of the rope
    } deriving(Eq,Ord,Show)

-- in part 1 we only tack a rope with a head and a tail
exec1 :: [Dir] -> Rope1
exec1 = flip execState start1 . go
    where start1 = Rope1
            (S.singleton (0,0)) -- visited
            (0,0)               -- head 
            (0,0)               -- tail
          go :: [Dir] -> State Rope1 ()
          go [    ] = return ()
          go (m:ms) = do
              Rope1{..} <- get
              let h = move m r1Head
              let t = follow h r1Tail
              let s = if t == r1Tail
                      then r1Moved
                      else S.insert t r1Moved
              put $ Rope1 s h t
              go ms

-- STATE - PART2
data Rope2 = Rope2
    { r2Moved :: S.Set Point    -- coords visited by the tail
    , r2Head  :: Point
    , r2Knots :: [Point]        -- now we have to track 9 extra ropes
    } deriving (Eq,Ord,Show)

-- in part 2 we track multiple knots in the rope, but
-- we still only care what sites the tail has visted
-- we can't just foldl1 because we still have to persist
-- the entire changed state. also there is some wasted
-- effort here. 2 tears in a bucket ...
exec2 :: [Dir] -> Rope2
exec2 = flip execState start2 . go 
    where start2 = Rope2
            (S.singleton (0,0)) -- visited
            (0,0)               -- head
            (replicate 9 (0,0)) -- tail
          go :: [Dir] -> State Rope2 ()
          go [    ] = return ()
          go (m:ms) = do
              Rope2{..} <- get
              let h      = move m r2Head
              let (_:xs) = scanl' follow h r2Knots
              let t      = last xs
              let s      = S.insert t r2Moved
              put $ Rope2 s h xs
              go ms
--
-- PARSING
type Parser a = Parsec String [Dir] a

-- run a parser. bail on error
parse :: Parser a -> String -> String -> a
parse p n = either (error . show) id . runParser p [] n

parseFile :: String -> Parser a -> IO a
parseFile n p = parse p n <$> readFile n

moves :: Parser [Dir]
moves = manyTill line eof >> fmap reverse getState 
    where line = move <* newline
          move = do
              d <- dir
              void $ char ' '
              c <- read <$> many1 digit
              let ds = replicate c d
              modifyState (ds ++)
          dir = choice [ char 'U' $> U
                       , char 'R' $> R
                       , char 'D' $> D
                       , char 'L' $> L
                       ]

-- MAIN
part1 :: [Dir] -> Int
part1 = S.size . r1Moved . exec1

part2 :: [Dir] -> Int
part2 = S.size . r2Moved . exec2

main :: IO ()
main = do
    path  <- head <$> getArgs
    input <- parseFile path moves
    putStrLn $ mconcat ["part 1: ", show $ part1 input]
    putStrLn $ mconcat ["part 2: ", show $ part2 input]
