#!/usr/bin/env cabal
{- cabal:
build-depends: base, containers, mtl parsec
ghc-options: -O2
-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import Control.Monad.State
import Data.Functor (($>))
import System.Environment (getArgs)
import Text.Parsec hiding (State,parse)
import qualified Data.Set as S

-- CORE LOGIC
data Dir = U | R | D | L
    deriving (Eq,Ord,Show)

type Point = (Int,Int)
type Delta = Point

delta :: Point -> Point -> Delta
delta (hx,hy) (tx,ty) = (hx-tx, hy-ty)

touching :: Delta -> Bool
touching (dx,dy) = abs dx <= 1 && abs dy <= 1

sameRow :: Delta -> Bool
sameRow (_,0) = True
sameRow _     = False

sameCol :: Delta -> Bool
sameCol (0,_) = True
sameCol _    = False

-- move the head given a dir
move :: Dir -> Point -> Point
move U (x,y) = (  x,y-1)
move R (x,y) = (x+1,y  )
move D (x,y) = (x  ,y+1)
move L (x,y) = (x-1,y  )

-- where does tail move given the head position
follow :: Point -> Point -> Point
follow h t@(tx,ty) = go $ delta h t
    where go :: Delta -> Point
          go d         | touching d = t
          go d@(dx, _) | sameRow d && dx < 0 = (tx-1,ty)
          go d@(dx, _) | sameRow d && dx > 0 = (tx+1,ty)
          go d@(_ ,dy) | sameCol d && dy < 0 = (tx,ty-1)
          go d@(_ ,dy) | sameCol d && dy < 0 = (tx,ty+1)
          go   (dx,dy) | abs dx > abs dy && dx < 0 = (tx-1,ty+dy)
          go   (dx,dy) | abs dx > abs dy && dx > 0 = (tx+1,ty+dy)
          go   (dx,dy) | dy < 0 = (tx+dx,ty-1)
          go   (dx,dy) | dy > 0 = (tx+dx,ty+1)
          go _                  = error "move: yikes!"

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
            (S.singleton (0,0))
            (0,0)
            (0,0)
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
    , r2Head  :: Point          -- still track the head separate, for reasons
    , r2Knots :: [Point]        -- now we have to track 9 extra ropes
    , r2Tail  :: Point          -- we keep the tail seperate too for Reasons(TM)
    } deriving (Eq,Ord,Show)

-- in part 2 we track multiple knots in the rope, but
-- we still only care what sites the tail has visted
-- we can't just foldl1 because we still have to persist
-- the entire changed state
exec2 :: [Dir] -> Rope2
exec2 = flip execState start2 . go
    where start2 = Rope2
            (S.singleton (0,0))
            (0,0)
            (replicate 8 (0,0))
            (0,0)
          go :: [Dir] -> State Rope2 ()
          go [    ] = return ()
          go (x:xs) = do
              Rope2{..} <- get
              let h        = move x r2Head
              let (ks, t') = weird follow h r2Tail r2Knots
              let m        = if t' == r2Tail
                                then r2Moved
                                else S.insert t' r2Moved
              put $ Rope2 m h ks t'
              go xs

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

-- UTILITIES

-- it's not a fold and it's not a scan, it's some cursed
-- thing that combinatronics has probably happily ignored
-- takes the scan fun, head, tail, list, returns the
-- tansformed list and transformed tail. why? reasons.
weird :: forall a b. (b -> a -> b) -> b -> a -> [a] -> ([b], b)
weird f h t xs = go h xs []
    where go :: b -> [a] -> [b] -> ([b], b)
          go _ [    ] _   = error "you done goofed"
          go b (x:[]) acc =
              let x'   = f b x
                  acc' = x':acc
                  t'   = f x' t
               in (reverse acc', t')
          go b (x:xs) acc =
              let x' = f b x
               in go x' xs (x':acc)
                
-- MAIN
test1 :: String
test1 = "R 4\n\
        \U 4\n\
        \L 3\n\
        \D 1\n\
        \R 4\n\
        \D 1\n\
        \L 5\n\
        \R 2\n"

test2 :: String
test2 = "R 5\n\
        \U 8\n\
        \L 8\n\
        \D 3\n\
        \R 17\n\
        \D 10\n\
        \L 25\n\
        \U 20\n"

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
