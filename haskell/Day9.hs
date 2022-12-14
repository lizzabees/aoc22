#!/usr/bin/env cabal
{- cabal:
build-depends: base, containers, mtl parsec
ghc-options: -O2
-}
{-# LANGUAGE RecordWildCards #-}
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

-- STATE
data Rope = Rope
    { rMoved :: S.Set Point
    , rHead :: Point
    , rTail :: Point
    } deriving(Eq,Ord,Show)

exec :: [Dir] -> Rope
exec = flip execState (Rope S.empty (0,0) (0,0)) . go
    where go :: [Dir] -> State Rope ()
          go []     = return ()
          go (m:ms) = do
              Rope{..} <- get
              let h = move m rHead
              let t = follow h rTail
              let s = if t == rTail
                      then rMoved
                      else S.insert t rMoved
              put $ Rope s h t
              go ms

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

main :: IO ()
main = do
    path  <- head <$> getArgs
    input <- parseFile path moves
    putStrLn $ mconcat ["part 1: ", show . S.size . rMoved $ exec input]
