#!/usr/bin/env cabal
{- cabal:
build-depends: base, pqueue
ghc-options: -O2
-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where
import Control.Monad (guard,void)
import Data.Char (ord)
import Data.Vector ((!))
import Lens.Micro
import Lens.Micro.TH
import System.Environment (getArgs)
import Text.Parsec hiding (parse)

import qualified Control.Monad.ST as ST
import qualified Data.PQueue.Min as PQ
import qualified Data.Vector as V

data HeightInput = Height Int
                 | Start
                 | Target
                 deriving(Eq,Ord,Show)

-- HEIGHTMAP 
data HeightMap = HeightMap 
    { _hmData   :: V.Vector Int
    , _hmWidth  :: Int
    , _hmHeight :: Int
    } deriving(Eq,Ord,Show)

makeLenses ''HeightMap

type Point = (Int,Int)

-- these are all partial for invalid inputs. whocare
idxAt :: HeightMap -> Point -> Int
idxAt HeightMap{_hmWidth} (x,y) = y * _hmWidth + x

valIdx :: HeightMap -> Int -> Int
valIdx HeightMap{_hmData} = (_hmData !)

valAt :: HeightMap -> Point -> Int
valAt m = valIdx m . idxAt m

canMove :: Int -> Int -> Bool
canMove p p' = p' <= (p + 1)

-- manhattan distance
mdist :: Point -> Point -> Int
mdist (px,py) (dx,dy) = abs (dx-px) + abs (dy-py)

-- PATHFINDING
data PathFind = PathFind
    { _pfHeights :: HeightMap
    , _pfStart   :: Point
    , _pfEnd     :: Point
    } deriving(Eq,Ord,Show)

makeLenses ''PathFind

-- returns a list of possible moves with both the
-- distance from the point to the destination and
-- the point coords themselves
posMoves :: HeightMap -> Point -> [Point]
posMoves m@HeightMap{..} p@(x,y) =
    let pIdx = idxAt  m p
        pVal = valIdx m pIdx
        nbrs = [(x,y-1), (x+1,y), (x,y+1), (x-1,y)]
     in do
         n@(nx,ny) <- nbrs
         guard $  nx >= 0 && nx < _hmWidth && ny >= 0 && ny < _hmHeight
         let nVal = valAt m n
         let vdel = nVal-pVal
         guard $ vdel <= 1
         return n

-- heuristic function
type Hfn = HeightMap -> Point -> Int

astar :: HeightMap -> Hfn -> Maybe [Point]
astar = undefined
            
-- PARSING
data ParseState = ParseState 
    { _psX      :: Int
    , _psY      :: Int
    , _psWidth  :: Int
    , _psHeight :: Int
    , _psStart  :: Point
    , _psEnd    :: Point
    } deriving (Eq,Ord,Show)
makeLenses ''ParseState

initState :: ParseState
initState = ParseState 0 0 0 0 (0,0) (0,0)

type Parser a = Parsec String ParseState a

parse :: Parser a -> String -> String -> a
parse p n = either (error . show) id . runParser p initState n

parseFile :: String -> Parser a -> IO a
parseFile n p = parse p n <$> readFile n

heightMap :: Parser PathFind
heightMap = do
    heights' <- V.fromList .  concat <$> heights
    ParseState{..} <- getState
    pure $ PathFind (HeightMap heights' _psWidth _psHeight) _psStart _psEnd
        where heights :: Parser [[Int]]
              heights = manyTill line eof'
              line    :: Parser [Int]
              line    = many1 item <* newline' 
              item    :: Parser Int
              item    = choice [height, start, end]
              height  :: Parser Int
              height = do
                  c <- oneOf ['a'..'z']
                  let a = ord 'a'
                  let h = ord c - a
                  modifyState $ psX +~ 1
                  pure h
              start :: Parser Int
              start = do
                  void $ char 'S'
                  modifyState $ \s -> s
                    & psStart .~ (s ^. psX, s ^. psY)
                    & psX +~ 1
                  pure 0
              end :: Parser Int
              end = do
                  void $ char 'E'
                  modifyState $ \s -> s
                    & psEnd .~ (s ^. psX, s ^. psY)
                    & psX +~ 1
                  pure 25
              newline' :: Parser ()
              newline' = do
                  void newline
                  modifyState $ \s -> s
                    & psWidth .~ s ^. psX
                    & psX .~ 0
                    & psY +~ 1
                  return ()
              eof' :: Parser ()
              eof' = do
                  eof
                  modifyState $ \s -> s
                    & psHeight .~ s ^. psY

main :: IO ()
main = do
    path <- head <$> getArgs
    input <- parseFile path heightMap
    print input
