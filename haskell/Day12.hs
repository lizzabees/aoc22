#!/usr/bin/env cabal
{- cabal:
build-depends: base, pqueue
ghc-options: -O2
-}
{-# LANGUAGE InstanceSigs    #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where
import Control.Monad (guard,void)
import Data.Bifunctor (bimap)
import Data.Function (on)
import Data.Char (ord)
import Data.Vector ((!))
import Lens.Micro.Platform
import System.Environment (getArgs)
import Text.Parsec hiding (parse)

import qualified Data.Map.Strict as M
import qualified Data.PQueue.Min as PQ
import qualified Data.Vector     as V

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

-- PATHFINDING
data PathFind = PathFind
    { _pfHeights :: HeightMap
    , _pfStart   :: Point
    , _pfEnd     :: Point
    } deriving(Eq,Ord,Show)
makeLenses ''PathFind

-- returns a list of possible next moves for a
-- given point, filtering out mkoves that are off
-- the map or un-passable
posMoves :: HeightMap -> Point -> [Point]
posMoves m@HeightMap{..} p =
    let pIdx  = idxAt  m p
        pVal  = valIdx m pIdx
        arnd  = [((+x),(+y)) | y <- [-1,1], x <- [-1,1]]
        nbrs = zipWith (uncurry bimap) arnd $ repeat p
     in do
         n@(nx,ny) <- nbrs
         guard $  nx >= 0 && nx < _hmWidth && ny >= 0 && ny < _hmHeight
         let nVal = valAt m n
         let vdel = nVal-pVal
         guard $ vdel <= 1
         return n

type CostFn = HeightMap -> Point -> Point -> Int
type NextPQ = PQ.MinQueue (Int,  Point )
type VisitM = M.Map Point Node

-- manhattan distance
mdist :: CostFn
mdist _ (px,py) (dx,dy) = abs (dx-px) + abs (dy-py)

data Node = Node
    { _noPath :: [Point] -- path back to origin
    , _noCost :: Int     -- cost so far to here
    } deriving (Eq,Show)
makeLenses ''Node

instance Ord Node where
    compare :: Node -> Node -> Ordering
    compare = compare `on` _noCost

-- in a* terms, we take the map, g(f) which is a function
-- calculating cost to move from one point to another and
-- h(f) which is a heuristic function guessing cost between
-- two points
astar :: PathFind -> CostFn -> CostFn -> Maybe [Point]
astar PathFind{..} gf hf =
    let cost    = hf _pfHeights _pfStart _pfEnd
        next    = PQ.singleton (cost, _pfStart)
        visited = M.empty
     in go next visited
    where go = undefined

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
    heights' <- V.fromList . concat <$> heights
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
