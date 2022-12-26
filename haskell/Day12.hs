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
import Data.Vector ((!))
import Lens.Micro.Platform
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

-- this is partial for invalid inputs. whocare
valAt :: HeightMap -> Point -> Int
valAt HeightMap{_hmData,_hmWidth} (x,y) = _hmData ! y * _hmWidth + x

canMove :: Int -> Int -> Bool
canMove p p' = p' <= (p + 1)

-- PATHFINDING
data PathFind = PathFind
    { _pfHeights :: HeightMap
    , _pfStart   :: Point
    , _pfEnd     :: Point
    } deriving(Eq,Ord,Show)
makeLenses ''PathFind

data Node = Node
    { _noPos  :: Point
    , _noPath :: [Point] -- path back to origin
    , _noCost :: Int     -- cost so far 
    , _noDist :: Int     -- heuristic to dest
    } deriving (Eq,Show)
makeLenses ''Node

instance Ord Node where
    compare :: Node -> Node -> Ordering
    compare = compare `on` _noCost

-- returns a list of possible next moves for a
-- given point, filtering out mkoves that are off
-- the map or un-passable
neighbors :: PathFind -> Point -> [Point]
neighbors pf p =
    let pval = valAt (pf^.pfHeights) (pf^.pfStart)
        arnd = [((+x),(+y)) | y <- [-1,1], x <- [-1,1]]
        nbrs = zipWith (uncurry bimap) arnd $ repeat p
        width  = pf^.pfHeights.hmWidth
        height = pf^.pfHeights.hmHeight
     in do
         n@(nx,ny) <- nbrs
         guard $ nx >= 0 && nx < width && ny >= 0 && ny < height
         let nval = valAt (pf^.pfHeights) n
         let vdel = nval-pval
         guard $ vdel <= 1
         return n

type CostFn = PathFind -> Point -> Point -> Int

type Front = PQ.MinQueue Node
type Visit = M.Map Point Node

-- a* state
data AState = AState
    { _asFront :: Front
    , _asVisit :: Visit
    } deriving(Show)
makeLenses ''AState

-- manhattan distance
mdist :: CostFn
mdist _ (px,py) (dx,dy) = abs (dx-px) + abs (dy-py)

-- in a* terms, we take the map, g(f) which is a function
-- calculating cost to move from one point to another and
-- h(f) which is a heuristic function guessing cost between
-- two points
astar :: PathFind -> CostFn -> CostFn -> Maybe [Point]
astar pf@PathFind{..} gf hf =
    let dist  = hf pf _pfStart _pfEnd
        node  = Node _pfStart [] 0 dist
        front = PQ.singleton node
        visit = M.singleton _pfStart node
        state = AState front visit
     in step state $ Just node
    where step :: AState -> Maybe Node -> Maybe [Point]
          step _     Nothing     = Nothing -- we didn't find target
          step _     (Just curr) | (curr^.noPos) == (pf^.pfEnd) = Just $ path curr
          step state (Just curr) =
              let nbrs   = neighbors pf (curr^.noPos)
                  ncosts = flip map nbrs $ gf pf (curr^.noPos)
                  ndists = flip map nbrs $ flip (hf pf) (pf^.pfEnd)
                  npaths = repeat $ path curr
                  nbrs'  = Node <$> nbrs <*> npaths <*> ncosts <*> ndists
                  state' = flip execState state $ do
                      modify . over asFront $ PQ.deleteMin
                      forM_ nbrs' $ \nbr -> undefined
               in step state' $ PQ.getMin (state'^.asFront)
          path :: Node -> [Point]
          path Node{_noPos,_noPath} = _noPos:_noPath

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
