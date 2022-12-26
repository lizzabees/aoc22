#!/usr/bin/env cabal
{- cabal:
build-depends: base, pqueue
ghc-options: -O2
-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where
import Prelude hiding (try)
import Data.Vector ((!))
import Lens.Micro.Platform
import Text.Parsec hiding (parse)

import qualified Data.Map.Strict as M
import qualified Data.PQueue.Prio.Min as PQ

-- HEIGHTMAPS
type Point = (Int,Int)

data HeightMap = HeightMap
    { _width  :: Int
    , _height :: Int
    , _hdata  :: Vector Int
    } deriving(Show)
makeClassy ''HeightMap

hmGet :: HeightMap -> Point -> Int
hmGet HeightMap{_width,_hdata} (x,y) = _hdata ! (y * _width + x)

data Input = Input
    { _start :: Point
    , _stop  :: Point
    , _hmap   :: HeightMap
    } deriving(Show)
makeClassy ''Input

instance HasHeightMap Input where
    heightMap = hmap

-- PATHFINDING
buildPath :: Point -> Map Point Point -> [Point]
buildPath p paths = go p []
    where go :: Point -> [Point] -> [Point]
          go p acc = case paths ^? (ix p) of
                       Nothing -> reverse acc
                       Just p' -> go p' (p:acc)

neighbors :: HeightMap -> Point -> [Point]
neighbors hm p =
    let dirs    = map (bimap (+) (+)) $ [(0,-1), (1,0), (0,1), (-1,0)]
        nbrs    = zipWith (uncurry bimap) dirs $ repeat p
        width'  = hm^.width
        height' = hm^.height
        currh   = hm `hmGet` p
     in do
         n@(nx,ny) <- nbrs
         guard $ nx >= 0 && nx < width'
         guard $ ny >= 0 && ny < height'
         let nbrh = hm `hmGet` n 
         guard $ nbrh - currh <= 1
         return n

data AState = AState
    { _front :: PQ.MinPQueue Int Point
    , _costs :: Map Point Int
    , _paths :: Map Point Point
    } deriving(Show)
makeLenses ''AState

data Node = Node
    { _pos  :: Point
    , _cost :: Int
    , _dist :: Int
    } deriving(Show)
makeLenses ''Node

type DistFn = Point -> Point -> Int

-- here thar be dragons
aStar :: Input -> DistFn -> DistFn -> Maybe [Point]
aStar input gf hf =
    let begin  = input^.start
        front' = PQ.singleton 0 begin
        costs' = M.singleton begin 0
        paths' = M.empty
        state  = AState front' costs' paths'
     in go state $ Just begin
    where go :: AState -> Maybe Point -> Maybe [Point]
          go _     Nothing  = Nothing
          go state (Just p) | p == (input^.stop) = Just $ buildPath p (state^.paths)
          go state (Just p) =
              let front' = PQ.deleteMin (state^.front)
                  nposes = neighbors (input^.hmap) p
                  ncosts = flip map nposes $ gf p
                  ndists = flip map nposes $ hf (input^.stop)
                  nbrs   = Node <$> nposes <*> ncosts <*> ndists
                  state' = step (state & front .~ front') p nbrs
                  _      = unsafePerformIO $ print state'
               in go state' . fmap snd $ PQ.getMin (state'^.front)
          step :: AState -> Point -> [Node] -> AState
          step state _    [    ] = state
          step state from (n:ns) =
              let ccost = state ^. costs ^?! ix from
                  ncost = ccost + gf from (n^.pos)
               in case state ^. costs ^? ix (n^.pos) of
                      Nothing                    -> step (addNew state from ncost n) from ns
                      Just ocost | ncost < ocost -> step (addNew state from ncost n) from ns
                      Just _                     -> step state from ns
          addNew :: AState -> Point -> Int -> Node -> AState
          addNew state from cost new =
              let npri = cost + hf (new^.pos) (input^.stop)
                  npos = new^.pos
               in state
                & costs %~ M.insert npos cost
                & front %~ PQ.insert npri (new^.pos)
                & paths %~ M.insert npos from

manhattan :: DistFn
manhattan (lx,ly) (rx,ry) = abs (lx - rx) + abs (ly - ry)

const2 :: a -> b -> c -> a
const2 a _ _ = a

-- PARSING
data PState = PState
    { _psStart  :: Point
    , _psStop   :: Point
    , _psX      :: Int
    , _psY      :: Int
    , _psWidth  :: Int
    , _psHeight :: Int
    } deriving(Show)
makeLenses ''PState

type Parser a = Parsec String PState a

parse :: Parser a -> String -> String -> a
parse p f = either (error . show) id . runParser p init' f
    where init' = PState (0,0) (0,0) 0 0 0 0

parseFile :: Parser a -> FilePath -> IO a
parseFile p f = parse p f <$> readFile f

pInput :: Parser Input
pInput = do
    hdata' <- fromList . concat <$> heights
    PState{..} <- getState
    pure $ Input _psStart _psStop (HeightMap _psWidth _psHeight hdata')
        where heights :: Parser [[Int]]
              heights = manyTill line eof'
              line :: Parser [Int]
              line = many1 item <* newline'
              item :: Parser Int
              item = choice [height, start, stop]
              height :: Parser Int
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
                    & psStart .~ (s^.psX, s^.psY)
                    & psX +~ 1
                  pure 0
              stop = do
                  void $ char 'E'
                  modifyState $ \s -> s
                    & psStop .~ (s^.psX,s^.psY)
                    & psX +~ 1
                  pure 25
              newline' :: Parser ()
              newline' = do
                  void newline
                  modifyState $ \s -> s
                    & psWidth .~ s^.psX
                    & psX .~ 0
                    & psY +~ 1
                  pure ()
              eof' :: Parser ()
              eof' = do
                  void eof
                  modifyState $ \s -> s
                    & psHeight .~ s^.psY


-- MAIN
part1 :: Input -> Int
part1 input = length . fromJust $ aStar input (const2 1) manhattan

part2 :: Input -> Int
part2 input =
    let coords   = [(x,y) | y <- [0..input^.height-1], x <- [0..input^.width-1]]
        cvals    = zip coords $ toList (input^.hmap^.hdata)
        starts   = map fst . filter ((==0) . snd) $ cvals
        aStar' p = aStar input{_start=p} (const2 1) manhattan
     in head . sort . map (length . fromJust) . filter isJust . map aStar' $ starts

main :: IO ()
main = do
    path  <- head <$> getArgs
    input <- parseFile pInput path
    putStrLn $ mconcat ["part1: ", show $ part1 input]
    putStrLn $ mconcat ["part2: ", show $ part2 input]
