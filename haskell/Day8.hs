#!/usr/bin/env cabal
{- cabal:
build-depends: base, vector
ghc-options: -O2
-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import Prelude hiding (left,right)
import Data.Vector ((!))
import qualified Data.Vector as V

data Input = Input
    { width  :: Int
    , height :: Int
    , treehs :: V.Vector Int
    } deriving(Eq,Ord,Show)

type Point = (Int,Int)

-- unsafe, who care
getAt :: Input -> Point -> Int
getAt Input{..} (x,y) = treehs ! (y * width + x)

type NeighborFn = Input -> Point -> [Int]

left :: Input -> Point -> [Int]
left i (x,y) = [getAt i (x',y) | x' <- [x-1,x-2..0]]

right :: Input -> Point -> [Int]
right i@Input{width} (x,y) = [getAt i (x',y) | x' <- [x+1..width-1]]

above :: Input -> Point -> [Int]
above i (x,y) = [getAt i (x,y') | y' <- [y-1,y-2..0]]

below :: Input -> Point -> [Int]
below i@Input{height} (x,y) = [getAt i (x,y') | y' <- [y+1..height-1]]

visibleFrom :: Input -> Point -> NeighborFn -> Bool
visibleFrom i p f = isNothing . find (>= me) $ f i p 
    where me = getAt i p

-- optimize a bit by checking nearest edges first
neighborFns :: Input -> Point -> [NeighborFn]
neighborFns Input{..} (x,y) = map snd . sortOn fst $ neighbors
    where neighbors = [ (x         ,  left)
                      , (width  - x, right)
                      , (y         , above)
                      , (height - y, below)
                      ]

visible :: Input -> Point -> Bool
visible _             (0,_)                   = True
visible _             (_,0)                   = True
visible Input{width}  (x,_) | x == width  - 1 = True
visible Input{height} (_,y) | y == height - 1 = True
visible i p = isJust . find check $ nfns
    where nfns = neighborFns i p
          check = visibleFrom i p

-- takeWhile but we include the first match as well
takeUntil :: forall a. (a -> Bool) -> [a] -> [a]
takeUntil p = flip go []
    where go :: [a] -> [a] -> [a]
          go []     acc       = reverse acc
          go (x: _) acc | p x = reverse $ x:acc
          go (x:xs) acc       = go xs (x:acc)

scores :: Input -> Point -> [NeighborFn] -> Int
scores i p = product . map dist
    where dist f = length . takeUntil (>= me) $ f i p
          me     = getAt i p

score :: Input -> Point -> Int
score i p@(0,0) = scores i p [right, below       ]
score i p@(0,_) = scores i p [right, above, below]
score i p@(_,0) = scores i p [left , right, below]
score i@Input{..} p@(x,y) | x == width-1 && y == height-1 = scores i p [left, above]
score i@Input{..} p@(x,_) | x == width-1                  = scores i p [left, above, below]
score i@Input{..} p@(_,y) |                 y == height-1 = scores i p [left, right, above]
score i p = scores i p [left, right, above, below]

parse :: String -> Input
parse s =
    let s'     = lines s
        width  = length . head $ s'
        height = length s'
        digits = map ((- ord '0' +) . ord) . mconcat $ s'
     in Input width height $ V.fromList digits

part1 :: Input -> Int
part1 i@Input{..} = length $ do
    y <- [0..height-1]
    x <- [0.. width-1]
    guard $ visible i (x,y)
    return ()

part2 :: Input -> Int
part2 i@Input{..} = maximum $ do
    y <- [0..height-1]
    x <- [0.. width-1]
    return $ score i (x,y) 

main :: IO ()
main = do
    path <- head <$> getArgs
    input <- parse <$> readFile path
    putStrLn $ mconcat ["part 1: ", show $ part1 input]
    putStrLn $ mconcat ["part 2: ", show $ part2 input]
    return ()

