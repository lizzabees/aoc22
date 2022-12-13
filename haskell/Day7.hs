#!/usr/bin/env cabal
{- cabal:
build-depends: base, parsec
ghc-options: -O2
-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
module Main where
import Prelude hiding (lookup,unzip)
import Control.Monad.Writer
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.List (foldl')
import System.Environment (getArgs)
import Text.Parsec hiding (parse)

-- VFS STUFF
-- we use a phantom type var 'a' here
-- because we want this to be a functor, so that 
-- we can have fun with recursion-schemes 
data Vfs a = Dir  String [Vfs a]
           | File String Int
           deriving (Eq,Functor,Ord,Show)

makeBaseFunctor  ''Vfs

data Path = Abs [String]
          | Rel [String]
          deriving(Eq,Ord,Show)

vfsName :: Vfs a -> String
vfsName (Dir  n _) = n
vfsName (File n _) = n

-- pretty print the filesystem
prettyVfs :: Vfs a -> IO ()
prettyVfs = go 0
    where go :: Int -> Vfs a -> IO ()
          go i (Dir name kids) = do
              putStr $ replicate i ' '
              putStr "dir "
              putStrLn name
              forM_ kids $ go (i+2)
          go i (File name size) = do
              putStr $ replicate i ' '
              putStr $ show size
              putChar ' '
              putStrLn name

-- UTILITIES
-- lookup child and remove from entries
lookup :: forall a. String -> [Vfs a] -> (Vfs a, [Vfs a])
lookup k = flip go []
    where go :: [Vfs a] -> [Vfs a] -> (Vfs a, [Vfs a])
          go [] _                        = error $ "key not found: " <> k
          go (x:xs) acc | vfsName x == k = (x, reverse acc ++ xs)
          go (x:xs) acc                  = go xs (x:acc) 

-- insert vfs entry, but don't if it's duplicate
insert :: forall a. Vfs a -> [Vfs a] -> [Vfs a]
insert f = flip go []
    where go :: [Vfs a] -> [Vfs a] -> [Vfs a]
          go [] acc = reverse $ f:acc
          go (x:xs) acc | vfsName x == vfsName f = reverse acc ++ x:xs
          go (x:xs) acc                          = go xs (x:acc)

-- ZIPPERS
-- parent name and our sibling nodes
data Crumb a = Crumb String [Vfs a]

-- current focus and path travelled
type Zipper a = (Vfs a, [Crumb a])

-- unzip one layer
unzip :: Zipper a -> Zipper a
unzip (d@(Dir _ _), Crumb par sibs:cs) =
    let sibs' = insert d sibs
     in (Dir par sibs', cs)
unzip _ = error "unzip: how did you find yourself here?"

goUp :: Zipper a -> Zipper a
goUp = unzip

goRoot :: Zipper a -> Zipper a
goRoot z@(Dir "/" _, _) = z
goRoot z@(Dir _   _, _) = goUp z
goRoot _                = error "goRoot: what r u doin?"

goDown :: String -> Zipper a -> Zipper a
goDown ".." z = goUp z
goDown d (Dir me kids, cs) =
    let (kid, kids') = lookup d kids
     in (kid, Crumb me kids':cs)
goDown _ _ = error "you done zooped"

navigate :: Path -> (Zipper a -> Zipper a)
navigate (Abs ps) = foldl' (.) id $ goRoot:map goDown ps
navigate (Rel ps) = foldl' (.) id $ map goDown ps

add :: Vfs a -> Zipper a -> Zipper a
add d@(Dir _ _) (Dir me kids, cs) =
    let kids' = insert d kids
     in (Dir me kids', cs)
add f@(File _ _) (Dir me kids, cs) =
    let kids' = insert f kids
     in (Dir me kids', cs)
add _ _ = error "what a horrible night to have a curse"

-- PARSING
-- a cool thing we're doing is ignoring the actual
-- parse results, we strictly use this to statefully
-- update the zipper, from which we will be able to 
-- build the full VFS tree
type Parser a b = Parsec String (Zipper b) a 

-- run a parser. bail on error
parse :: Parser a b -> String -> String -> a
parse p n = either (error . show) id . runParser p root n
    where root :: Zipper a
          root = (Dir "/" [], [])

-- parse a file down to a vfs , bailing on error
parseFile :: String -> Parser (Zipper a) b -> IO (Vfs a)
parseFile n p = fst . goRoot . parse p n <$> readFile n

terminal :: Parser (Zipper a) a 
terminal = manyTill line eof >> getState
    where line = (cmd <|> out) <* newline
          out = dir <|> file
          cmd = string "$ " *> (cd <|> ls)
          dir = do
              void $ string "dir "
              n <- fsname
              modifyState $ add (Dir n [])
          file = do
              sz <- read <$> many1 digit
              void $ char ' '
              n <- fsname
              modifyState $ add (File n sz)
          cd = do
              void $ string "cd "
              p <- path
              modifyState $ navigate p
          ls = void $ string "ls"
          path = do
              pre <- optionMaybe $ char '/'
              parts <- sepEndBy fsname $ char '/'
              return $ case pre of
                         Just _  -> Abs parts
                         Nothing -> Rel parts
          fsname = many1 $ alphaNum <|> oneOf "_-."

dirSizes :: Vfs Int -> [(String,Int)]
dirSizes = execWriter . cataA go
    where go :: VfsF Int (Writer [(String,Int)] Int) -> Writer [(String,Int)] Int
          go (DirF d kids) = do
              sz <- sum <$> sequence kids
              tell [(d,sz)]
              return sz
          go (FileF _ sz) = return sz

test :: Vfs Int
test = Dir "/"
    [ Dir "a"
        [ Dir "e"
            [ File "i" 584
            ]
        , File "f" 29116
        , File "g" 2557
        , File "h.lst" 62596
        ]
    , File "b.txt" 14848514
    , File "c.dat" 8504156
    , Dir "d"
        [ File "j" 4060174
        , File "d.log" 8033020
        , File "d.ext" 5626152
        , File "k" 7214296
        ]
    ]

-- part1 :: Vfs Int -> Int
-- part1 = sum . filter (<= 100000 . snd) . dirSizes

main :: IO ()
main = do
    path <- head <$> getArgs
    vfs <- parseFile path terminal
    return ()
    -- putStrLn $ mconcat ["test: ", ]
