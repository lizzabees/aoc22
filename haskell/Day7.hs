#!/usr/bin/env cabal
{- cabal:
build-depends: base, parsec
ghc-options: -O2
-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import Control.Monad (forM_)
import Data.Functor (void)
import Data.List (foldl')
import Prelude hiding (lookup, unzip)
import System.Environment (getArgs)
import Text.Parsec hiding (parse)

-- VFS basics
type Entry = (String,VFS)

data VFS = Dir  String [Entry]
         | File String Int
         deriving(Eq,Ord,Show)

data Path = Abs [String]
          | Rel [String]
          deriving(Eq,Ord,Show)

-- Zippers
data Crumb = Crumb
    { crName :: String  -- parent's name
    , crSibs :: [Entry] -- sibling nodes
    } deriving (Eq, Ord, Show)

type Zipper = (VFS, [Crumb])

-- pretty print the filesystem
prettyVfs :: VFS -> IO ()
prettyVfs = go 0
    where go :: Int -> VFS -> IO ()
          go i (Dir name kids) = do
              putStr $ replicate i ' '
              putStr "dir "
              putStrLn name
              forM_ kids (go (i+2) . snd)
          go i (File name size) = do
              putStr $ replicate i ' '
              putStr $ show size
              putChar ' '
              putStrLn name

-- helper to unzip one level
unzip :: Zipper -> Zipper
unzip (File {   },  _) = error "cannot unzip files"
unzip (Dir  {   }, []) = error "cannot unzip empty zipper"
unzip (Dir  "/" _,  _) = error "cannot unzip root zipper"
unzip (Dir n es, Crumb{..}:cs) =
    let sibs = insert n (Dir n es) crSibs
     in (Dir crName sibs, cs)

goUp :: Zipper -> Zipper
goUp = unzip

goRoot :: Zipper -> Zipper
goRoot z@(Dir "/" _, _) = z
goRoot z = goUp z

goDown :: String -> Zipper -> Zipper
goDown ".." z              = goUp z
goDown _    (File {}, _)   = error "cannot zip down file"
goDown p    (Dir n es, cs) =
    case lookup p es of
      (File{}, _) -> error "cannot zip down file"
      (fs,  sibs) -> (fs, Crumb n sibs:cs)

fsAdd :: VFS -> Zipper -> Zipper
fsAdd f@(Dir  n _) (Dir  p sibs, cs) = (Dir p (insert n f sibs), cs)
fsAdd f@(File n _) (Dir  p sibs, cs) = (Dir p (insert n f sibs), cs)
fsAdd _            (File {    },  _) = error "cannot insert into file" 

navigate :: Path -> (Zipper -> Zipper)
navigate (Abs ps) = foldl' (.) id $ goRoot:map goDown ps
navigate (Rel ps) = foldl' (.) id $ map goDown ps

-- parsing
type Parser a = Parsec String Zipper a

parse :: Parser a -> SourceName -> String -> a
parse p s = either (error . show) id . runParser p root s
    where root :: Zipper
          root = (Dir "/" [], [])

parseFile :: Parser a -> String -> IO a
parseFile p n = parse p n <$> readFile n

-- parse a terminal session returning the zipper
-- that we build up as we go
terminal :: Parser Zipper
terminal = manyTill line eof >> getState
    where line = (cmd <|> out) <* newline
          out  = dir <|> file
          cmd  = string "$ " *> (cd <|> ls)
          dir  = do
              void $ string "dir "
              n <- fsname
              modifyState $ fsAdd (Dir n [])
          file = do
              size <- read <$> many1 digit
              void $ char ' '
              name <- fsname
              modifyState $ fsAdd (File name size)
          cd   = do 
              void $ string "cd "
              p <- path
              modifyState $ navigate p
          ls   = void $ string "ls"
          path = do
              pre   <- optionMaybe $ char '/'
              parts <- sepEndBy fsname $ char '/'
              return $ case pre of
                         Just _  -> Abs parts
                         Nothing -> Rel parts
          fsname = many1 $ alphaNum <|> oneOf "_-."

-- simple a-list maps but when we insert we do not replace
-- existing keys, and when we lookup, we remove the key
-- from the map and return the point in which we split it
type Map k v = [(k,v)]

lookup :: forall k v. Eq k => k -> Map k v -> (v, Map k v)
lookup k = flip go []
    where go :: Map k v -> Map k v -> (v, Map k v)
          go [        ]  _          = error "map key not found"
          go ((x,v):xs) ys | x == k = (v, reverse ys ++ xs)
          go ( x   :xs) ys          = go xs $ x:ys

insert :: forall k v. Eq k => k -> v -> Map k v -> Map k v
insert k v = flip go []
    where go :: Map k v -> Map k v -> Map k v
          go [        ] ys          = reverse $ (k,v):ys
          go ((x,_):xs) ys | x == k = reverse ys ++ xs
          go ( x   :xs) ys          = go xs $ x:ys

main :: IO ()
main = do
    path <- head <$> getArgs
    vfs <- fst . goRoot <$> parseFile terminal path
    prettyVfs vfs
