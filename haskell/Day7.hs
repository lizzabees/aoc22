#!/usr/bin/env cabal
{- cabal:
build-depends: base, parsec, split
ghc-options: -O2
-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import Control.Monad (forM_)
import Data.List (foldl')
import Debug.Trace (trace)
import Text.Parsec
import System.Environment (getArgs)

data Path = Abs [String]
          | Rel [String]
          deriving(Eq,Ord,Show)

type Entries = [(String,VFS)]

data VFS = Dir  Entries
         | File Int
         deriving(Eq,Ord,Show)

-- movement inside the VFS
data Move = GoRoot | GoUp | GoDown String
    deriving (Eq,Ord,Show)

-- given a path, what moves must we make to get there
moves :: Path -> [Move]
moves ps = go ps []
    where go :: Path -> [Move] -> [Move]
          go (Abs       ps ) ms = go (Rel ps) ( GoRoot   :ms)
          go (Rel ("..":ps)) ms = go (Rel ps) ( GoUp     :ms)
          go (Rel (p   :ps)) ms = go (Rel ps) ((GoDown p):ms)
          go (Rel [       ]) ms = reverse ms

-- our path so far: the previous VFS tree
-- *without* the path we took, as well as
-- the move we made to get here. this should
-- be all we need to reconstruct the full tree
-- once done processing commands
data Crumb = Crumb String Entries Move
    deriving (Eq,Ord,Show)

-- our currently focused segment
-- of the VFS and our trail of crumbs
type Zipper = (VFS, [Crumb])

-- lookup but we remove the found element
lookOut :: forall a b. (Eq a,Show a) => a -> [(a,b)] -> (b, [(a,b)])
lookOut key xs = go xs []
    where go :: [(a,b)] -> [(a,b)] -> (b, [(a,b)])
          go [        ]  _ = error $ "missing key " <> show key
          go ((k,v):xs) ys | k == key = (v, (reverse ys) ++ xs)
          go (x    :xs) ys            = go xs (x:ys)

-- insert only if not already existing
-- (we don't wanna whipe out a dir we have
-- already visited)
mayInsert :: forall a b. (Eq a,Show a) => (a,b) -> [(a,b)] -> [(a,b)]
mayInsert (key,val) xs = go xs []
    where go :: [(a,b)] -> [(a,b)] -> [(a,b)]
          go [] ys  = reverse $ (key,val):ys
          go ((k,v):xs) ys | k == key = (reverse ys) ++ ((k,v):xs)
          go (x    :xs) ys            = go xs (x:ys)

-- parsing
type Parser a = Parsec String () a

runParse :: Parser a -> String -> a
runParse p = either (error . show)  id . parse p ""

data Input = InCd Path
           | InLs
           | InDir String
           | InFile Int String
           deriving(Eq,Ord,Show)
         
fsname :: Parser String
fsname = many1 (alphaNum <|> oneOf "_-.")

path :: Parser Path
path = do
    pre <- optionMaybe (char '/')
    parts <- sepEndBy fsname $ char '/'
    return $ case pre of 
               Just _  -> Abs parts
               Nothing -> Rel parts

input :: Parser [Input]
input = line `manyTill` eof
    where line = (cmd <|> out) <* newline
          out  = dir <|> file
          cmd  = do { string "$ "  ; cd <|> ls        }
          cd   = do { string "cd " ; InCd <$> path    }
          ls   = do { string "ls"  ; return InLs      }
          dir  = do { string "dir "; InDir <$> fsname }
          file = do { InFile <$> size <*> fsname      }
          size = do { read <$> many1 digit <* char ' '}
          
-- execution
goDown :: String -> Zipper -> Zipper
goDown _    (File   _,  _) = error "cannot navigate files!"
goDown name (Dir kids, bs)  =
    let (dir, kids') = lookOut name kids
     in (dir, Crumb name kids' (GoDown name):bs)

-- utility for unwinding zipper
shiftUp :: Entries -> [Crumb] -> Zipper
shiftUp    _ [                      ] = error "cannot go up from empty path"
shiftUp kids ((Crumb cwd aunts _):bs) =
    let aunts' = mayInsert (cwd, Dir kids) aunts
     in (Dir aunts', bs)

goUp :: Zipper -> Zipper
goUp (Dir    _, []) = error "cannot go up from root"
goUp (File   _,  _) = error "cannot navigate files"
goUp (Dir kids, bs) = shiftUp kids bs

goRoot :: Zipper -> Zipper
goRoot (Dir kids, []) = (Dir kids, [])
goRoot (Dir kids, bs) = goRoot $ shiftUp kids bs
goRoot (File   _,  _) = error "cannot navigate files"

navigate :: Move -> Zipper -> Zipper
navigate  GoRoot    = goRoot
navigate  GoUp      = goUp
navigate (GoDown p) = goDown p

insert :: String -> VFS -> Zipper -> Zipper
insert     _   _ (File   _,  _) = error "cannot insert into file"
insert  name kid (Dir kids, bs) =
    let kids' = mayInsert (name,kid) kids
     in (Dir kids', bs)

runInput :: [Input] -> VFS
runInput = fst . goRoot . foldl' (flip go) (Dir [], [])
    where go :: Input -> Zipper -> Zipper
          go (InLs      ) z = z
          go (InCd     p) z = foldr navigate z $ moves p
          go (InDir    n) z = insert n (Dir  []) z
          go (InFile s n) z = insert n (File  s) z


part1 :: VFS -> Int
part1 = sum . filter (< 1000) . sizes []
    where sizes :: [Int] -> VFS -> [Int]
          sizes acc (File sz) = sz:acc
          sizes acc (Dir  []) = acc
          sizes acc (Dir  xs) = foldl' sizes acc $ map snd xs

main :: IO ()
main = do
    path <- head <$> getArgs
    term <- runParse input <$> readFile path
    let vfs = runInput term
    putStrLn $ mconcat ["part 1: ", show $ part1 vfs]
