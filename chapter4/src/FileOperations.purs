module FileOperations where

import Prelude
import Control.MonadZero (guard)
import Data.Array (null, filter, concatMap, (:))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Path (isDirectory, size, Path, ls, filename, root)

allFiles :: Path -> Array Path
allFiles root = root : concatMap allFiles (ls root)

allFiles' :: Path -> Array Path
allFiles' file = file : do
  child <- ls file
  allFiles' child

isFile :: Path -> Boolean
isFile file = not (isDirectory file)

-- Ex 4.17.1
-- Ahh, filter is the grep vrsion of map
onlyFiles :: Path -> Array Path
onlyFiles path = do
  file <- allFiles' path
  guard $ isFile file
  pure file

-- Ex 4.17.2a
-- Ahh, THINK, only your acc needed to be Maybe...
smallerPath :: Maybe Path -> Path -> Maybe Path
smallerPath (Just x) y = if (size x) < (size y) then Just x else Just y
smallerPath Nothing y = Just y

-- You give us a path to look for the smallest file in, but there may not be
-- one so you only get a Maybe Path back...
smallestFile :: Path -> Maybe Path
-- Again, it's not that we can't make runtime tests but rather that the compiler
-- is satisfied (at compile time) that we have not introduced any type leaks...
-- Note well, smallestFile returns a *function*, it doesn't *do anything* (yet).
smallestFile = foldl smallerPath Nothing <<< onlyFiles
-- contrast with:
-- smallestFile path = foldl smallerPath Nothing (onlyFiles path)
-- this is written by someone who still wants his functions to *do things* rather
-- than just be pieces of a puzzle to be asembled and (finally) used elsewhere

-- Ex 4.17.3
{-
We have a string and want to match it to a filename
We must return the directory in which it sits (if any).

Start at the given root node
Check each of the contained paths (ie ls node)
  If file
    If match DONE, returning current path
  Else
    recurse

Do the recusion to traverse the tree, but only for directories


-}



--map (\path -> if isDirectory path then whereIs' path else (if (filename path) == name then path else "")


whereIs :: String -> Array (Array Path)
whereIs name = filter (\x -> not (null x)) (findFile root root)
  where
    matchesName :: Path  -> Boolean
    matchesName path = name == filename path

    match :: Path -> Path -> Array Path
    match path parent = if (matchesName path) then [parent] else []

    findFile :: Path -> Path -> Array (Array Path)
    findFile node parent = (match node parent) : do
     path <- ls node
     findFile path node

-- From https://github.com/ChrisCoffey/ps-by-example/blob/master/src/chapter4/FileSystem.purs
-- whereIs :: String -> Maybe Path
-- whereIs term = let
--     match = do
--         dir <- allDirs
--         matching <- dirMatch dir
--         pure matching
--     in case match of
--         ([dir]) -> Just dir
--         _       -> Nothing
--     where
--         allDirs = filter isDirectory $ allFiles root
--         dirMatch dir =
--             do child <- ls dir
--                guard $ (filename child) == (filename dir <> term)
--                pure dir


{-
KEY POINT: If you can't write this code simply because you do not understand the
fundamentals of the lnaguage well enough, then FIX that and get on with it.

On the other hand, if the langauge is truly obtuse, do something else.

Might be worth trying to write this three different ways in Common Lisp to compare...

Then blg about that...

-}
