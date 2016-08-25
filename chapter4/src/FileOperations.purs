module FileOperations where

import Prelude
import Control.MonadZero (guard)
import Data.Array (concatMap, (:))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Path (size, Path, ls, isDirectory)

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
