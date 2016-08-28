module FileOperations where

import Prelude
import Control.MonadZero (guard)
import Data.Array (concatMap, (:))
import Data.Foldable (foldl)
import Data.Maybe (isJust, Maybe(Nothing, Just))
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

smallestFile :: Path -> Maybe Path
smallestFile = foldl smallerPath Nothing <<< onlyFiles

-- Ex 4.17.3
-- This works now but is it sort of buggy because of what happens when we get
-- multiple matches?
whereIs :: String -> Maybe Path
whereIs name = foldl (\b a -> if isJust a then a else b) Nothing $ findFile root root
  where
    matchesName :: Path -> Boolean
    matchesName path = name == filename path

    tryMatch :: Path -> Path -> Maybe Path
    tryMatch path parent = if matchesName path then Just parent else Nothing

    findFile :: Path -> Path -> Array (Maybe Path)
    findFile node parent = tryMatch node parent : do
     path <- ls node
     findFile path node
