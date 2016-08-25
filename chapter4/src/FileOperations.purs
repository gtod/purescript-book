module FileOperations where

import Prelude
import Control.MonadZero (guard)
import Data.Array (tail, head, concatMap, (:))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Path (root, size, Path, ls, isDirectory)

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

maybeLess :: Maybe Int -> Maybe Int -> Boolean
maybeLess (Just x) (Just y) = x < y
maybeLess _ _ = false

-- This seems sort of subtle, we need to get the pattern match with the
-- acc right, and I'm not sure I have yet...
smallerPath :: Maybe Path -> Maybe Path -> Maybe Path
smallerPath (Just b) (Just a) = if (maybeLess (size a) (size b)) then (Just a) else (Just b)
smallerPath (Just b) _ = Just b
smallerPath _ (Just a) = Just a
smallerPath _ _ = Nothing

-- You gives us a path to look for the smallest file in, but there may not be
-- one so you only get a Maybe Path back...
smallestFile :: Path -> Maybe Path
-- Again, it's not that we can't make runtime tests but rather that the compiler
-- is satisfied (at compile time) that we have not introduced any type leaks...
smallestFile path = foldl smallerPath (if isFile path then (Just path) else Nothing) (map Just (onlyFiles path))
