module Chess.Core.Domain.Base where

import Import

data Color = Black | White
    deriving (Show, Eq)

instance ToJSON Color where
    toJSON Black = toJSON ("b" :: Text)
    toJSON White = toJSON ("w" :: Text)

data Direction = North
               | NorthEast
               | East
               | SouthEast
               | South
               | SouthWest
               | West
               | NorthWest
    deriving (Show, Eq, Ord, Enum, Bounded)

{- Increment diagonal -}
rotateRight :: (Bounded a, Enum a, Eq a) => a -> a 
rotateRight d
  | d == maxBound = minBound
  | otherwise = succ d

rotateLeft :: (Bounded a, Enum a, Eq a) => a -> a 
rotateLeft d
  | d == minBound = maxBound
  | otherwise = pred d



