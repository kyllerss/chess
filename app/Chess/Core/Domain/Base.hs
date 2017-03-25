module Chess.Core.Domain.Base where

import Import

data Color = Black | White
    deriving (Show, Eq, Read, Generic, NFData)

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
    deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic, NFData)

{- Increment diagonal -}
rotateRight :: (Bounded a, Enum a, Eq a) => a -> a 
rotateRight d
  | d == maxBound = minBound
  | otherwise = succ d

rotateLeft :: (Bounded a, Enum a, Eq a) => a -> a 
rotateLeft d
  | d == minBound = maxBound
  | otherwise = pred d



