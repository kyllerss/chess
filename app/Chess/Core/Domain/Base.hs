module Chess.Core.Domain.Base where

import Import

data Color =  White | Black | Green | Blue | Orange | Yellow | Red | Purple
    deriving (Show, Eq, Read, Bounded, Enum, Generic, NFData)

instance ToJSON Color where
    toJSON White =  toJSON ("w" :: Text)
    toJSON Black =  toJSON ("b" :: Text)
    toJSON Green =  toJSON ("g" :: Text)
    toJSON Blue =   toJSON ("c" :: Text)
    toJSON Purple = toJSON ("p" :: Text)
    toJSON Orange = toJSON ("o" :: Text)
    toJSON Yellow = toJSON ("y" :: Text)
    toJSON Red =    toJSON ("r" :: Text)

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



