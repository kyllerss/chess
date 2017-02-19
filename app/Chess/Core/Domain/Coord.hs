module Chess.Core.Domain.Coord where

import Import

data Coord = Coord Int Int
    deriving (Show, Generic, Eq)

instance ToJSON Coord where
    toJSON (Coord x y) = toJSON $ [ x, y ]

instance Ord Coord where
    (Coord x1 y1) `compare` (Coord x2 y2) =
        (show x1 ++ " -- " ++ show y1) `compare` (show x2 ++ " -- " ++ show y2)

