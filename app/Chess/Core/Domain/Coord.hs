module Chess.Core.Domain.Coord where

import Import
import Chess.Core.Domain.Base

data Coord = Coord Int Int
    deriving (Show, Read, Generic, Eq, NFData)

instance ToJSON Coord where
    toJSON (Coord row col) = toJSON $ [ row, col ]

instance FromJSON Coord where
  parseJSON value = do
    [row, col] <- parseJSON value
    return $ Coord row col

{-
import Data.Aeson
let c = encode $ Coord 1 2
(decode c) :: Maybe Coord
-}

instance Ord Coord where
    (Coord row1 col1) `compare` (Coord row2 col2) =
        (show row1 ++ " -- " ++ show col1) `compare` (show row2 ++ " -- " ++ show col2)

{- Utility function for incrementing space based on direction. -}
moveD :: Coord -> Direction -> Int -> Coord
moveD (Coord row col) dir cnt
    | dir == North     = Coord (row - cnt) col
    | dir == NorthEast = Coord (row - cnt) (col + cnt)
    | dir == East      = Coord row (col + cnt)
    | dir == SouthEast = Coord (row + cnt) (col + cnt)
    | dir == South     = Coord (row + cnt) col
    | dir == SouthWest = Coord (row + cnt) (col - cnt)
    | dir == West      = Coord row (col - cnt)
    | dir == NorthWest = Coord (row - cnt) (col - cnt)
    | otherwise = error "Unkown Direction value!" -- TODO: Why is this needed?
