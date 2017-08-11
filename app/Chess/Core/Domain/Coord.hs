module Chess.Core.Domain.Coord where

import Import
import qualified Data.Maybe as DM
import Chess.Core.Domain.Base

data Coord = Coord Int Int
    deriving (Show, Read, Generic, Eq, NFData)

instance ToJSON Coord where
    toJSON (Coord row col) = toJSON [ row, col ]

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

{- Fetches coordinates between two coords in a board (inclusive). -}
fetchBetweenCoords :: Coord -> Coord -> [Coord]
fetchBetweenCoords startCoord@(Coord r1 c1) endCoord@(Coord r2 c2) = approachCoords nextStartCoord endCoord
  where
    direction :: Maybe Direction
    direction
      | r1 == r2 && c1 > c2 = Just West
      | r1 == r2 && c2 > c1 = Just East
      | c1 == c2 && r1 > r2 = Just North
      | c1 == c2 && r2 > r1 = Just South
      | abs(r1 - r2) /= abs(c1 - c2) = Nothing   -- safety: don't allow non-direct line of sight inputs
      | c1 > c2 && r1 > r2 = Just NorthWest
      | c1 > c2 && r2 > r1 = Just SouthWest
      | c2 > c1 && r1 > r2 = Just NorthEast
      | c2 > c1 && r2 > r1 = Just SouthEast
      | otherwise = Nothing

    nextStartCoord :: Coord
    nextStartCoord = moveD startCoord (DM.fromJust direction) 1

    approachCoords :: Coord -> Coord -> [Coord]
    approachCoords sc ec
      | isNothing direction = []
      | sc == ec = []
      | otherwise = sc : approachCoords (moveD sc (DM.fromJust direction) 1) ec
