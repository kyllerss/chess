module Chess.Core.Domain.Move where

import Import
import Chess.Core.Domain.Board
import Chess.Core.Domain.Coord
import Chess.Core.Domain.Piece
import Chess.Core.Domain.Space
import qualified Data.Maybe as DM

data Move = Move { movePieceId        :: PieceId
                 , moveSpace          :: Space
                 , moveIsConsumable   :: Bool
                 , moveSideEffects    :: [Move]
                 }
    deriving (Show, Eq)

instance ToJSON Move where
    toJSON (Move{moveSpace = Space{spaceCoord = Coord x y}}) = toJSON $ [ x, y ]
    toJSON (Move{moveSpace = Void (Coord x y)}) = toJSON $ [ x, y ]

{- Convenience builder for Move -}
buildMove :: Piece -> Board -> Coord -> Bool -> [Move] -> Maybe Move
buildMove p b c offensive sideEffects
  | fetchSpace c b == Nothing = Nothing
  | otherwise = Just Move { movePieceId = pieceId p
                          , moveSpace = DM.fromJust $ fetchSpace c b
                          , moveIsConsumable = offensive
                          , moveSideEffects = sideEffects
                          }
