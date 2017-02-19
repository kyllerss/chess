module Chess.Core.Domain.Piece where

import Import
import Chess.Core.Domain.Base
import Chess.Core.Domain.Coord
import Chess.Core.Domain.Player

data PieceType = Pawn | Rook | Knight | Bishop | King | Queen
    deriving (Show, Eq)

instance ToJSON PieceType where
    toJSON Pawn = toJSON ("p" :: Text)
    toJSON Rook = toJSON ("r" :: Text)
    toJSON Knight = toJSON ("n" :: Text)
    toJSON Bishop = toJSON ("b" :: Text)
    toJSON King = toJSON ("k" :: Text)
    toJSON Queen = toJSON ("q" :: Text)

data PieceId = PieceId {pieceIdValue :: Int}
    deriving (Show, Generic, Eq)

instance ToJSON PieceId where
    toJSON (PieceId indx) = toJSON (indx :: Int)

data Piece = Piece { pieceColor   :: Color
                   , pieceType    :: PieceType
                   , piecePlayer  :: Player
                   , pieceId      :: PieceId
                   , pieceOrigin  :: Maybe Coord
                   , pieceMoved   :: Bool
                   }
    deriving (Show, Generic)

instance ToJSON Piece

instance Eq Piece where
    (==) (Piece{pieceId = a}) (Piece{pieceId = b}) =
        a == b

{- piece builder -}
buildPiece :: PieceId -> PieceType -> Color -> Player -> Maybe Coord -> Piece
buildPiece pId pt color player initCoord =
    Piece { pieceColor = color
          , pieceType = pt
          , piecePlayer = player
          , pieceId = pId
          , pieceOrigin = initCoord
          , pieceMoved = False
          }

{- PieceId builder -}
buildPieceId :: Coord -> PieceId
buildPieceId (Coord x y) =
    PieceId ((x + 1) * (y + 1) + (y + 1))

