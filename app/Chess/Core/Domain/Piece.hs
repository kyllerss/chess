module Chess.Core.Domain.Piece where

import Import
import Chess.Core.Domain.Base
import Chess.Core.Domain.Coord
import Chess.Core.Domain.Player

data PieceType = Pawn | Rook | Knight | Bishop | King | Queen
    deriving (Show, Read, Eq, Ord, Generic, NFData)

instance ToJSON PieceType where
    toJSON Pawn = toJSON ("p" :: Text)
    toJSON Rook = toJSON ("r" :: Text)
    toJSON Knight = toJSON ("n" :: Text)
    toJSON Bishop = toJSON ("b" :: Text)
    toJSON King = toJSON ("k" :: Text)
    toJSON Queen = toJSON ("q" :: Text)

data PieceId = PieceId {pieceIdValue :: Int}
    deriving (Show, Read, Generic, Eq, NFData)

instance ToJSON PieceId where
    toJSON (PieceId indx) = toJSON (indx :: Int)

instance FromJSON PieceId where
  parseJSON value = do
    idx <- parseJSON value
    return $ PieceId idx

{-
import Data.Aeson
let c = encode $ Coord 1 2
(decode c) :: Maybe Coord
-}

data Piece = Piece { pieceColor   :: Color 
                   , pieceType    :: PieceType
                   , piecePlayer  :: Player
                   , pieceId      :: PieceId
                   , pieceOrigin  :: Maybe Coord
                   , pieceMoved   :: Bool
                   }
    deriving (Show, Read, Generic, NFData)

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
    PieceId (((x + 1) * 7) * ((y + 1) * 11) + (y + 1))

