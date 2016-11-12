module Chess.Core.Domain where

data Color = Black | White
    deriving Show

data PieceType = Pawn | Rook | Knight | Bishop | King | Queen
    deriving Show

data Piece = Piece { color     :: Color
                   , pieceType :: PieceType
                   }
    deriving Show

data Player = Human String
              | Computer String
    deriving Show

data Coordinate = Coordinate Int Int
    deriving Show

data Space = Space { piece :: Maybe Piece
                   , color :: Color
                   , coord :: Coordinate
                   }
           | Void Coordinate
    deriving Show

type Board = [Space]

data Move = Move { piece    :: Piece
                 , position :: Coordinate
                 }
    deriving Show

data GameState = GameState { board :: Board
                           , moves :: [Move]
                           }
    deriving Show
