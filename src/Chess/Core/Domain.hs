module Chess.Core.Domain where

import Data.Text

data Color = Black | White
    deriving Show

data PieceType = Pawn | Rook | Knight | Bishop | King | Queen
    deriving Show

data Piece = Piece { color     :: Color
                   , pieceType :: PieceType
                   }
    deriving Show

data Player = Human Text
              | Computer Text
    deriving Show

data Coord = Coord Int Int
    deriving Show

data Space = Space { piece :: Maybe Piece
                   , color :: Color
                   , coord :: Coord
                   }
             | Void Coord
    deriving Show

type Board = [Space]

data Move = Move { piece    :: Piece
                 , space    :: Space
                 }
    deriving Show

data GameState = GameState { board :: Board
                           , moves :: [Move]
                           , players :: [Player]
                           , playerTurn :: Player
                           , token :: Text
                           }
    deriving Show

initGame :: Int -> Int -> GameState
initGame width height = GameState {board = []
                                  , moves = []
                                  , players = []
                                  , playerTurn = Human (pack "Kyle")
                                  , token = pack "abc"
                                  }
