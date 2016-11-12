module Chess.Core.Domain where

import qualified Data.Text as T

data Color = Black | White
    deriving Show

data PieceType = Pawn | Rook | Knight | Bishop | King | Queen
    deriving Show

data Piece = Piece { color     :: Color
                   , pieceType :: PieceType
                   }
    deriving Show

data Player = Human T.Text
              | Computer T.Text
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
                           , token :: T.Text
                           }
    deriving Show

{- Generate a new board.  -}
initBoard :: Int -> Int -> Board
initBoard width height = map ( \(x, y) -> newSpace x y (calcPieceColor x y) Pawn ) [ (i, j) | i <- [ 1 .. width ], j <- [1 .. height ] ]
                         where
                           newSpace :: Int -> Int -> Color -> PieceType -> Space
                           newSpace x y pc pt = Space { piece = Just Piece { color = White
                                                                           , pieceType = pt
                                                                           }
                                                      , color = pc
                                                      , coord = Coord x y
                                                      }

                           calcPieceColor :: Int -> Int -> Color
                           calcPieceColor x y = if ( even (x + y) ) then White else Black

{- Create a new board.  -}
initGame :: Int -> Int -> GameState
initGame width height = GameState { board = initBoard width height
                                  , moves = []
                                  , players = [ Human (T.pack "Kyle"), Computer (T.pack "Bot 1") ]
                                  , playerTurn = Human (T.pack "Kyle")
                                  , token = T.pack "abc"
                                  }
