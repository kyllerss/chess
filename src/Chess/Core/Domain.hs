module Chess.Core.Domain where

import qualified Data.Text.Lazy as T
import           GHC.Generics

data Color = Black | White
    deriving Show

data PieceType = Pawn | Rook | Knight | Bishop | King | Queen
    deriving Show

data Piece = Piece { color     :: Color
                   , pieceType :: PieceType
                   , player    :: Player
                   , pieceId   :: Int
                   }
    deriving (Show, Generic)

data Player = Human T.Text Int
            | Computer T.Text Int
    deriving Show

data Coord = Coord Int Int
    deriving (Show, Generic)

data Space = Space { piece :: Maybe Piece
                   , color :: Color
                   , coord :: Coord
                   }
           | Void Coord
    deriving (Show, Generic)

data Board = Board [Space]
    deriving (Show, Generic)

--  toJSON b = --J.encode(b)
data Move = Move { piece :: Piece
                 , space :: Space
                 }
    deriving Show

data GameState = GameState { board      :: Board
                           , moves      :: [Move]
                           , players    :: [Player]
                           , playerTurn :: Player
                           , token      :: T.Text
                           }
    deriving Show

{- Generate a new board.  -}
initBoard :: Int -> Int -> Board
initBoard width height =
    Board $
        map (\(x, y) -> newSpace x y (calcPieceColor x y) Pawn)
            [ (i, j)
            | i <- [1 .. width]
            , j <- [1 .. height] ]
  where
    -- ++ [Void (Coord 0 0)]
    dummyPlayer = Human (T.pack "dummy1") 1

    newSpace :: Int -> Int -> Color -> PieceType -> Space
    newSpace x y pc pt = Space { piece = Just Piece { color = White
                                                    , pieceType = pt
                                                    , player = dummyPlayer
                                                    , pieceId = (x * y + y)
                                                    }
                               , color = pc
                               , coord = Coord x y
                               }

    calcPieceColor :: Int -> Int -> Color
    calcPieceColor x y = if (even (x + y)) then White else Black

{- Create a new board.  -}
initGame :: Int -> Int -> GameState
initGame width height = GameState { board = initBoard width height
                                  , moves = []
                                  , players = [ human1, ai1 ]
                                  , playerTurn = human1
                                  , token = T.pack "abc"
                                  }
  where
    human1 = Human (T.pack "Kyle") 1
    ai1 = Computer (T.pack "Bot 1") 2
