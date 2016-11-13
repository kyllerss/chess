module Chess.Core.Domain where

import qualified Data.Text as T
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Types
import qualified Data.Aeson.Encode as J
import GHC.Generics
import Data.List as DL (sortBy)

data Color = Black | White deriving Show
instance ToJSON Color where
  toJSON Black = toJSON $ show "b"
  toJSON White = toJSON $ show "w"

data PieceType = Pawn | Rook | Knight | Bishop | King | Queen
  deriving Show
instance ToJSON PieceType where
  toJSON Pawn = toJSON $ show "p"
  toJSON Rook = toJSON $ show "r"
  toJSON Knight = toJSON $ show "n"
  toJSON Bishop = toJSON $ show "b"
  toJSON King = toJSON $ show "k"
  toJSON Queen = toJSON $ show "q"

data Piece = Piece { color     :: Color
                   , pieceType :: PieceType
                   }
    deriving (Show, Generic)
instance ToJSON Piece

data Player = Human T.Text
            | Computer T.Text
    deriving Show

data Coord = Coord Int Int
  deriving (Show, Generic)
instance ToJSON Coord where
  toJSON (Coord x y) = toJSON $ show "[" ++ show x ++ "," ++ show y ++ "]"

data Space = Space { piece :: Maybe Piece
                   , color :: Color
                   , coord :: Coord
                   }
           | Void Coord
    deriving (Show, Generic)
instance ToJSON Space

data Board = Board [Space]
  deriving (Show, Generic)
instance ToJSON Board where
  toJSON (Board sp) = toJSON $ show $ DL.sortBy orderByColumnAndRow sp
    where
      orderByColumnAndRow :: Space -> Space -> Ordering
      orderByColumnAndRow a b = case (a, b) of
        (Void (Coord a1 b1), Void (Coord a2 b2))
          | a1 > a2 -> GT
          | a2 > a1 -> LT
          | a1 == a2 -> if (b1 > b2) then GT else LT
          | otherwise -> EQ
        (Void (Coord a1 b1), Space {coord = (Coord a2 b2)})
          | a1 > a2 -> GT
          | a2 > a1 -> LT
          | a1 == a2 -> if (b1 > b2) then GT else LT
          | otherwise -> EQ
        (Space {coord = (Coord a1 b1)}, Void (Coord a2 b2))
          | a1 > a2 -> GT
          | a2 > a1 -> LT
          | a1 == a2 -> if (b1 > b2) then GT else LT
          | otherwise -> EQ
        (Space {coord = (Coord a1 b1)}, Space {coord = (Coord a2 b2)})
          | a1 > a2 -> GT
          | a2 > a1 -> LT
          | a1 == a2 -> if (b1 > b2) then GT else LT
          | otherwise -> EQ

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

instance Show GameState where
    show _ = "Hello!"

{- Generate a new board.  -}
initBoard :: Int -> Int -> Board
initBoard width height =
    Board $
        map (\(x, y) -> newSpace x y (calcPieceColor x y) Pawn)
            [ (i, j)
            | i <- [1 .. width]
            , j <- [1 .. height] ]
        -- ++ [Void (Coord 0 0)]
  where
    newSpace :: Int -> Int -> Color -> PieceType -> Space
    newSpace x y pc pt = Space { piece = Just Piece { color = White
                                                    , pieceType = pt
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
                                  , players = [ Human (T.pack "Kyle")
                                              , Computer (T.pack "Bot 1")
                                              ]
                                  , playerTurn = Human (T.pack "Kyle")
                                  , token = T.pack "abc"
                                  }
