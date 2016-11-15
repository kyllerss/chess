{-# LANGUAGE OverloadedStrings #-}

module Chess.Core.Domain where

import qualified Data.Text.Lazy    as T
import qualified Data.Text as TT
import           Data.Aeson        ( FromJSON, ToJSON )
import           Data.Aeson.Types
import qualified Data.Aeson.Encode as J
import           GHC.Generics
import           Data.List         as DL ( groupBy, sortBy )

data Color = Black | White
    deriving Show

instance ToJSON Color where
    toJSON Black = toJSON ("b" :: T.Text)
    toJSON White = toJSON ("w" :: T.Text)

data PieceType = Pawn | Rook | Knight | Bishop | King | Queen
    deriving Show

instance ToJSON PieceType where
    toJSON Pawn = toJSON ("p" :: T.Text)
    toJSON Rook = toJSON ("r" :: T.Text)
    toJSON Knight = toJSON ("n" :: T.Text)
    toJSON Bishop = toJSON ("b" :: T.Text)
    toJSON King = toJSON ("k" :: T.Text)
    toJSON Queen = toJSON ("q" :: T.Text)

data Piece = Piece { color     :: Color
                   , pieceType :: PieceType
                   , player    :: Player
                   , pieceId   :: Int
                   }
    deriving (Show, Generic)

instance ToJSON Piece

data Player = Human T.Text Int
            | Computer T.Text Int
    deriving Show

instance ToJSON Player where
    toJSON (Human name id) =
        object [ "name" .= name, "id" .= id ]
    toJSON (Computer name id) =
        object [ "name" .= name, "id" .= id ]

data Coord = Coord Int Int
    deriving (Show, Generic)

instance ToJSON Coord where
    toJSON (Coord x y) = toJSON $ "[" ++ show x ++ "," ++ show y ++ "]"

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
    toJSON (Board sp) = toJSON $ encode . toColor . shape . sortSpaces $ sp
      where
        multimap :: (a -> b) -> [[a]] -> [[b]]
        multimap f ss = map (\xs -> map f xs) ss

        -- group by row
        shape :: [Space] -> [[Space]]
        shape vs = DL.groupBy (\a -> (\b -> (extractRow a) == (extractRow b)))
                              vs
          where
            extractRow :: Space -> Int
            extractRow (Space{coord = Coord row _}) =
                row

        encode :: [[Color]] -> [[Value]]
        encode sp = multimap (\x -> toJSON x) sp

        toColor :: [[Space]] -> [[Color]]
        toColor sp = multimap (\s -> color (s :: Space)) sp

        sortSpaces :: [Space] -> [Space]
        sortSpaces sp = DL.sortBy orderByColumnAndRow sp

        orderByColumnAndRow :: Space -> Space -> Ordering
        orderByColumnAndRow a b =
            case (a, b) of
                (Void (Coord a1 b1), Void (Coord a2 b2))
                    | a1 > a2 -> GT
                    | a2 > a1 -> LT
                    | a1 == a2 -> if (b1 > b2) then GT else LT
                    | otherwise -> EQ
                (Void (Coord a1 b1), Space{coord = (Coord a2 b2)})
                    | a1 > a2 -> GT
                    | a2 > a1 -> LT
                    | a1 == a2 -> if (b1 > b2) then GT else LT
                    | otherwise -> EQ
                (Space{coord = (Coord a1 b1)}, Void (Coord a2 b2))
                    | a1 > a2 -> GT
                    | a2 > a1 -> LT
                    | a1 == a2 -> if (b1 > b2) then GT else LT
                    | otherwise -> EQ
                (Space{coord = (Coord a1 b1)}, Space{coord = (Coord a2 b2)})
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
    deriving Show

instance ToJSON GameState where
    toJSON GameState{board = b} =
        object [ "board" .= b, "pieces" .= (renderPieces b) ]
      where
        renderPieces :: Board -> [Value]
        renderPieces (Board sp) =
            map (\(Space{piece = Just p}) ->
                     object [ (TT.pack (show (pieceId p))) .= T.pack "1" ])
                sp

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
