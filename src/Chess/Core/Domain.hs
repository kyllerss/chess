module Chess.Core.Domain where

import qualified Data.List as DL
import qualified Data.Text.Lazy as T
import           GHC.Generics

data Color = Black | White
    deriving (Show, Eq)

data PieceType = Pawn | Rook | Knight | Bishop | King | Queen
    deriving (Show, Eq)

data PieceId = PieceId Int
  deriving (Show, Generic, Eq)

data Piece = Piece { color     :: Color
                   , pieceType :: PieceType
                   , player    :: Player
                   , pieceId   :: PieceId
                   }
    deriving (Show, Generic)

instance Eq Piece where
  (==) a b = (pieceId a) == (pieceId b)

data Player = Human T.Text Int
            | Computer T.Text Int
    deriving (Show, Eq)

data Coord = Coord Int Int
    deriving (Show, Generic, Eq)

data Space = Space { piece :: Maybe Piece
                   , color :: Color
                   , coord :: Coord
                   }
           | Void Coord
    deriving (Show, Generic, Eq)

data Board = Board [Space]
    deriving (Show, Generic, Eq)

spacesFromBoard :: Board -> [Space]
spacesFromBoard (Board sps) = sps

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
initBoard :: Int -> Int -> (Coord -> Space) -> Board
initBoard width height spaceBuilder =
    Board $
        map spaceBuilder
            [ Coord i j
            | i <- [0 .. (width - 1)]
            , j <- [0 .. (height - 1)] ] 

{- Create a new board.  -}
initGame :: Int -> Int -> GameState
initGame width height = GameState { board = initBoard width
                                                      height
                                                      defaultSpaceBuilder
                                  , moves = []
                                  , players = [ human1, ai1 ]
                                  , playerTurn = human1
                                  , token = T.pack "abc"
                                  }
  where
    human1 = Human (T.pack "Kyle") 1
    ai1 = Computer (T.pack "Bot 1") 2

{- default space builder -}
defaultSpaceBuilder :: Coord -> Space
defaultSpaceBuilder = \(Coord x y) -> buildSpace x y $ calcSpaceColor $ Coord x y

{- fetch space with given coordinates -}
fetchSpace :: Board -> Coord -> Maybe Space
fetchSpace (Board spaces) c =
    DL.find (\x -> coord x == c) spaces

{- alternates piece color -}
calcSpaceColor :: Coord -> Color
calcSpaceColor (Coord x y) = if (even (x + y)) then White else Black

{- space builder -}
buildSpace :: Int -> Int -> Color -> Space
buildSpace x y c = Space { piece = Nothing
                         , color = c
                         , coord = Coord x y
                         }

{- piece builder -}
buildPiece :: PieceId -> PieceType -> Color -> Player -> Piece
buildPiece pId pt color player = Piece { color = White
                                       , pieceType = pt
                                       , player = Human (T.pack "dummy1") 1
                                       , pieceId = pId 
                                       }

{- PieceId builder -}
buildPieceId :: Coord -> PieceId
buildPieceId (Coord x y) = PieceId (x * y + y + 1)

{- Remove a piece from a given space.  -}
removePiece :: Space -> Piece -> Space
removePiece srem spem = srem { piece = Nothing }

{- Add a piece to a given space.  -}
addPiece :: Space -> Piece -> Space
addPiece sadd padd = sadd { piece = Just padd }

{- Add a piece to board -}
addPieceToBoard :: Board -> Piece -> Coord -> Maybe Board
addPieceToBoard b@(Board sps) p c = addPiece' [] sps
  where

    addPiece' :: 
