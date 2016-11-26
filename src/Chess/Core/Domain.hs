module Chess.Core.Domain where

import qualified Data.List as DL
import qualified Data.Text.Lazy as T
import           GHC.Generics
import qualified Data.Map as Map

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
  (==) (Piece {pieceId = a}) (Piece {pieceId = b}) = a == b

data Player = Human T.Text Int
            | Computer T.Text Int
    deriving (Show, Eq)

data Coord = Coord Int Int
    deriving (Show, Generic, Eq)

instance Ord Coord where
  (Coord x1 y1) `compare` (Coord x2 y2) =
    ((x1 + 1) * (y1 + 1) + (y1 + 1)) `compare` ((x2 + 1) * (y2 + 1) + (y2 + 1))

data Space = Space { piece :: Maybe Piece
                   , color :: Color
                   , coord :: Coord
                   }
           | Void Coord
    deriving (Show, Generic, Eq)

instance Ord Space where
  (Space {coord = c1}) `compare` (Space {coord = c2}) = c1 `compare` c2

data Board = Board { spacesMap :: Map.Map Coord Space }
    deriving (Show, Generic, Eq)

data Move = Move { pId :: PieceId
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
    buildBoard $
        map spaceBuilder
            [ Coord i j
            | i <- [0 .. (width - 1)]
            , j <- [0 .. (height - 1)] ]

{- Builder function -}
buildBoard :: [Space] -> Board
buildBoard sps = Board {spacesMap = buildSpaceMap sps}

buildSpaceMap :: [Space] -> Map.Map Coord Space
buildSpaceMap sps = foldl (\m s -> Map.insert (coord s) s m) Map.empty sps

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
defaultSpaceBuilder = \c@(Coord x y) -> buildSpace x y $ calcSpaceColor $ c

{- fetch space with given coordinates -}
fetchSpace :: Board -> Coord -> Maybe Space
fetchSpace (Board {spacesMap = m}) c = Map.lookup c m

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
removePiece :: Space -> Space
removePiece srem = srem { piece = Nothing }

{- Add a piece to a given space.  -}
addPiece :: Space -> Piece -> Space
addPiece sadd padd = sadd { piece = Just padd }

{- Add a piece to board -}
addPieceToBoard :: Board -> Piece -> Coord -> Maybe Board
addPieceToBoard (Board {spacesMap = spsMap}) p c = addPiece' $ Map.lookup c spsMap
  where
    addPiece' :: Maybe Space -> Maybe Board
    addPiece' Nothing = Nothing
    addPiece' (Just s) = Just $ Board {spacesMap = Map.insert c (addPiece s p) spsMap}
