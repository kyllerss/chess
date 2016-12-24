module Chess.Core.Domain where

import qualified Data.List      as DL
import qualified Data.Text.Lazy as T
import           GHC.Generics
import qualified Data.Map       as Map
import qualified Data.Maybe     as DM

data Color = Black | White
    deriving (Show, Eq)

data PieceType = Pawn | Rook | Knight | Bishop | King | Queen
    deriving (Show, Eq)

data PieceId = PieceId Int
    deriving (Show, Generic, Eq)

data Piece = Piece { pieceColor   :: Color
                   , pieceType    :: PieceType
                   , piecePlayer  :: Player
                   , pieceId      :: PieceId
                   , pieceOrigin  :: Maybe Coord
                   , pieceMoved   :: Bool
                   }
    deriving (Show, Generic)

instance Eq Piece where
    (==) (Piece{pieceId = a}) (Piece{pieceId = b}) =
        a == b

data Direction = North
               | NorthEast
               | East
               | SouthEast
               | South
               | SouthWest
               | West
               | NorthWest
    deriving (Show, Eq, Ord, Enum, Bounded)

data PlayerType = Human | Computer
    deriving (Show, Eq)

data Player = Player { playerName      :: T.Text
                     , playerId        :: Int
                     , playerType      :: PlayerType
                     , playerDirection :: Direction
                     }
    deriving (Show, Eq)

data Coord = Coord Int Int
    deriving (Show, Generic, Eq)

instance Ord Coord where
    (Coord x1 y1) `compare` (Coord x2 y2) =
        (show x1 ++ " -- " ++ show y1) `compare` (show x2 ++ " -- " ++ show y2)

data Space = Space { spacePiece :: Maybe Piece
                   , spaceColor :: Color
                   , spaceCoord :: Coord
                   }
           | Void Coord
    deriving (Show, Generic, Eq)

instance Ord Space where
    (Space{spaceCoord = c1}) `compare` (Space{spaceCoord = c2}) =
        c1 `compare` c2

data Board = Board { spacesMap :: Map.Map Coord Space }
    deriving (Show, Generic, Eq)

data Move = Move { movePieceId        :: PieceId
                 , moveSpace          :: Space
                 , moveIsConsumable   :: Bool
                 }
    deriving (Show, Eq)

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
    buildBoard width height $
        map spaceBuilder
            [ Coord i j
            | i <- [0 .. (width - 1)]
            , j <- [0 .. (height - 1)] ]

{- Builder function -}
buildBoard :: Int -> Int -> [Space] -> Board
buildBoard r c sps = Board { spacesMap = buildSpaceMap sps }

buildSpaceMap :: [Space] -> Map.Map Coord Space
buildSpaceMap sps = foldl (\m s -> Map.insert (spaceCoord s) s m) Map.empty sps

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
    human1 = Player { playerName = T.pack "Kyle"
                    , playerId = 1
                    , playerType = Human
                    , playerDirection = North
                    }
    ai1 = Player { playerName = T.pack "Bot 1"
                 , playerId = 2
                 , playerType = Computer
                 , playerDirection = South
                 }

{- default space builder -}
defaultSpaceBuilder :: Coord -> Space
defaultSpaceBuilder = \c@(Coord x y) -> buildSpace x y $ calcSpaceColor $ c

{- fetch space with given coordinates -}
fetchSpace :: Board -> Coord -> Maybe Space
fetchSpace (Board{spacesMap = m}) c = Map.lookup c m

{- alternates piece color -}
calcSpaceColor :: Coord -> Color
calcSpaceColor (Coord x y) =
    if (even (x + y)) then White else Black

{- space builder -}
buildSpace :: Int -> Int -> Color -> Space
buildSpace x y c = Space { spacePiece = Nothing
                         , spaceColor = c
                         , spaceCoord = Coord x y
                         }

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

{- Remove a piece from a given space.  -}
removePiece :: Space -> Space
removePiece srem = srem { spacePiece = Nothing }

{- Add a piece to a given space.  -}
addPiece :: Space -> Piece -> Space
addPiece sadd padd = sadd { spacePiece = Just padd }

{- Add a piece to board -}
addPieceToBoard :: Board -> Piece -> Coord -> Maybe Board
addPieceToBoard b@Board{spacesMap = spsMap} p c =
    addPiece' $ Map.lookup c spsMap
  where
    addPiece' :: Maybe Space -> Maybe Board
    addPiece' Nothing = Nothing
    addPiece' (Just s) = Just $
        b { spacesMap = Map.insert c spaceWPiece spsMap }
            where spaceWPiece :: Space
                  spaceWPiece = addPiece s p{pieceOrigin = Just c, pieceMoved = False}

{- Increment diagonal -}
rotateRight :: (Bounded a, Enum a, Eq a) => a -> a 
rotateRight d
  | d == maxBound = minBound
  | otherwise = succ d

rotateLeft :: (Bounded a, Enum a, Eq a) => a -> a 
rotateLeft d
  | d == minBound = maxBound
  | otherwise = pred d

{- Fetches space containing specified piece. -}
fetchPieceSpace :: Board -> Piece -> Maybe Space
fetchPieceSpace (Board {spacesMap = spsMap}) p =
  fetchSpace' $
        Map.toList $ Map.filter (\s -> evalPiece $ spacePiece s) spsMap
      where
        fetchSpace' :: [(Coord, Space)] -> Maybe Space
        fetchSpace' [] = Nothing
        fetchSpace' [ (_, spc) ] = Just spc
        fetchSpace' ((_, spc) : sps) = Nothing

        evalPiece :: Maybe Piece -> Bool
        evalPiece Nothing = False
        evalPiece (Just p') = p' == p

{- Convenience builder for Move -}
buildMove :: Piece -> Board -> Coord -> Bool -> Move
buildMove p b c offensive = Move { movePieceId = pieceId p
                                 , moveSpace = DM.fromJust $ fetchSpace b c
                                 , moveIsConsumable = offensive
                                 }
