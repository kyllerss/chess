module Chess.Core.Domain where

import Import
import qualified Data.List      as DL
import qualified Data.Text.Lazy as T
import qualified Data.Map       as Map
import qualified Data.Maybe     as DM

data Color = Black | White
    deriving (Show, Eq)

instance ToJSON Color where
    toJSON Black = toJSON ("b" :: Text)
    toJSON White = toJSON ("w" :: Text)

data PieceType = Pawn | Rook | Knight | Bishop | King | Queen
    deriving (Show, Eq)

instance ToJSON PieceType where
    toJSON Pawn = toJSON ("p" :: Text)
    toJSON Rook = toJSON ("r" :: Text)
    toJSON Knight = toJSON ("n" :: Text)
    toJSON Bishop = toJSON ("b" :: Text)
    toJSON King = toJSON ("k" :: Text)
    toJSON Queen = toJSON ("q" :: Text)

data PieceId = PieceId {pieceIdValue :: Int}
    deriving (Show, Generic, Eq)

instance ToJSON PieceId where
    toJSON (PieceId indx) = toJSON (indx :: Int)

data Piece = Piece { pieceColor   :: Color
                   , pieceType    :: PieceType
                   , piecePlayer  :: Player
                   , pieceId      :: PieceId
                   , pieceOrigin  :: Maybe Coord
                   , pieceMoved   :: Bool
                   }
    deriving (Show, Generic)

instance ToJSON Piece

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

instance ToJSON Player where
    toJSON (Player{playerName = pName,playerId = pId}) =
        object [ "name" .= pName, "id" .= pId ]

data Coord = Coord Int Int
    deriving (Show, Generic, Eq)

instance ToJSON Coord where
    toJSON (Coord x y) = toJSON $ [ x, y ]

instance Ord Coord where
    (Coord x1 y1) `compare` (Coord x2 y2) =
        (show x1 ++ " -- " ++ show y1) `compare` (show x2 ++ " -- " ++ show y2)

data Space = Space { spacePiece :: Maybe Piece
                   , spaceColor :: Color
                   , spaceCoord :: Coord
                   }
           | Void Coord
    deriving (Show, Generic, Eq)

instance ToJSON Space

instance Ord Space where
    (Space{spaceCoord = c1}) `compare` (Space{spaceCoord = c2}) =
        c1 `compare` c2
    (Space{spaceCoord = c1}) `compare` (Void c2) = c1 `compare` c2
    (Void c1) `compare` (Space{spaceCoord = c2}) = c1 `compare` c2
    (Void c1) `compare` (Void c2) = c1 `compare` c2

data Board = Board { spacesMap  :: Map.Map Coord Space
                   , boardMoves :: [(PieceId, Coord)]
                   }
    deriving (Show, Generic, Eq)

instance ToJSON Board where
    toJSON (Board{spacesMap = spMap}) =
        toJSON $ encode . toColor . shape . sortSpaces $ foldr (:) [] spMap
      where
        multimap :: (a -> b) -> [[a]] -> [[b]]
        multimap f ss = map (\xs -> map f xs) ss

        -- group by row
        shape :: [Space] -> [[Space]]
        shape vs = DL.groupBy (\a -> (\b -> (extractRow a) == (extractRow b)))
                              vs
          where
            extractRow :: Space -> Int
            extractRow (Void (Coord row _)) = row 
            extractRow (Space{spaceCoord = Coord row _}) = row

        encode :: [[Color]] -> [[Value]]
        encode sp = multimap (\x -> toJSON x) sp

        toColor :: [[Space]] -> [[Color]]
        toColor sp = multimap (\s -> spaceColor (s :: Space)) sp

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
                (Void (Coord a1 b1), Space{spaceCoord = (Coord a2 b2)})
                    | a1 > a2 -> GT
                    | a2 > a1 -> LT
                    | a1 == a2 -> if (b1 > b2) then GT else LT
                    | otherwise -> EQ
                (Space{spaceCoord = (Coord a1 b1)}, Void (Coord a2 b2))
                    | a1 > a2 -> GT
                    | a2 > a1 -> LT
                    | a1 == a2 -> if (b1 > b2) then GT else LT
                    | otherwise -> EQ
                (Space{spaceCoord = (Coord a1 b1)}, Space{spaceCoord = (Coord a2 b2)})
                    | a1 > a2 -> GT
                    | a2 > a1 -> LT
                    | a1 == a2 -> if (b1 > b2) then GT else LT
                    | otherwise -> EQ

data Move = Move { movePieceId        :: PieceId
                 , moveSpace          :: Space
                 , moveIsConsumable   :: Bool
                 , moveSideEffects    :: [Move]
                 }
    deriving (Show, Eq)

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
        renderPieces :: Board -> Map Text Value
        renderPieces Board{spacesMap = sp} =
            foldl' appendPiece Map.empty sp

        appendPiece :: Map Text Value -> Space -> Map Text Value
        appendPiece m (Void _) = m
        appendPiece m (Space{spacePiece = Nothing}) = m
        appendPiece m (Space{spaceCoord = c, spacePiece = Just p}) =
            Map.insert (pack $ show $ pieceIdValue  $ pieceId p) (renderPiece p c) m

        {-
            map (\(Space{coord = c, piece = Just p}) ->
                     object [ (Tpack (show (pieceId p))) .= (renderPiece p c) ])
                sp

        -}
        renderPiece :: Piece -> Coord -> Value
        renderPiece p c = object [ "t" .= (pieceType p)
                                 , "c" .= (pieceColor (p :: Piece))
                                 , "p" .= (playerId $ piecePlayer p)
                                 , "xy" .= c
                                 ]

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
buildBoard _ _ sps = Board { spacesMap = buildSpaceMap sps
                           , boardMoves = []}
{- Record history of board moves.  -}
recordBoardMove :: PieceId -> Coord  -> Board -> Maybe Board
recordBoardMove pId coord b = Just b { boardMoves = (pId, coord) : boardMoves b } 

buildSpaceMap :: [Space] -> Map.Map Coord Space
buildSpaceMap sps = DL.foldl (\m s -> Map.insert (spaceCoord s) s m) Map.empty sps

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
fetchSpace :: Coord -> Board -> Maybe Space
fetchSpace c Board{spacesMap = m} = Map.lookup c m

{- fetch piece at given coordinate -}
fetchPiece :: Coord -> Board -> Maybe Piece
fetchPiece c b = maybe Nothing (\s -> spacePiece s) $ fetchSpace c b

{- fetch piece by given pieceId -}
fetchPieceById :: PieceId -> Board -> Maybe Piece
fetchPieceById pId Board {spacesMap = spsMap} =
  Map.foldr' (\v a -> let opId = pieceId <$> (spacePiece v)
                      in if (Just pId) == opId then spacePiece v else a
             )
             Nothing
             spsMap 
  
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
addPieceToBoard :: Piece -> Coord -> Board -> Maybe Board
addPieceToBoard p c b@Board{spacesMap = spsMap} =
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
fetchPieceSpace :: Piece -> Board -> Maybe Space
fetchPieceSpace p Board {spacesMap = spsMap} =
  fetchSpace' $
        Map.toList $ Map.filter (\s -> evalPiece $ spacePiece s) spsMap
      where
        fetchSpace' :: [(Coord, Space)] -> Maybe Space
        fetchSpace' [] = Nothing
        fetchSpace' [ (_, spc) ] = Just spc
        fetchSpace' ((_, _) : _) = error "Piece was found more than once on board!"

        evalPiece :: Maybe Piece -> Bool
        evalPiece Nothing = False
        evalPiece (Just p') = p' == p

{- Convenience builder for Move -}
buildMove :: Piece -> Board -> Coord -> Bool -> [Move] -> Move
buildMove p b c offensive sideEffects =
  Move { movePieceId = pieceId p
       , moveSpace = DM.fromJust $ fetchSpace c b
       , moveIsConsumable = offensive
       , moveSideEffects = sideEffects
       }

{- Builds standard board. -}
initStandardBoard :: Player -> Player -> Board
initStandardBoard player1 player2 = DM.fromJust board
  where 
      emptyBoard = initBoard 9 9 defaultSpaceBuilder
      
      pawnA1 = buildPiece (buildPieceId (Coord 1 0)) Pawn Black player2 (Just (Coord 1 0))
      pawnA2 = buildPiece (buildPieceId (Coord 1 1)) Pawn Black player2 (Just (Coord 1 1))
      pawnA3 = buildPiece (buildPieceId (Coord 1 2)) Pawn Black player2 (Just (Coord 1 2))
      pawnA4 = buildPiece (buildPieceId (Coord 1 3)) Pawn Black player2 (Just (Coord 1 3))
      pawnA5 = buildPiece (buildPieceId (Coord 1 4)) Pawn Black player2 (Just (Coord 1 4))
      pawnA6 = buildPiece (buildPieceId (Coord 1 5)) Pawn Black player2 (Just (Coord 1 5))
      pawnA7 = buildPiece (buildPieceId (Coord 1 6)) Pawn Black player2 (Just (Coord 1 6))
      pawnA8 = buildPiece (buildPieceId (Coord 1 7)) Pawn Black player2 (Just (Coord 1 7))
      rookA1 = buildPiece (buildPieceId (Coord 0 0)) Rook Black player2 (Just (Coord 0 0))
      rookA2 = buildPiece (buildPieceId (Coord 0 7)) Rook Black player2 (Just (Coord 0 7))
      knightA1 = buildPiece (buildPieceId (Coord 0 1)) Knight Black player2 (Just (Coord 0 1))
      knightA2 = buildPiece (buildPieceId (Coord 0 6)) Knight Black player2 (Just (Coord 0 6))
      bishopA1 = buildPiece (buildPieceId (Coord 0 2)) Bishop Black player2 (Just (Coord 0 2))
      bishopA2 = buildPiece (buildPieceId (Coord 0 5)) Bishop Black player2 (Just (Coord 0 5))
      queenA = buildPiece (buildPieceId (Coord 0 3)) Queen Black player2 (Just (Coord 0 3))
      kingA = buildPiece (buildPieceId (Coord 0 4)) King Black player2 (Just (Coord 0 4))
      
      pawnB1 = buildPiece (buildPieceId (Coord 6 0)) Pawn Black player1 (Just (Coord 6 0))
      pawnB2 = buildPiece (buildPieceId (Coord 6 1)) Pawn Black player1 (Just (Coord 6 1))
      pawnB3 = buildPiece (buildPieceId (Coord 6 2)) Pawn Black player1 (Just (Coord 6 2))
      pawnB4 = buildPiece (buildPieceId (Coord 6 3)) Pawn Black player1 (Just (Coord 6 3))
      pawnB5 = buildPiece (buildPieceId (Coord 6 4)) Pawn Black player1 (Just (Coord 6 4))
      pawnB6 = buildPiece (buildPieceId (Coord 6 5)) Pawn Black player1 (Just (Coord 6 5))
      pawnB7 = buildPiece (buildPieceId (Coord 6 6)) Pawn Black player1 (Just (Coord 6 6))
      pawnB8 = buildPiece (buildPieceId (Coord 6 7)) Pawn Black player1 (Just (Coord 6 7))
      rookB1 = buildPiece (buildPieceId (Coord 7 0)) Rook Black player1 (Just (Coord 7 0))
      rookB2 = buildPiece (buildPieceId (Coord 7 7)) Rook Black player1 (Just (Coord 7 7))
      knightB1 = buildPiece (buildPieceId (Coord 7 1)) Knight Black player1 (Just (Coord 7 1))
      knightB2 = buildPiece (buildPieceId (Coord 7 6)) Knight Black player1 (Just (Coord 7 6))
      bishopB1 = buildPiece (buildPieceId (Coord 7 2)) Bishop Black player1 (Just (Coord 7 2))
      bishopB2 = buildPiece (buildPieceId (Coord 7 5)) Bishop Black player1 (Just (Coord 7 5))
      queenB = buildPiece (buildPieceId (Coord 7 3)) Queen Black player1 (Just (Coord 7 3))
      kingB = buildPiece (buildPieceId (Coord 7 4)) King Black player1 (Just (Coord 7 4))

      board :: Maybe Board
      board = Just emptyBoard >>=
              addPieceToBoard pawnA1 (Coord 1 0) >>=
              addPieceToBoard pawnA2 (Coord 1 1) >>=
              addPieceToBoard pawnA3 (Coord 1 2) >>=
              addPieceToBoard pawnA4 (Coord 1 3) >>=
              addPieceToBoard pawnA5 (Coord 1 4) >>=
              addPieceToBoard pawnA6 (Coord 1 5) >>=
              addPieceToBoard pawnA7 (Coord 1 6) >>=
              addPieceToBoard pawnA8 (Coord 1 7) >>=
              addPieceToBoard rookA1 (Coord 0 0) >>=
              addPieceToBoard rookA2 (Coord 0 7) >>=
              addPieceToBoard knightA1 (Coord 0 1) >>=
              addPieceToBoard knightA2 (Coord 0 6) >>=
              addPieceToBoard bishopA1 (Coord 0 2) >>=
              addPieceToBoard bishopA2 (Coord 0 5) >>=
              addPieceToBoard queenA (Coord 0 3) >>=
              addPieceToBoard kingA (Coord 0 4) >>=
              addPieceToBoard pawnB1 (Coord 6 0) >>=
              addPieceToBoard pawnB2 (Coord 6 1) >>=
              addPieceToBoard pawnB3 (Coord 6 2) >>=
              addPieceToBoard pawnB4 (Coord 6 3) >>=
              addPieceToBoard pawnB5 (Coord 6 4) >>=
              addPieceToBoard pawnB6 (Coord 6 5) >>=
              addPieceToBoard pawnB7 (Coord 6 6) >>=
              addPieceToBoard pawnB8 (Coord 6 7) >>=
              addPieceToBoard rookB1 (Coord 7 0) >>=
              addPieceToBoard rookB2 (Coord 7 7) >>=
              addPieceToBoard knightB1 (Coord 7 1) >>=
              addPieceToBoard knightB2 (Coord 7 6) >>=
              addPieceToBoard bishopB1 (Coord 7 2) >>=
              addPieceToBoard bishopB2 (Coord 7 5) >>=
              addPieceToBoard queenB (Coord 7 3) >>=
              addPieceToBoard kingB (Coord 7 4)
