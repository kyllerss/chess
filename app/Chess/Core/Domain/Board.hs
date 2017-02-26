module Chess.Core.Domain.Board where

import Import
import Chess.Core.Domain.Base
import Chess.Core.Domain.Coord
import Chess.Core.Domain.Piece
import Chess.Core.Domain.Player
import Chess.Core.Domain.Space
import qualified Data.Map as Map
import qualified Data.List as DL
import qualified Data.Maybe as DM

data Board = Board { spacesMap  :: Map.Map Coord Space
                   , boardMoves :: [(PieceId, Coord)]
                   }
    deriving (Show, Generic, Eq, Read)

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
                    | a1 == a2 -> if (b1 > b2) then GT else LT                    | otherwise -> EQ

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

{- Builds standard board. -}
initStandardBoard :: Player -> Player -> Board
initStandardBoard player1 player2 = DM.fromJust board
  where 
      emptyBoard = initBoard 8 8 defaultSpaceBuilder
      
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
      
      pawnB1 = buildPiece (buildPieceId (Coord 6 0)) Pawn White player1 (Just (Coord 6 0))
      pawnB2 = buildPiece (buildPieceId (Coord 6 1)) Pawn White player1 (Just (Coord 6 1))
      pawnB3 = buildPiece (buildPieceId (Coord 6 2)) Pawn White player1 (Just (Coord 6 2))
      pawnB4 = buildPiece (buildPieceId (Coord 6 3)) Pawn White player1 (Just (Coord 6 3))
      pawnB5 = buildPiece (buildPieceId (Coord 6 4)) Pawn White player1 (Just (Coord 6 4))
      pawnB6 = buildPiece (buildPieceId (Coord 6 5)) Pawn White player1 (Just (Coord 6 5))
      pawnB7 = buildPiece (buildPieceId (Coord 6 6)) Pawn White player1 (Just (Coord 6 6))
      pawnB8 = buildPiece (buildPieceId (Coord 6 7)) Pawn White player1 (Just (Coord 6 7))
      rookB1 = buildPiece (buildPieceId (Coord 7 0)) Rook White player1 (Just (Coord 7 0))
      rookB2 = buildPiece (buildPieceId (Coord 7 7)) Rook White player1 (Just (Coord 7 7))
      knightB1 = buildPiece (buildPieceId (Coord 7 1)) Knight White player1 (Just (Coord 7 1))
      knightB2 = buildPiece (buildPieceId (Coord 7 6)) Knight White player1 (Just (Coord 7 6))
      bishopB1 = buildPiece (buildPieceId (Coord 7 2)) Bishop White player1 (Just (Coord 7 2))
      bishopB2 = buildPiece (buildPieceId (Coord 7 5)) Bishop White player1 (Just (Coord 7 5))
      queenB = buildPiece (buildPieceId (Coord 7 3)) Queen White player1 (Just (Coord 7 3))
      kingB = buildPiece (buildPieceId (Coord 7 4)) King White player1 (Just (Coord 7 4))

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
