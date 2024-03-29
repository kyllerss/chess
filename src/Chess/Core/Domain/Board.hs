{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

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
    deriving (Show, Generic, Eq, Read, NFData)

instance ToJSON Board where
    toJSON Board{spacesMap = spMap} =
        toJSON $ encode . toColor . shape . sortSpaces $ foldr (:) [] spMap
      where
        multimap :: (a -> b) -> [[a]] -> [[b]]
        multimap f = map (map f)

        -- group by row
        shape :: [Space] -> [[Space]]
        shape = DL.groupBy (\ a b -> extractRow a == extractRow b)
                              
          where
            extractRow :: Space -> Int
            extractRow (Void (Coord row _)) = row
            extractRow Space{spaceCoord = Coord row _} = row

        encode :: [[Color]] -> [[Value]]
        encode = multimap toJSON

        toColor :: [[Space]] -> [[Color]]
        toColor = multimap func
          where func :: Space -> Color
                func (Void _) = None
                func s = spaceColor s

        sortSpaces :: [Space] -> [Space]
        sortSpaces = DL.sortBy orderByColumnAndRow

        orderByColumnAndRow :: Space -> Space -> Ordering
        orderByColumnAndRow a b =
            case (a, b) of
                (Void (Coord a1 b1), Void (Coord a2 b2))
                    | a1 > a2 -> GT
                    | a2 > a1 -> LT
                    | a1 == a2 -> if b1 > b2 then GT else LT
                    | otherwise -> EQ
                (Void (Coord a1 b1), Space{spaceCoord = (Coord a2 b2)})
                    | a1 > a2 -> GT
                    | a2 > a1 -> LT
                    | a1 == a2 -> if b1 > b2 then GT else LT
                    | otherwise -> EQ
                (Space{spaceCoord = (Coord a1 b1)}, Void (Coord a2 b2))
                    | a1 > a2 -> GT
                    | a2 > a1 -> LT
                    | a1 == a2 -> if b1 > b2 then GT else LT
                    | otherwise -> EQ
                (Space{spaceCoord = (Coord a1 b1)}, Space{spaceCoord = (Coord a2 b2)})
                    | a1 > a2 -> GT
                    | a2 > a1 -> LT
                    | a1 == a2 -> if b1 > b2 then GT else LT
                    | otherwise -> EQ

{- Represents empty board. -}
nullBoard :: Board
nullBoard = Board{ spacesMap = Map.empty, boardMoves = [] }

{- Generate a new board.  -}
initBoard :: Int -> Int -> (Coord -> Space) -> Board
initBoard width height spaceBuilder =
    buildBoard width height $
        map spaceBuilder [ Coord j i | i <- [0 .. (width - 1)], j <- [0 .. (height - 1)] ]

{- Builder function -}
buildBoard :: Int -> Int -> [Space] -> Board
buildBoard _ _ sps = Board { spacesMap = buildSpaceMap sps
                           , boardMoves = []}

{- Replaces the existing space in the board. -}
replaceSpace :: Space -> Board -> Maybe Board
replaceSpace s b@Board{spacesMap = spsMap}
  | Map.member coord spsMap = Just b{spacesMap = Map.insert coord s spsMap}
  | otherwise = Nothing
    where coord = spaceCoord s

updateBoardSpaceSideEffect :: Coord -> SpaceSideEffectType -> Board -> Maybe Board
updateBoardSpaceSideEffect c se b@Board{spacesMap = sps} = updateBoard (fetchSpace c b)
  where
    updateBoard :: Maybe Space -> Maybe Board
    updateBoard Nothing = Nothing
    updateBoard (Just space) = Just b{spacesMap = Map.insert c updatedSpace sps}
      where
        updatedSpace :: Space
        updatedSpace = space{spaceSideEffectType = Just se}

{- Record history of board moves.  -}
recordBoardMove :: PieceId -> Coord  -> Board -> Maybe Board
recordBoardMove pId coord b = Just b { boardMoves = (pId, coord) : boardMoves b }

{- fetch space with given coordinatesx -}
fetchSpace :: Coord -> Board -> Maybe Space
fetchSpace c Board{spacesMap = m} = Map.lookup c m

{- fetch piece at given coordinate -}
fetchPiece :: Coord -> Board -> Maybe Piece
fetchPiece c b = func $ fetchSpace c b
  where func :: Maybe Space -> Maybe Piece
        func Nothing = Nothing
        func (Just (Void _)) = Nothing
        func (Just Space{spacePiece = sp}) = sp

{- Returns all pieces of a given type for the specified player. -}
fetchPiecesByType :: Player -> PieceType -> Board -> [Piece]
fetchPiecesByType pl pt b = DL.filter (\Piece{pieceType = pt'} -> pt == pt') $ fetchPiecesByPlayer pl b

{- Returns all pieces by a player. -}
fetchPiecesByPlayer :: Player -> Board -> [Piece]
fetchPiecesByPlayer pl Board{spacesMap = spsMap} = Map.foldl' (\acc s -> accumulate s ++ acc) [] spsMap
  where
    accumulate :: Space -> [Piece]
    accumulate (Void _) = []
    accumulate Space{spacePiece = Nothing} = []
    accumulate Space{spacePiece = Just p@Piece{piecePlayer = pl'}}
      | pl' == pl = [p]
      | otherwise = []

{- fetch piece by given pieceId -}
fetchPieceById :: PieceId -> Board -> Maybe Piece
fetchPieceById pId Board {spacesMap = spsMap} =
  Map.foldr' acc Nothing spsMap
      where acc :: Space -> Maybe Piece -> Maybe Piece
            acc (Void _) a = a
            acc v a = let opId = pieceId <$> spacePiece v
                      in if Just pId == opId then spacePiece v else a

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
                  spaceWPiece = addPiece s p{pieceOrigin = Just c}

{- Removes a piece at a given coordinate from specified board. -}
removePieceFromBoard :: Coord -> Board -> Maybe Board
removePieceFromBoard c b@Board{spacesMap = spsMap}
  | DM.isJust space = Just b{spacesMap = updatedMap}
  | otherwise = Nothing

  where
    space :: Maybe Space
    space = fetchSpace c b

    updatedMap :: Map.Map Coord Space
    updatedMap
      | DM.isJust space = Map.insert (spaceCoord $ DM.fromJust space) (removePiece $ DM.fromJust space) spsMap
      | otherwise = spsMap

{- Fetches space containing specified piece. -}
fetchPieceSpace :: Piece -> Board -> Maybe Space
fetchPieceSpace p Board {spacesMap = spsMap} =
  fetchSpace' $
        Map.toList $ Map.filter spacePred spsMap
      where
        spacePred :: Space -> Bool
        spacePred (Void _) = False
        spacePred s = evalPiece $ spacePiece s

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
      emptyBoard = Just (initBoard 8 8 defaultSpaceBuilder) >>=
                   updateBoardSpaceSideEffect (Coord 0 0) PawnPromotion >>=
                   updateBoardSpaceSideEffect (Coord 0 1) PawnPromotion >>=
                   updateBoardSpaceSideEffect (Coord 0 2) PawnPromotion >>=
                   updateBoardSpaceSideEffect (Coord 0 3) PawnPromotion >>=
                   updateBoardSpaceSideEffect (Coord 0 4) PawnPromotion >>=
                   updateBoardSpaceSideEffect (Coord 0 5) PawnPromotion >>=
                   updateBoardSpaceSideEffect (Coord 0 6) PawnPromotion >>=
                   updateBoardSpaceSideEffect (Coord 0 7) PawnPromotion >>=
                   updateBoardSpaceSideEffect (Coord 7 0) PawnPromotion >>=
                   updateBoardSpaceSideEffect (Coord 7 1) PawnPromotion >>=
                   updateBoardSpaceSideEffect (Coord 7 2) PawnPromotion >>=
                   updateBoardSpaceSideEffect (Coord 7 3) PawnPromotion >>=
                   updateBoardSpaceSideEffect (Coord 7 4) PawnPromotion >>=
                   updateBoardSpaceSideEffect (Coord 7 5) PawnPromotion >>=
                   updateBoardSpaceSideEffect (Coord 7 6) PawnPromotion >>=
                   updateBoardSpaceSideEffect (Coord 7 7) PawnPromotion

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
      board = emptyBoard >>=
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
