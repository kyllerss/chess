module Chess.Core.Moves
    ( move
    , validMoves
    , candidateMoves
    ) where

import           Chess.Core.Domain
import qualified Data.Map          as M
import qualified Data.List         as DL
import qualified Data.Maybe        as DM
import qualified Debug.Trace       as T

{- Moves piece from one space to the next. If requested move is invalid, returns nothing.  -}
move :: Board -> Piece -> Coord -> Maybe Board
move b@Board{spacesMap = spsMap} p c
    | targetCoordValid originSpace = transfer b p originSpace destSpace
    | otherwise = Nothing
  where
    targetCoordValid :: Maybe Space -> Bool
    targetCoordValid Nothing = False
    targetCoordValid (Just s) =
        c `elem` (validCoords $ validMoves b p (spaceCoord s))

    originSpace :: Maybe Space
    originSpace = fetchPieceSpace b p
    destSpace :: Maybe Space
    destSpace = M.lookup c spsMap

{- Moves piece without any validation. -}
transfer :: Board -> Piece -> Maybe Space -> Maybe Space -> Maybe Board
transfer _ _ Nothing _ = Nothing
transfer _ _ _ Nothing = Nothing
transfer b@Board {spacesMap = spsMap} p (Just os) (Just ds) =
        Just $ b { spacesMap = updatedMap }
      where
        updatedMap :: M.Map Coord Space
        updatedMap = M.insert (spaceCoord ds)
                              (addPiece ds p) $
            M.insert (spaceCoord os) (removePiece os) spsMap

{- Returns all valid moves for a given piece. -}
validMoves :: Board -> Piece -> Coord -> [Move]
validMoves b@Board{spacesMap = spsMap} p@Piece{pieceId = pId} originCoord =
    DL.foldl' (++) [] $ map (\d -> candidateMoves p originCoord b d) [minBound .. maxBound]
    
{- Returns coordinates for all valid moves. -}
validCoords :: [Move] -> [Coord]
validCoords ms = map (\m -> spaceCoord . moveSpace $ m) ms

{- Utility function for incrementing space based on direction. -}
moveD :: Coord -> Direction -> Int -> Coord
moveD (Coord row col) dir count
    | dir == North     = Coord (row - count) col
    | dir == NorthEast = Coord (row - count) (col + count)
    | dir == East      = Coord row (col + count)
    | dir == SouthEast = Coord (row + count) (col + count)
    | dir == South     = Coord (row + count) col
    | dir == SouthWest = Coord (row + count) (col - count)
    | dir == West      = Coord row (col - count)
    | dir == NorthWest = Coord (row - count) (col - count)

{- Returns candidate moves (legal and illegal) for given piece type. -}
candidateMoves :: Piece -> Coord -> Board -> Direction -> [Move]

-- pawn
candidateMoves p@Piece{ pieceType = Pawn
                      , piecePlayer = pp@Player{ playerDirection = pd }
                      , pieceId = pId
                      }
               c@(Coord row col)
               b@Board{spacesMap = spsMap}
               d
    | d /= pd = []
    | pieceMoved p c == True = forwardOne ++ diagonals -- piece moved
    | pieceMoved p c == False = forwardTwo ++ diagonals -- piece not moved
  where
    forwardOne :: [Move]
    forwardOne = (pawnForwardMove $ M.lookup (moveD c d 1) spsMap)

    forwardTwo :: [Move]
    forwardTwo = forwardOne ++ (pawnForwardMove $ M.lookup (moveD c d 2) spsMap)

    diagonals :: [Move]
    diagonals = (pawnDiagonalMove $ M.lookup (moveD c (rotateLeft d) 1) spsMap)
        ++ (pawnDiagonalMove $ M.lookup (moveD c (rotateRight d) 1) spsMap)

    pawnDiagonalMove :: Maybe Space -> [Move]
    pawnDiagonalMove Nothing = []
    pawnDiagonalMove s@(Just Space{spaceCoord = coord})
        | hasOpponent pp s = [ buildMove p b coord True ]
        | otherwise = []

    pawnForwardMove :: Maybe Space -> [Move]
    pawnForwardMove Nothing = []
    pawnForwardMove s@(Just Space{spaceCoord = coord})
        | canOccupy pp s = [ buildMove p b coord False ]
        | otherwise = []

-- king
{-
    Rules: 1) Pieces cannot have moved.
           2) No pieces in the way.
           3) Cannot castle out of, through, or into check.
-}
candidateMoves p@Piece{pieceType = King,piecePlayer = pp} c b@Board{spacesMap = spsMap} d
    | canOccupy pp (M.lookup nextCoord spsMap) && (isThreatenedSpace == False)
        = [ buildMove p b nextCoord True ]
    | otherwise = []
  where
    nextCoord :: Coord
    nextCoord = moveD c d 1

    isThreatenedSpace :: Bool
    isThreatenedSpace = isOverlapSpace nextCoord threatenedSpaces 

    kingMovedBoardState :: Maybe Board
    kingMovedBoardState = transfer b p (fetchSpace b c) (fetchSpace b nextCoord)

    threatenedSpaces :: [Space]
    threatenedSpaces = map (\mv -> moveSpace mv) $ -- extract spaces
                       filter (\mv -> moveIsConsumable mv) $ -- remove offensive moves
                       DL.foldl' (++) [] $ -- join list of valid coords
                       map (\(op, oc) -> if (DM.isJust kingMovedBoardState)
                                         then validMoves (DM.fromJust kingMovedBoardState) op oc
                                         else []) $ -- calc opp valid moves
                       oppCoords kingMovedBoardState -- fetch opp coords

    oppCoords :: Maybe Board -> [(Piece, Coord)]
    oppCoords Nothing = []
    oppCoords (Just Board{spacesMap = spsMap'}) =
      foldr (\spc acc -> if isOpponent spc pp
                         then (DM.fromJust (spacePiece spc), spaceCoord spc) : acc
                         else acc)
                      []
                      spsMap'

    isOpponent :: Space -> Player -> Bool
    isOpponent s pp = (DM.isJust $ spacePiece s)
                      && (piecePlayer (DM.fromJust $ spacePiece s) /= pp) 

-- knight
candidateMoves p@Piece{pieceType = Knight,piecePlayer = pp} c@(Coord row col) b@Board{spacesMap = spsMap} d
    | d `notElem` [ North, East, South, West ] = []
    | otherwise = DL.filter (\Move {moveSpace = Space{spaceCoord = c}} ->
                               canOccupy pp $ M.lookup c spsMap) $ fetchDirMoves d c
  where
    fetchDirMoves :: Direction -> Coord -> [Move]
    fetchDirMoves North c =[ buildMove p b (Coord (row - 2) (col + 1)) True
                           , buildMove p b (Coord (row - 2) (col - 1)) True
                           ]
    fetchDirMoves East c = [ buildMove p b (Coord (row - 1) (col + 2)) True
                            , buildMove p b (Coord (row + 1) (col + 2)) True
                           ]
    fetchDirMoves South c = [ buildMove p b (Coord (row + 2) (col - 1)) True
                            , buildMove p b (Coord (row + 2) (col + 1)) True
                            ]
    fetchDirMoves West c = [ buildMove p b (Coord (row - 1) (col - 2)) True
                            , buildMove p b (Coord (row + 1) (col - 2)) True
                           ]
    fetchDirMoves _ c = []

-- rook
candidateMoves p@Piece{pieceType = Rook} c b d =
    directionalCandidateMoves' [ North, East, South, West ] p c b d

-- bishop
candidateMoves p@Piece{pieceType = Bishop} c b d =
    directionalCandidateMoves' [ NorthEast, SouthEast, SouthWest, NorthWest ]
                                p
                                c
                                b
                                d

-- queen
candidateMoves p@Piece{pieceType = Queen} c b d =
    directionalCandidateMoves' [ North
                                , NorthEast
                                , East
                                , SouthEast
                                , South
                                , SouthWest
                                , West
                                , NorthWest
                                ]
                                p
                                c
                                b
                                d

{- Pieces that move directionally, returns candidate coordinates  -}
directionalCandidateMoves' :: [Direction] -> Piece -> Coord -> Board -> Direction -> [Move]
directionalCandidateMoves' ds p@Piece{piecePlayer = cPlayer} c b@Board{spacesMap = spsMap} d
    | d `notElem` ds = []
    | validSpaceWithOpp = [ buildMove p b (spaceCoord cSpace) True ]
    | validSpace = buildMove p b (spaceCoord cSpace) True : candidateMoves p nextCoord b d
    | otherwise = []
  where
    validSpace :: Bool
    validSpace = if M.member nextCoord spsMap
                 then canOccupy cPlayer $ M.lookup nextCoord spsMap
                 else False

    validSpaceWithOpp :: Bool
    validSpaceWithOpp = if validSpace &&
                            (hasOpponent cPlayer $ M.lookup nextCoord spsMap)
                        then True
                        else False

    cSpace :: Space
    cSpace = DM.fromJust $ M.lookup nextCoord spsMap

    nextCoord :: Coord
    nextCoord = moveD c d 1

{- Returns true if space has opponent  -}
hasOpponent :: Player -> Maybe Space -> Bool
hasOpponent _ Nothing = False
hasOpponent _ (Just Space{spacePiece = Nothing}) = False
hasOpponent cPlayer (Just (Space{spacePiece = Just (Piece{piecePlayer = pp})})) =
    cPlayer /= pp

{- Returns true if piece can be occupied by given player. -}
canOccupy :: Player -> Maybe Space -> Bool
canOccupy _ Nothing = False
canOccupy _ (Just Space{spacePiece = Nothing}) = True
canOccupy p s = hasOpponent p s

{- Determines if space is threatened by another piece. -}
isOverlapSpace :: Coord -> [Space] -> Bool
isOverlapSpace c [] = False
isOverlapSpace c sps = c `elem` (map (\s -> spaceCoord s) sps)
  
