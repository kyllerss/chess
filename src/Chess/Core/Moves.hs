module Chess.Core.Moves
    ( move
    , validMoves
    ) where

import           Chess.Core.Domain
import qualified Data.Map          as M
import qualified Data.List         as DL
import qualified Data.Maybe        as DM
import qualified Debug.Trace       as T

{- Moves piece from one space to the next. If requested move is invalid, returns nothing.  -}
move :: Board -> Piece -> Coord -> Maybe Board
move b@Board{spacesMap = spsMap} p c
    | targetCoordValid originSpace =
          move' originSpace destSpace
    | otherwise = Nothing
  where
    targetCoordValid :: Maybe Space -> Bool
    targetCoordValid Nothing =
        False
    targetCoordValid (Just s) =
        c `elem` (validCoords b p (spaceCoord s))

    originSpace :: Maybe Space
    originSpace = fetchPieceSpace b p
    destSpace :: Maybe Space
    destSpace = M.lookup c spsMap

    move' :: Maybe Space -> Maybe Space -> Maybe Board
    move' Nothing _ = Nothing
    move' _ Nothing = Nothing
    move' (Just os) (Just ds) =
        Just $ b { spacesMap = updatedMap }
      where
        updatedMap :: M.Map Coord Space
        updatedMap = M.insert (spaceCoord ds)
                              (addPiece ds p { pieceMoved = True }) $
            M.insert (spaceCoord os) (removePiece os) spsMap

{- Returns all valid moves for a given piece. -}
validMoves :: Board -> Piece -> Coord -> [Move]
validMoves b@Board{spacesMap = spsMap} p@Piece{pieceId = pId} originCoord =
    map (\coord -> Move { movePieceId = pId
                        , moveSpace = DM.fromJust $ M.lookup coord spsMap
                        })
        $ validCoords b p originCoord

{- Returns coordinates for all valid moves. -}
validCoords :: Board -> Piece -> Coord -> [Coord]
validCoords b p c = DL.foldl' (++) [] $
    map (\d -> candidateCoords p c b d) [minBound .. maxBound]

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
candidateCoords :: Piece -> Coord -> Board -> Direction -> [Coord]
-- pawn
candidateCoords Piece{pieceType = Pawn, piecePlayer = pp@Player{playerDirection = pd},pieceId = pId,pieceMoved = pMoved} c@(Coord row col) Board{spacesMap = spsMap} d
    | d /= pd = []
    | pMoved == True = forwardOne ++ diagonals
    | pMoved == False = forwardTwo ++ diagonals
  where
    forwardOne :: [Coord]
    forwardOne = (pawnForwardMove $ M.lookup (moveD c d 1) spsMap)

    forwardTwo :: [Coord]
    forwardTwo = forwardOne ++ (pawnForwardMove $ M.lookup (moveD c d 2) spsMap)

    diagonals :: [Coord]
    diagonals = (pawnDiagonalMove $ M.lookup (moveD c (rotateLeft d) 1) spsMap)
        ++ (pawnDiagonalMove $ M.lookup (moveD c (rotateRight d) 1) spsMap)

    pawnDiagonalMove :: Maybe Space -> [Coord]
    pawnDiagonalMove Nothing =
        []
    pawnDiagonalMove s@(Just Space{spaceCoord = coord})
        | hasOpponent pp s = [ coord ]
        | otherwise = []

    pawnForwardMove :: Maybe Space -> [Coord]
    pawnForwardMove Nothing =
        []
    pawnForwardMove s@(Just Space{spaceCoord = coord})
        | canOccupy pp s = [ coord ]
        | otherwise = []

-- king
candidateCoords Piece{pieceType = King,piecePlayer = p} c b@Board{spacesMap = spsMap} d
    | canOccupy p (M.lookup nextCoord spsMap) && isThreatenedSpace == False = [ nextCoord ]
    | otherwise = []
  where
    nextCoord :: Coord
    nextCoord = moveD c d 1

    isThreatenedSpace :: Bool
    isThreatenedSpace = isOverlapSpace nextCoord threatenedSpaces 

    threatenedSpaces :: [Space]
    threatenedSpaces = map (\coord -> DM.fromJust $ M.lookup coord spsMap) $ -- extract coords
                           DL.foldl' (++) [] $ -- join list of valid coords
                               map (\(op, oc) -> validCoords b op oc) $ oppCoords b -- calc valid coords

    oppCoords :: Board -> [(Piece, Coord)]
    oppCoords = undefined

-- knight
candidateCoords Piece{pieceType = Knight,piecePlayer = p} c@(Coord row col) Board{spacesMap = spsMap} d
    | d `notElem` [ North, East, South, West ] =
          []
    | otherwise = DL.filter (\c -> canOccupy p $ M.lookup c spsMap) $
          fetchDirCoords d c
  where
    fetchDirCoords :: Direction -> Coord -> [Coord]
    fetchDirCoords North c =
        [ Coord (row - 2) (col + 1), Coord (row - 2) (col - 1) ]
    fetchDirCoords East c = [ Coord (row - 1) (col + 2)
                            , Coord (row + 1) (col + 2)
                            ]
    fetchDirCoords South c =
        [ Coord (row + 2) (col - 1), Coord (row + 2) (col + 1) ]
    fetchDirCoords West c = [ Coord (row - 1) (col - 2)
                            , Coord (row + 1) (col - 2)
                            ]
    fetchDirCoords _ c = []

-- rook
candidateCoords p@Piece{pieceType = Rook} c b d =
    directionalCandidateCoords' [ North, East, South, West ] p c b d

-- bishop
candidateCoords p@Piece{pieceType = Bishop} c b d =
    directionalCandidateCoords' [ NorthEast, SouthEast, SouthWest, NorthWest ]
                                p
                                c
                                b
                                d

-- queen
candidateCoords p@Piece{pieceType = Queen} c b d =
    directionalCandidateCoords' [ North
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
directionalCandidateCoords' ds p@Piece{piecePlayer = cPlayer} c b@Board{spacesMap = spsMap} d
    | d `notElem` ds = []
    | validSpaceWithOpp = [ spaceCoord cSpace ]
    | validSpace = spaceCoord cSpace : candidateCoords p nextCoord b d
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
  
