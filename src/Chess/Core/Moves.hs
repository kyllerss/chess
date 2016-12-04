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
move b@Board{spacesMap = spsMap} p c =
  if targetCoordValid originSpace 
  then move' originSpace destSpace
  else  Nothing
  
  where
    targetCoordValid :: Maybe Space -> Bool
    targetCoordValid Nothing = False
    targetCoordValid (Just s) = c `elem` (validCoords b p (spaceCoord s))

    originSpace :: Maybe Space
    originSpace = fetchSpace' $
        M.toList $ M.filter (\s -> evalPiece $ spacePiece s) spsMap
      where
        fetchSpace' :: [(Coord, Space)] -> Maybe Space
        fetchSpace' [] = Nothing
        fetchSpace' [ (coord, spc) ] =
            Just spc

        evalPiece :: Maybe Piece -> Bool
        evalPiece Nothing = False
        evalPiece (Just p') = p' == p

    destSpace :: Maybe Space
    destSpace = M.lookup c spsMap

    move' :: Maybe Space -> Maybe Space -> Maybe Board
    move' Nothing _ = Nothing
    move' _ Nothing = Nothing
    move' (Just os) (Just ds) =
        Just $ b { spacesMap = updatedMap }
      where
        updatedMap :: M.Map Coord Space
        updatedMap = M.insert (spaceCoord ds) (addPiece ds p) $
            M.insert (spaceCoord os) (removePiece os) spsMap

{- Returns all valid moves for a given piece. -}
validMoves :: Board -> Piece -> Coord -> [Move]
validMoves b@Board {spacesMap = spsMap}
           p@Piece{pieceId = pId}
           originCoord =
  map (\coord -> Move{ movePieceId = pId, moveSpace = DM.fromJust $ M.lookup coord spsMap})
      $ validCoords b p originCoord

{- Returns coordinates for all valid moves. -}
validCoords :: Board -> Piece -> Coord -> [Coord]
validCoords b p c = 
  (candidateCoords p c b North)
    ++ (candidateCoords p c b NorthEast)
        ++ (candidateCoords p c b East)
            ++ (candidateCoords p c b SouthEast)
                ++ (candidateCoords p c b South)
                    ++ (candidateCoords p c b SouthWest)
                        ++ (candidateCoords p c b West)
                            ++ (candidateCoords p c b NorthWest)

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

-- king
candidateCoords p@Piece{pieceType = King, piecePlayer = pp} c b@Board{spacesMap = spsMap} d
  | canOccupy pp (M.lookup (moveD c d 1) spsMap) = [moveD c d 1]
  | otherwise = []

-- pawn
candidateCoords Piece{ pieceType = Pawn
                     , piecePlayer = pp@Player{playerDirection = pd}
                     , pieceId = pId}
                c@(Coord row col)
                Board{spacesMap = spsMap}
                d =
    (pawnMove $ M.lookup (moveD c d 1) spsMap)
        ++ (pawnMove $ M.lookup (moveD c d 2) spsMap)
  where
    pawnMove :: Maybe Space -> [Coord]
    pawnMove Nothing = []
    pawnMove s@(Just Space{spaceCoord = coord})
        | d == pd && canOccupy pp s = [ coord ]
        | otherwise = []

-- knight
candidateCoords Piece{pieceType = Knight, piecePlayer = p} (Coord row col) Board{spacesMap = spsMap} d
    | d == North =
        DL.filter (\c -> canOccupy p $ M.lookup c spsMap)
        $ [ Coord (row + 1) (col + 2)
          , Coord (row + 1) (col - 2)
          , Coord (row + 2) (col + 1)
          , Coord (row + 2) (col - 1)
          , Coord (row - 1) (col + 2)
          , Coord (row - 1) (col - 2)
          , Coord (row - 2) (col + 1)
          , Coord (row - 2) (col - 1)
          ]
    | otherwise = []

-- rook
candidateCoords p@Piece{pieceType = Rook} c b d =
  directionalCandidateCoords' [ North, East, South, West ] p c b d

-- bishop
candidateCoords p@Piece{pieceType = Bishop} c b d =
  directionalCandidateCoords' [ NorthEast, SouthEast, SouthWest, NorthWest ] p c b d

-- queen
candidateCoords p@Piece{pieceType = Queen} c b d =
  directionalCandidateCoords' [ North, NorthEast, East, SouthEast, South, SouthWest, West, NorthWest ] p c b d

{- Pieces that move directionally, returns candidate coordinates  -}
directionalCandidateCoords' ds p@Piece{piecePlayer = cPlayer} c b@Board{spacesMap = spsMap} d 
    | d `notElem` ds = []
    | validSpaceWithOpp = [spaceCoord cSpace]
    | validSpace = spaceCoord cSpace : candidateCoords p nextCoord b d
    | otherwise = []
  where
    validSpace :: Bool
    validSpace = if M.member nextCoord spsMap
                 then canOccupy cPlayer $ M.lookup nextCoord spsMap
                 else False

    validSpaceWithOpp :: Bool
    validSpaceWithOpp = if validSpace && (hasOpponent cPlayer $ M.lookup nextCoord spsMap)
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
hasOpponent cPlayer (Just (Space{spacePiece = Just (Piece{piecePlayer = pp})})) = cPlayer /= pp

{- Returns true if piece can be occupied by given player. -}
canOccupy :: Player -> Maybe Space -> Bool
canOccupy _ Nothing = False
canOccupy _ (Just Space{spacePiece = Nothing}) = True
canOccupy p s = hasOpponent p s

