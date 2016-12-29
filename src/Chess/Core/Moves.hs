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
move b@Board{spacesMap = spsMap} p destCoord
    | targetCoordStandard originSpace = transfer b p originSpace destSpace
    | targetCoordSpecial originSpace = moveSpecial $ transfer b p originSpace destSpace
    | otherwise = Nothing

  where

    moveSpecial :: Maybe Board -> Maybe Board
    moveSpecial newB =
      foldl (\accB m ->
                let collateralPiece = DM.fromJust (fetchPieceById (DM.fromJust accB) (movePieceId m))
                    collateralDestSpace = Just (moveSpace m)
                    collateralOriginSpace = fetchPieceSpace (DM.fromJust accB) collateralPiece 
                in
                  transfer (DM.fromJust accB) collateralPiece collateralOriginSpace collateralDestSpace
            )
            newB
            (moveSideEffects $ targetSpecialMove)

    targetCoordStandard :: Maybe Space -> Bool
    targetCoordStandard Nothing = False
    targetCoordStandard (Just s) = movesChecker s destCoord standardMoves

    targetCoordSpecial :: Maybe Space -> Bool
    targetCoordSpecial Nothing = False
    targetCoordSpecial (Just s) = movesChecker s destCoord specialMoves

    standardMoves, specialMoves :: [Move]
    standardMoves = validStandardMoves b p (spaceCoord $ DM.fromJust originSpace)
    specialMoves = validSpecialMoves b p (spaceCoord $ DM.fromJust originSpace)

    movesChecker :: Space -> Coord -> [Move] -> Bool
    movesChecker s destCoord moves =
      destCoord `elem` (map (\m -> spaceCoord . moveSpace $ m) moves)
    
    originSpace :: Maybe Space
    originSpace = fetchPieceSpace b p
    destSpace :: Maybe Space
    destSpace = fetchSpace b destCoord

    targetSpecialMove :: Move
    targetSpecialMove = DM.fromJust $ DL.find (\m -> Just (moveSpace m) == destSpace) specialMoves

{- Moves piece without any validation. -}
transfer :: Board -> Piece -> Maybe Space -> Maybe Space -> Maybe Board
transfer _ _ Nothing _ = Nothing
transfer _ _ _ Nothing = Nothing
transfer b@Board {spacesMap = spsMap} p (Just os) (Just ds) =
        Just $ b { spacesMap = updatedMap }
      where
        updatedMap :: M.Map Coord Space
        updatedMap = M.insert (spaceCoord ds)
                              (addPiece ds p{pieceMoved = True}) $
            M.insert (spaceCoord os) (removePiece os) spsMap

{- Returns all valid moves for a given piece. -}
validMoves :: Board -> Piece -> Coord -> [Move]
validMoves b@Board{spacesMap = spsMap} p@Piece{pieceId = pId} originCoord =
  validStandardMoves b p originCoord ++ validSpecialMoves b p originCoord 
  
validStandardMoves :: Board -> Piece -> Coord -> [Move]
validStandardMoves b@Board{spacesMap = spsMap} p@Piece{pieceId = pId} originCoord =
      (DL.foldl' (++) [] $ map (\d -> candidateMoves p originCoord b d) [minBound .. maxBound])

validSpecialMoves :: Board -> Piece -> Coord -> [Move]
validSpecialMoves b@Board{spacesMap = spsMap} p@Piece{pieceId = pId} originCoord =
    (DL.foldl' (++) [] $ map (\d -> specialCandidateMoves p originCoord b d) [minBound .. maxBound])
    
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

threatenedSpaces :: Maybe Board -> Player -> [Space]
threatenedSpaces Nothing _ = []
threatenedSpaces b player = map (\mv -> moveSpace mv) $ -- extract spaces
                   filter (\mv -> moveIsConsumable mv) $ -- remove offensive moves
                   DL.foldl' (++) [] $ -- join list of valid coords
                   map (\(op, oc) -> validMoves (DM.fromJust b) op oc) $ -- calc opp valid moves
                   oppCoords b -- fetch opp coords
  where
    oppCoords :: Maybe Board -> [(Piece, Coord)]
    oppCoords Nothing = []
    oppCoords (Just Board{spacesMap = spsMap'}) =
      foldr (\spc acc -> if isOpponent spc
                         then (DM.fromJust (spacePiece spc), spaceCoord spc) : acc
                         else acc)
                      []
                      spsMap'

    isOpponent :: Space -> Bool
    isOpponent s = (DM.isJust $ spacePiece s)
                   && (piecePlayer (DM.fromJust $ spacePiece s) /= player) 


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
    | pieceMoved p == True = forwardOne ++ diagonals -- piece moved
    | pieceMoved p == False = forwardTwo ++ diagonals -- piece not moved
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
        | hasOpponent pp s = [ buildMove p b coord True []]
        | otherwise = []

    pawnForwardMove :: Maybe Space -> [Move]
    pawnForwardMove Nothing = []
    pawnForwardMove s@(Just Space{spaceCoord = coord})
        | canOccupy pp s = [ buildMove p b coord False []]
        | otherwise = []

-- king
{-
    Rules: 1) Pieces cannot have moved.
           2) No pieces in the way.
           3) Cannot castle out of, through, or into check.
-}
candidateMoves p@Piece{pieceType = King,piecePlayer = pp} c b@Board{spacesMap = spsMap} d
    | canOccupy pp (M.lookup nextCoord spsMap) && (isThreatenedSpace == False)
        = [ buildMove p b nextCoord True []]
    | otherwise = []
  where
    nextCoord :: Coord
    nextCoord = moveD c d 1

    threatSpaces :: [Space]
    threatSpaces = threatenedSpaces kingMovedBoardState pp

    isThreatenedSpace :: Bool
    isThreatenedSpace = isOverlapSpace nextCoord threatSpaces 

    kingMovedBoardState :: Maybe Board
    kingMovedBoardState = transfer b p (fetchSpace b c) (fetchSpace b nextCoord)

-- knight
candidateMoves p@Piece{pieceType = Knight,piecePlayer = pp} c@(Coord row col) b@Board{spacesMap = spsMap} d
    | d `notElem` [ North, East, South, West ] = []
    | otherwise = DL.filter (\Move {moveSpace = Space{spaceCoord = c}} ->
                               canOccupy pp $ M.lookup c spsMap) $ fetchDirMoves d c
  where
    fetchDirMoves :: Direction -> Coord -> [Move]
    fetchDirMoves North c =[ buildMove p b (Coord (row - 2) (col + 1)) True []
                           , buildMove p b (Coord (row - 2) (col - 1)) True []
                           ]
    fetchDirMoves East c = [ buildMove p b (Coord (row - 1) (col + 2)) True []
                            , buildMove p b (Coord (row + 1) (col + 2)) True []
                           ]
    fetchDirMoves South c = [ buildMove p b (Coord (row + 2) (col - 1)) True []
                            , buildMove p b (Coord (row + 2) (col + 1)) True []
                            ]
    fetchDirMoves West c = [ buildMove p b (Coord (row - 1) (col - 2)) True []
                            , buildMove p b (Coord (row + 1) (col - 2)) True []
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

{- Returns candidate 'special' moves for given piece type. -}
specialCandidateMoves :: Piece -> Coord -> Board -> Direction -> [Move]
specialCandidateMoves Piece{pieceType = Rook} _ _ _ = []
specialCandidateMoves Piece{pieceType = Knight} _ _ _ = []
specialCandidateMoves Piece{pieceType = Bishop} _ _ _ = []
specialCandidateMoves Piece{pieceType = Queen} _ _ _ = []

{- Disqualify self if moved
   Look left 5 spaces for non-moved rook
   Look right 4 spaces for non-moved rook
   Determine which slots threatened-}
specialCandidateMoves p@Piece{pieceType = King, pieceMoved = True} c b d = []
specialCandidateMoves p@Piece{pieceType = King, piecePlayer = Player{playerDirection = pd}} c b d
  | d == cLeftDir = castleDir [moveD c cLeftDir 1
                              , moveD c cLeftDir 2
                              , moveD c cLeftDir 3]
                              (moveD c cLeftDir 4)
                              (moveD c cLeftDir 2)
                              (moveD c cLeftDir 1)
  | d == cRightDir = castleDir [moveD c cRightDir 1
                               , moveD c cRightDir 2]
                               (moveD c cRightDir 3)
                               (moveD c cRightDir 2)
                               (moveD c cRightDir 1)
  | otherwise = []

  where

    cLeftDir :: Direction
    cLeftDir = rotateLeft . rotateLeft $ pd

    cRightDir :: Direction
    cRightDir = rotateRight . rotateRight $ pd

    castleDir :: [Coord] -> Coord -> Coord -> Coord -> [Move]
    castleDir betweenCoords rookCoord kingDestCoord rookDestCoord =
      if (DM.isJust rookMoved)
         && not (DM.fromJust rookMoved)
         && spacesPassable
      then [
            buildMove p
                      b
                      kingDestCoord
                      False
                      [buildMove (DM.fromJust $ fetchPiece b rookCoord) b rookDestCoord False []]
           ]
      else []

      where

        rookMoved :: Maybe Bool
        rookMoved = pieceTypeMoved b rookCoord Rook

        spacesPassable :: Bool
        spacesPassable = DL.all (\coord -> (isValid b coord) && (not $ isObstructed coord)) betweenCoords

        isObstructed :: Coord -> Bool
        isObstructed coord = maybe False (\s -> DM.isJust $ spacePiece s) $ fetchSpace b coord

        {-
           FIXME: Is there a better way? Too many contexts within contexts!
         -}
        pieceTypeMoved :: Board -> Coord -> PieceType -> Maybe Bool
        pieceTypeMoved b pCoord pType =
          if DM.isNothing (matchesPiece $ fetchSpace b pCoord)
          then Nothing
          else Just (movedPiece $ DM.fromJust $ fetchSpace b pCoord)
              where
                matchesPiece :: Maybe Space -> Maybe Bool
                matchesPiece Nothing = Nothing
                matchesPiece (Just Space{spacePiece = Nothing}) = Nothing
                matchesPiece (Just Space{spacePiece = Just Piece{pieceType = pt}}) = Just (pt == pType)

                movedPiece :: Space -> Bool
                movedPiece sp = pieceMoved $ DM.fromJust $ spacePiece sp 

specialCandidateMoves p@Piece{pieceType = Pawn} c b d = []


{- Pieces that move directionally, returns candidate coordinates  -}
directionalCandidateMoves' :: [Direction] -> Piece -> Coord -> Board -> Direction -> [Move]
directionalCandidateMoves' ds p@Piece{piecePlayer = cPlayer} c b@Board{spacesMap = spsMap} d
    | d `notElem` ds = []
    | validSpaceWithOpp = [ buildMove p b (spaceCoord cSpace) True []]
    | validSpace = buildMove p b (spaceCoord cSpace) True [] : candidateMoves p nextCoord b d
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

{- Determines if space exists on board. -}
isValid :: Board -> Coord -> Bool
isValid Board{spacesMap = spsMap} c = M.member c spsMap
