module Chess.Core.Logic.Moves
    ( move
    , validMoves
    , allValidMoves
    , candidateMoves
    , threatenedSpaces
    , transfer
    , playerInCheck
    ) where

import           Import
import           Chess.Core.Domain.Base
import           Chess.Core.Domain.Coord
import           Chess.Core.Domain.Board
import           Chess.Core.Domain.Move
import           Chess.Core.Domain.Piece
import           Chess.Core.Domain.Player
import           Chess.Core.Domain.Space
import qualified Data.Map          as M
import qualified Data.List         as DL
import           Data.Maybe

{- Determines if player is currently in check. -}
playerInCheck :: Maybe Board -> Player -> Bool
playerInCheck Nothing _ = False
playerInCheck b pp = DL.any (\s -> ((piecePlayer <$> (spacePiece s)) == Just pp)
                                   && ((pieceType <$> (spacePiece s)) == Just King)) $ threatenedSpaces b pp

{- Moves piece from one space to the next. If requested move is invalid, returns nothing.  -}
move :: PieceId -> Coord -> Board -> Maybe Board
move pId c b = moveInner (fetchPieceById pId b) c b

{- Internal implementation of 'move' with piece resolved. -}
moveInner :: Maybe Piece -> Coord -> Board -> Maybe Board
moveInner Nothing _ _ = Nothing
moveInner (Just p@Piece{pieceId = pId, piecePlayer = pp}) destCoord b
    | resultsInCheck = Nothing
    | targetCoordStandard originSpace = movedBoard
    | targetCoordSpecial originSpace = movedBoard >>= applyCollateralMoves
    | otherwise = Nothing

  where

    movedBoard :: Maybe Board
    movedBoard = transfer b p originSpace destSpace >>= recordBoardMove pId destCoord

    resultsInCheck :: Bool
    resultsInCheck = playerInCheck virtBoard pp
      where
        virtBoard :: Maybe Board
        virtBoard = transfer b p originSpace destSpace

    applyCollateralMoves :: Board -> Maybe Board
    applyCollateralMoves newB =
      DL.foldl (\accB m ->
                let collateralPiece = fromJust (fetchPieceById (movePieceId m) (fromJust accB))
                    collateralDestSpace = Just (moveSpace m)
                    collateralOriginSpace = fetchPieceSpace collateralPiece (fromJust accB)
                in
                  transfer (fromJust accB) collateralPiece collateralOriginSpace collateralDestSpace
            )
            (Just newB)
            (moveSideEffects $ targetSpecialMove)

    targetCoordStandard :: Maybe Space -> Bool
    targetCoordStandard Nothing = False
    targetCoordStandard (Just s) = movesChecker s destCoord standardMoves

    targetCoordSpecial :: Maybe Space -> Bool
    targetCoordSpecial Nothing = False
    targetCoordSpecial (Just s) = movesChecker s destCoord specialMoves

    standardMoves, specialMoves :: [Move]
    standardMoves = validStandardMoves b p (spaceCoord $ fromJust originSpace)
    specialMoves = validSpecialMoves b p (spaceCoord $ fromJust originSpace)

    movesChecker :: Space -> Coord -> [Move] -> Bool
    movesChecker _ dc moves = -- TODO: Space not needed, remove from callers
      dc `DL.elem` (DL.map (\m -> spaceCoord . moveSpace $ m) moves)
    
    originSpace :: Maybe Space
    originSpace = fetchPieceSpace p b
    destSpace :: Maybe Space
    destSpace = fetchSpace destCoord b

    targetSpecialMove :: Move
    targetSpecialMove = fromJust $ DL.find (\m -> Just (moveSpace m) == destSpace) specialMoves

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

{- Returns all valid moves for each piece of a given player in a given board -}
allValidMoves :: Maybe Board -> Player -> [Move]
allValidMoves Nothing _ = []
allValidMoves (Just b@Board{spacesMap = spsMap}) pl =
  M.foldl' accumulator [] spsMap
  where
    accumulator :: [Move] -> Space -> [Move]
    accumulator acc Space{spacePiece = Nothing} = acc
    accumulator acc (Void _) = acc
    accumulator acc Space{spaceCoord = sCoord, spacePiece = Just Piece{pieceId = pId, piecePlayer = ppl}}
      | ppl == pl = (validMoves b pId sCoord) ++ acc
      | otherwise = acc

{- Returns all valid moves for a given piece. -}
validMoves :: Board -> PieceId -> Coord -> [Move]
validMoves b pId c = validMovesInner b (fetchPieceById pId b) c

validMovesInner :: Board -> Maybe Piece -> Coord -> [Move]
validMovesInner _ Nothing _ = []
validMovesInner b (Just p) originCoord =
  (validStandardMoves b p originCoord) DL.++ (validSpecialMoves b p originCoord) 
  
validStandardMoves :: Board -> Piece -> Coord -> [Move]
validStandardMoves b p originCoord =
      (DL.foldl' (DL.++) [] $ DL.map (\d -> candidateMoves p originCoord b d) [minBound .. maxBound])

validSpecialMoves :: Board -> Piece -> Coord -> [Move]
validSpecialMoves b p originCoord =
    (DL.foldl' (DL.++) [] $ DL.map (\d -> specialCandidateMoves p originCoord b d) [minBound .. maxBound])
    
{- Utility function for incrementing space based on direction. -}
moveD :: Coord -> Direction -> Int -> Coord
moveD (Coord row col) dir cnt
    | dir == North     = Coord (row - cnt) col
    | dir == NorthEast = Coord (row - cnt) (col + cnt)
    | dir == East      = Coord row (col + cnt)
    | dir == SouthEast = Coord (row + cnt) (col + cnt)
    | dir == South     = Coord (row + cnt) col
    | dir == SouthWest = Coord (row + cnt) (col - cnt)
    | dir == West      = Coord row (col - cnt)
    | dir == NorthWest = Coord (row - cnt) (col - cnt)
    | otherwise = error "Unkown Direction value!" -- TODO: Why is this needed?

{-
     Returns list of spaces that threaten provided player
     (Note: does not simulate what would happen if you moved onto a pawn's diagonal - FIXME).
-}
threatenedSpaces :: Maybe Board -> Player -> [Space]
threatenedSpaces Nothing _ = []
threatenedSpaces b player = DL.map (\mv -> moveSpace mv) $ -- extract spaces
                   filter (\mv -> moveIsConsumable mv) $ -- keep offensive moves
                   DL.foldl' (DL.++) [] $ -- join list of valid coords
                   DL.map (\(op, oc) -> validMoves (fromJust b) (pieceId op) oc) $ -- calc opp valid moves
                   oppCoords b -- fetch opp coords
  where
    oppCoords :: Maybe Board -> [(Piece, Coord)]
    oppCoords Nothing = []
    oppCoords (Just Board{spacesMap = spsMap'}) =
      foldr (\spc acc -> if isOpponent spc
                         then (fromJust (spacePiece spc), spaceCoord spc) : acc
                         else acc)
                      []
                      spsMap'

    isOpponent :: Space -> Bool
    isOpponent s = (isJust $ spacePiece s)
                   && (piecePlayer (fromJust $ spacePiece s) /= player) 


{- Returns candidate moves (legal and illegal) for given piece type. -}
candidateMoves :: Piece -> Coord -> Board -> Direction -> [Move]

-- pawn
candidateMoves p@Piece{ pieceType = Pawn
                      , piecePlayer = pp@Player{ playerDirection = pd }
                      }
               c
               b@Board{spacesMap = spsMap}
               d
    | d /= pd = []
    | pieceMoved p == True = forwardOne DL.++ diagonals -- piece moved
    | pieceMoved p == False = forwardTwo DL.++ diagonals -- piece not moved
    | otherwise = []
  where
    forwardOne :: [Move]
    forwardOne
      | isEmptySpace $ M.lookup (moveD c d 1) spsMap = (pawnForwardMove $ M.lookup (moveD c d 1) spsMap)
      | otherwise = []

    forwardTwo :: [Move]
    forwardTwo
      | forwardOne == [] = []
      | otherwise = forwardOne DL.++ (pawnForwardMove $ M.lookup (moveD c d 2) spsMap)

    diagonals :: [Move]
    diagonals = (pawnDiagonalMove $ M.lookup (moveD c (rotateLeft d) 1) spsMap)
                DL.++ (pawnDiagonalMove $ M.lookup (moveD c (rotateRight d) 1) spsMap)

    pawnDiagonalMove :: Maybe Space -> [Move]
    pawnDiagonalMove Nothing = []
    pawnDiagonalMove (Just (Void(_))) = []
    pawnDiagonalMove s@(Just Space{spaceCoord = coord})
        | hasOpponent pp s = maybeToList $ buildMove p b coord True []
        | otherwise = []

    pawnForwardMove :: Maybe Space -> [Move]
    pawnForwardMove Nothing = []
    pawnForwardMove (Just (Void(_))) = []
    pawnForwardMove s@(Just Space{spaceCoord = coord})
        | canOccupy pp s = maybeToList $ buildMove p b coord False []
        | otherwise = []

-- king
{-
    Rules: 1) Pieces cannot have moved.
           2) No pieces in the way.
           3) Cannot castle out of, through, or into check.
-}
candidateMoves p@Piece{pieceType = King,piecePlayer = pp} c b@Board{spacesMap = spsMap} d
    | canOccupy pp (M.lookup nextCoord spsMap) && (isThreatenedSpace == False)
        = maybeToList $ buildMove p b nextCoord True []
    | otherwise = []
  where
    nextCoord :: Coord
    nextCoord = moveD c d 1

    threatSpaces :: [Space]
    threatSpaces = threatenedSpaces kingMovedBoardState pp

    isThreatenedSpace :: Bool
    isThreatenedSpace = isOverlapSpace nextCoord threatSpaces 

    kingMovedBoardState :: Maybe Board
    kingMovedBoardState = transfer b p (fetchSpace c b) (fetchSpace nextCoord b)

-- knight
candidateMoves p@Piece{pieceType = Knight,piecePlayer = pp} (Coord row col) b@Board{spacesMap = spsMap} d
    | d `DL.notElem` [ North, East, South, West ] = []
    | otherwise = DL.filter (\Move {moveSpace = Space{spaceCoord = crd}} ->
                               canOccupy pp $ M.lookup crd spsMap) $ fetchDirMoves d
  where
    fetchDirMoves :: Direction -> [Move]
    fetchDirMoves North = (maybeToList $ buildMove p b (Coord (row - 2) (col + 1)) True [])
                          ++
                          (maybeToList $ buildMove p b (Coord (row - 2) (col - 1)) True [])
    fetchDirMoves East = (maybeToList $ buildMove p b (Coord (row - 1) (col + 2)) True [])
                         ++
                         (maybeToList $ buildMove p b (Coord (row + 1) (col + 2)) True [])
    fetchDirMoves South = (maybeToList $ buildMove p b (Coord (row + 2) (col - 1)) True [])
                          ++
                          (maybeToList $ buildMove p b (Coord (row + 2) (col + 1)) True [])
    fetchDirMoves West = (maybeToList $ buildMove p b (Coord (row - 1) (col - 2)) True [])
                         ++ 
                         (maybeToList $ buildMove p b (Coord (row + 1) (col - 2)) True [])
    fetchDirMoves _ = []

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

{- King castling moves. -}
specialCandidateMoves Piece{pieceType = King, pieceMoved = True} _ _ _ = []
specialCandidateMoves p@Piece{pieceType = King, piecePlayer = pp@Player{playerDirection = pd}} c b d
  | d == cLeftDir = castleDir [moveD c cLeftDir 1
                              , moveD c cLeftDir 2
                              , moveD c cLeftDir 3]
                              [moveD c cLeftDir 1
                              , moveD c cLeftDir 2]
                              (moveD c cLeftDir 4)
                              (moveD c cLeftDir 2)
                              (moveD c cLeftDir 1)
  | d == cRightDir = castleDir [moveD c cRightDir 1
                               , moveD c cRightDir 2]
                               [moveD c cRightDir 1
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

    castleDir :: [Coord] -> [Coord] -> Coord -> Coord -> Coord -> [Move]
    castleDir betweenCoords kingMoveCoords rookCoord kingDestCoord rookDestCoord =
      if (isJust rookMoved)
         && not (fromJust rookMoved)
         && spacesPassable 
         && not (spacesThreatened kingMoveCoords)
      then maybeToList kingMove
      else []

      where

        kingMove, rookMove :: Maybe Move
        kingMove = buildMove p b kingDestCoord False (maybeToList rookMove)
        rookMove = buildMove (fromJust $ fetchPiece rookCoord b) b rookDestCoord False []
        
        rookMoved :: Maybe Bool
        rookMoved = pieceTypeMoved b rookCoord Rook

        spacesPassable :: Bool
        spacesPassable =
          DL.all (\coord -> (isValid b coord) && (not $ isObstructed coord)) betweenCoords

        isObstructed :: Coord -> Bool
        isObstructed coord = maybe False (\s -> isJust $ spacePiece s) $ fetchSpace coord b

        spacesThreatened :: [Coord] -> Bool
        spacesThreatened kingMCoords =
          DL.any (\crd -> isOverlapSpace crd (threatenedSpaces (virtBoard b pp crd) pp)) kingMCoords
          where
            virtBoard :: Board -> Player -> Coord -> Maybe Board
            virtBoard brd pp' coord = addPieceToBoard (testPiece pp' coord) coord brd

            testPiece :: Player -> Coord -> Piece
            testPiece pp' coord = buildPiece (PieceId (-1)) Pawn White pp' (Just coord) 

        {-
           TODO: Is there a better way? Too many contexts within contexts!
         -}
        pieceTypeMoved :: Board -> Coord -> PieceType -> Maybe Bool
        pieceTypeMoved brd pCoord pType =
          if isNothing (matchesPiece $ fetchSpace pCoord brd)
          then Nothing
          else movedPiece <$> fetchSpace pCoord brd
              where
                matchesPiece :: Maybe Space -> Maybe Bool
                matchesPiece Nothing = Nothing
                matchesPiece (Just (Void(_))) = Nothing
                matchesPiece (Just Space{spacePiece = Nothing}) = Nothing
                matchesPiece (Just Space{spacePiece = Just Piece{pieceType = pt}}) = Just (pt == pType)

                movedPiece :: Space -> Bool
                movedPiece sp = pieceMoved $ fromJust $ spacePiece sp 

specialCandidateMoves p@Piece{pieceType = Pawn, piecePlayer = Player{playerId = plId, playerDirection = pd}} c b d
  | d /= pd = []
  | bothQualify = leftEnPassant DL.++ rightEnPassant
  | leftQualifies = leftEnPassant
  | rightQualifies = rightEnPassant
  | otherwise = []
    where

      bothQualify, leftQualifies, rightQualifies :: Bool
      bothQualify = leftQualifies && rightQualifies 
      leftQualifies = leftIsPawn && leftJumpedOpenning && turnOnLeftNotMissed
      rightQualifies = rightIsPawn && rightJumpedOpenning && turnOnRightNotMissed

      turnOnLeftNotMissed, turnOnRightNotMissed :: Bool
      turnOnLeftNotMissed = length (movesSince leftNeighbour) == 0
      turnOnRightNotMissed = length (movesSince rightNeighbour) == 0

      movesSince :: Maybe Piece -> [(PieceId, Coord)]
      movesSince Nothing = []
      movesSince (Just Piece{pieceId = pId}) =
        filter matchesPlayer $ takeWhile notMatchesPiece (boardMoves b)

        where
          notMatchesPiece :: (PieceId, Coord) -> Bool
          notMatchesPiece (mPieceId, _) = mPieceId /= pId

          matchesPlayer :: (PieceId, Coord) -> Bool
          matchesPlayer (mPieceId, _) = (Just plId) == (playerId <$> piecePlayer <$> (fetchPieceById mPieceId b))
      
      leftIsPawn, rightIsPawn :: Bool
      leftIsPawn = (pieceType <$> leftNeighbour) == Just Pawn
      rightIsPawn = (pieceType <$> rightNeighbour) == Just Pawn

      leftJumpedOpenning, rightJumpedOpenning :: Bool
      leftJumpedOpenning = isJump (playerDirection <$> piecePlayer <$> leftNeighbour)
                                  (join $ pieceOrigin <$> leftNeighbour) leftCoord
      rightJumpedOpenning = isJump (playerDirection <$> piecePlayer <$> rightNeighbour)
                                   (join $ pieceOrigin <$> rightNeighbour) rightCoord

      isJump :: Maybe Direction -> Maybe Coord -> Coord -> Bool
      isJump Nothing _ _ = False
      isJump _ Nothing _ = False
      isJump (Just od) (Just oc) cc = (moveD oc od 2) == cc

      leftNeighbour, rightNeighbour :: Maybe Piece
      leftNeighbour = fetchPiece leftCoord b
      rightNeighbour = fetchPiece rightCoord b

      leftCoord, rightCoord :: Coord
      leftCoord = moveD c (rotateLeft . rotateLeft $ pd) 1
      rightCoord = moveD c (rotateRight . rotateRight $ pd) 1

      leftEnPassantCoord, rightEnPassantCoord :: Coord
      leftEnPassantCoord = moveD leftCoord pd 1
      rightEnPassantCoord = moveD rightCoord pd 1
      
      leftEnPassant, rightEnPassant :: [Move]
      leftEnPassant = maybeToList $ buildMove p b leftEnPassantCoord True []
      rightEnPassant = maybeToList $ buildMove p b rightEnPassantCoord True []

{- Pieces that move directionally, returns candidate coordinates  -}
directionalCandidateMoves' :: [Direction] -> Piece -> Coord -> Board -> Direction -> [Move]
directionalCandidateMoves' ds p@Piece{piecePlayer = cPlayer} c b@Board{spacesMap = spsMap} d
    | d `DL.notElem` ds = []
    | validSpaceWithOpp = maybeToList $ buildMove p b (spaceCoord cSpace) True []
    | validSpace = maybeToList (buildMove p b (spaceCoord cSpace) True []) ++ candidateMoves p nextCoord b d
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
    cSpace = fromJust $ M.lookup nextCoord spsMap

    nextCoord :: Coord
    nextCoord = moveD c d 1

{- Returns true if space has opponent  -}
hasOpponent :: Player -> Maybe Space -> Bool
hasOpponent _ Nothing = False
hasOpponent _ (Just (Void _)) = False
hasOpponent _ (Just Space{spacePiece = Nothing}) = False
hasOpponent cPlayer (Just (Space{spacePiece = Just (Piece{piecePlayer = pp})})) =
    cPlayer /= pp

{- Returns true if space has a teammate. -}
hasTeammate :: Player -> Maybe Space -> Bool
hasTeammate _ Nothing = False
hasTeammate _ (Just (Void _)) = False
hasTeammate _ (Just Space{spacePiece = Nothing}) = True
hasTeammate cPlayer (Just (Space{spacePiece = Just (Piece{piecePlayer = pp})})) =
    cPlayer == pp

{- Returns true if space has no opponent and no teammate. -}
isEmptySpace :: Maybe Space -> Bool
isEmptySpace Nothing = False
isEmptySpace (Just (Void _)) = False
isEmptySpace (Just Space{spacePiece = Nothing}) = True
isEmptySpace (Just Space{spacePiece = Just _}) = False

{- Returns true if piece can be occupied by given player. -}
canOccupy :: Player -> Maybe Space -> Bool
canOccupy _ Nothing = False
canOccupy _ (Just (Void _)) = False
canOccupy _ (Just Space{spacePiece = Nothing}) = True
canOccupy p s
  | isEmptySpace s = True
  | hasTeammate p s = False 
  | hasOpponent p s = True
  | otherwise = False

{- Determines if coord within spaces. -}
isOverlapSpace :: Coord -> [Space] -> Bool
isOverlapSpace _ [] = False
isOverlapSpace c sps = c `DL.elem` (DL.map (\s -> spaceCoord s) sps)

{- Determines if space exists on board. -}
isValid :: Board -> Coord -> Bool
isValid Board{spacesMap = spsMap} c = M.member c spsMap
