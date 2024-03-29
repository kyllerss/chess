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
--import           Chess.Core.DebugUtils

{- Determines if player is currently in check. -}
playerInCheck :: Maybe Board -> Player -> Bool
playerInCheck Nothing _ = False
playerInCheck b pp = isPlayerInCheck

  where isPlayerInCheck :: Bool
        isPlayerInCheck = DL.any isPlayerKingSpace allSpacesThreatened

        isPlayerKingSpace :: Space -> Bool
        isPlayerKingSpace (Void _) = False
        isPlayerKingSpace s = matchesPlayer && matchesKing
          where matchesPlayer, matchesKing :: Bool
                matchesPlayer = (piecePlayer <$> spacePiece s) == Just pp
                matchesKing = (pieceType <$> spacePiece s) == Just King

        allSpacesThreatened :: [Space]
        allSpacesThreatened = threatenedSpaces b pp

{- Moves piece from one space to the next. If requested move is invalid, returns nothing.  -}
move :: PieceId -> Coord -> Board -> Maybe Board
move pId c !b = force $ moveInner p c b
  where
    p :: Maybe Piece
    p = fetchPieceById pId b

{- Internal implementation of 'move' with piece resolved. -}
moveInner :: Maybe Piece -> Coord -> Board -> Maybe Board
moveInner Nothing _ _ = Nothing
moveInner (Just p@Piece{pieceId = pId, piecePlayer = pp}) destCoord b
    | resultsInCheck = Nothing
    | targetCoordStandard originSpace = movedBoard >>= applyCollateralPieces 
    | targetCoordSpecial originSpace = movedBoard >>= applyCollateralMoves >>= applyCollateralCaptures
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
            [ m | SideEffectMove{sideEffectMove = m} <- moveSideEffects targetSpecialMove]

    applyCollateralPieces :: Board -> Maybe Board
    applyCollateralPieces newB =
      DL.foldl (\accB mp ->
                 let s' = sideEffectSpace mp
                     p' = sideEffectPiece mp
                 in addPieceToBoard p' (spaceCoord s') (fromJust accB))
               (Just newB)
               [sp | sp@SideEffectPiece{} <- maybe [] moveSideEffects targetSideEffectPieceMove]
      
    applyCollateralCaptures :: Board -> Maybe Board
    applyCollateralCaptures newB =
      DL.foldl (\accB c -> removePieceFromBoard c (fromJust accB))
               (Just newB) 
               [ c | SideEffectCapture{sideEffectCoord = c} <- moveSideEffects targetSpecialMove]

    {- Returns true if target coordinate implies a standard move (ie. no side-effects) -}
    targetCoordStandard :: Maybe Space -> Bool
    targetCoordStandard Nothing = False
    targetCoordStandard (Just s) = movesChecker s destCoord standardMoves

    {- Returns true if target coordinate implies special move (ie. with side-effects) -}
    targetCoordSpecial :: Maybe Space -> Bool
    targetCoordSpecial Nothing = False
    targetCoordSpecial (Just s) = movesChecker s destCoord specialMoves

    standardMoves, specialMoves :: [Move]
    standardMoves = validStandardMoves b p (spaceCoord $ fromJust originSpace) []
    specialMoves = validSpecialMoves b p (spaceCoord $ fromJust originSpace) []

    movesChecker :: Space -> Coord -> [Move] -> Bool
    movesChecker _ dc moves = -- TODO: Space not needed, remove from callers
      dc `DL.elem` DL.map (spaceCoord . moveSpace) moves
    
    originSpace :: Maybe Space
    originSpace = fetchPieceSpace p b
    destSpace :: Maybe Space
    destSpace = fetchSpace destCoord b

    targetSpecialMove :: Move
    targetSpecialMove = fromJust $ DL.find (\m -> Just (moveSpace m) == destSpace) specialMoves

    {- Spaces with side effects -}
    --sideEffectPieceSpaces :: [Space]
    --sideEffectPieceSpaces = M.foldl' (\acc s -> if isJust $ spaceSideEffectType s then s : acc else acc) [] (spacesMap b) 
    targetSideEffectPieceMove :: Maybe Move
    targetSideEffectPieceMove
      | isNothing maybeSideEffect = Nothing
      | otherwise = buildMove p b destCoord False [fromJust maybeSideEffect]
      where
        maybeSideEffect :: Maybe BoardSideEffect
        maybeSideEffect = applySideEffectMove p (fromJust destSpace)

{- Moves piece without any validation. -}
transfer :: Board -> Piece -> Maybe Space -> Maybe Space -> Maybe Board
transfer _ _ Nothing _ = Nothing
transfer _ _ _ Nothing = Nothing
transfer b p (Just os) (Just ds) = Just b >>=
                                   replaceSpace newOriginSpace >>=
                                   replaceSpace newDestinationSpace
    where
        newOriginSpace, newDestinationSpace :: Space
        newOriginSpace = removePiece os
        newDestinationSpace = addPiece ds p{pieceMoved = True}

{- Returns all valid moves for each piece of a given player in a given board -}
allValidMoves :: Maybe Board -> Player -> [Move]
allValidMoves Nothing _ = []
allValidMoves jb@(Just b) pl = validNonCheckMoves
  where
    validNonCheckMoves :: [Move]
    validNonCheckMoves = DL.filter fixesCheck expectedMoves

    expectedMoves :: [Move]
    expectedMoves = allValidMovesInner jb pl  
    
    fixesCheck :: Move -> Bool
    fixesCheck Move{moveSpace = Void _} = False
    fixesCheck Move{movePieceId = mpId, moveSpace = mvs} = not $ playerInCheck newBoard pl
      where
        newBoard :: Maybe Board
        newBoard = transfer b targetPiece originSpace (Just mvs)

        originSpace :: Maybe Space
        originSpace = fetchPieceSpace (fromJust $ fetchPieceById mpId b) b

        targetPiece :: Piece
        targetPiece = fromJust $ fetchPieceById mpId b
            

allValidMovesInner :: Maybe Board -> Player -> [Move]
allValidMovesInner Nothing _ = []
allValidMovesInner (Just b@Board{spacesMap = spsMap}) pl = M.foldl' accumulator [] spsMap
  where
    accumulator :: [Move] -> Space -> [Move]
    accumulator acc Space{spacePiece = Nothing} = acc
    accumulator acc (Void _) = acc
    accumulator acc Space{spaceCoord = sCoord, spacePiece = Just Piece{pieceId = pId, piecePlayer = ppl}}
      | ppl == pl = validMoves b pId sCoord ++ acc
      | otherwise = acc

{- Returns all valid moves for a given piece. -}
validMoves :: Board -> PieceId -> Coord -> [Move]
validMoves !b pId c = validMovesInner b p c []
  where
    p :: Maybe Piece
    p = fetchPieceById pId b
  
validMovesInner :: Board -> Maybe Piece -> Coord -> [Piece] -> [Move]
validMovesInner _ Nothing _ _ = []
validMovesInner b (Just p) originCoord instigatorPieces =
  validStandardMoves b p originCoord instigatorPieces DL.++ validSpecialMoves b p originCoord instigatorPieces 
  
validStandardMoves :: Board -> Piece -> Coord -> [Piece] -> [Move]
validStandardMoves b p originCoord instigatorPieces =
  concatMap (\d -> candidateMoves p originCoord b d instigatorPieces) [minBound .. maxBound]

validSpecialMoves :: Board -> Piece -> Coord -> [Piece] -> [Move]
validSpecialMoves b p originCoord instigatorPieces =
  concatMap (\d -> specialCandidateMoves p originCoord b d instigatorPieces) [minBound .. maxBound]
    
{-
     Returns list of spaces that threaten provided player
     (Note: does not simulate what would happen if you moved onto a pawn's diagonal - FIXME).
-}

threatenedSpaces :: Maybe Board -> Player -> [Space]
threatenedSpaces Nothing _ = []
threatenedSpaces b player = threatenedSpacesInner b player []

threatenedSpacesInner :: Maybe Board -> Player -> [Piece] -> [Space]
threatenedSpacesInner Nothing _ _ = [] 
threatenedSpacesInner b player instigator = DL.map moveSpace $ -- extract spaces
                                            filter moveIsConsumable $ -- keep offensive moves
                                            DL.foldl' (DL.++) [] $ -- join list of valid coords
                                            DL.map (\(op, oc) -> validMovesInner (fromJust b) (Just op) oc instigator) $ -- calc opp valid moves
                                            oppCoords b -- fetch opp coords
  where
    oppCoords :: Maybe Board -> [(Piece, Coord)]
    oppCoords Nothing = []
    oppCoords (Just Board{spacesMap = spsMap'}) =
      foldr func [] spsMap'
          where func :: Space -> [(Piece, Coord)] -> [(Piece, Coord)]
                func (Void _) acc = acc
                func spc acc = if isOpponent spc
                               then (fromJust (spacePiece spc), spaceCoord spc) : acc
                               else acc

    isOpponent :: Space -> Bool
    isOpponent s = isJust (spacePiece s)
                   && (piecePlayer (fromJust $ spacePiece s) /= player) 


{- Returns candidate moves (legal and illegal) for given piece type. -}
candidateMoves :: Piece -> Coord -> Board -> Direction -> [Piece] -> [Move]

-- pawn
candidateMoves p@Piece{ pieceType = Pawn
                      , piecePlayer = pp@Player{ playerDirection = pd }
                      }
               c
               b@Board{spacesMap = spsMap}
               d
               _
    | d /= pd = []
    | pieceMoved p = forwardOne DL.++ diagonals -- piece moved
    | not (pieceMoved p) = forwardOne DL.++ forwardTwo DL.++ diagonals -- piece not moved
    | otherwise = []
  where
    forwardOne :: [Move]
    forwardOne
      | isEmptySpace $ M.lookup (moveD c d 1) spsMap = pawnForwardMove $ M.lookup (moveD c d 1) spsMap
      | otherwise = []

    forwardTwo :: [Move]
    forwardTwo
      | null forwardOne = [] -- first pawn space obstructed
      | isEmptySpace $ M.lookup (moveD c d 2) spsMap = pawnForwardMove $ M.lookup (moveD c d 2) spsMap
      | otherwise = []

    diagonals :: [Move]
    diagonals = pawnDiagonalMove (M.lookup (moveD c (rotateLeft d) 1) spsMap)
                DL.++ pawnDiagonalMove (M.lookup (moveD c (rotateRight d) 1) spsMap)

    pawnDiagonalMove :: Maybe Space -> [Move]
    pawnDiagonalMove Nothing = []
    pawnDiagonalMove (Just (Void _)) = []
    pawnDiagonalMove s@(Just Space{spaceCoord = coord})
        | hasOpponent pp s = maybeToList $ buildMove p b coord True []
        | otherwise = []

    pawnForwardMove :: Maybe Space -> [Move]
    pawnForwardMove Nothing = []
    pawnForwardMove (Just (Void _)) = []
    pawnForwardMove s@(Just Space{spaceCoord = coord})
        | canOccupy pp s = maybeToList $ buildMove p b coord False []
        | otherwise = []

-- king
{-
    Rules: 1) Pieces cannot have moved.
           2) No pieces in the way.
           3) Cannot castle out of, through, or into check.
-}
-- candidateMoves p@Piece{pieceType = King} _ _ _ (Just instigatorPiece)
--  | p == instigatorPiece = []
candidateMoves p@Piece{pieceType = King,piecePlayer = pp} c b@Board{spacesMap = spsMap} d instigators
  | p `elem` instigators = []
  | canOccupy pp (M.lookup nextCoord spsMap) && not isThreatenedSpace = maybeToList $ buildMove p b nextCoord True []
  | otherwise = []
  where
    nextCoord :: Coord
    nextCoord = moveD c d 1

    threatSpaces :: [Space]
    threatSpaces = threatenedSpacesInner kingMovedBoardState pp (p : instigators)

    isThreatenedSpace :: Bool
    isThreatenedSpace = isOverlapSpace nextCoord threatSpaces 

    kingMovedBoardState :: Maybe Board
    kingMovedBoardState = transfer b p (fetchSpace c b) (fetchSpace nextCoord b)

-- knight
candidateMoves p@Piece{pieceType = Knight,piecePlayer = pp} (Coord row col) b@Board{spacesMap = spsMap} d _
    | d `DL.notElem` [ North, East, South, West ] = []
    | otherwise = DL.filter func $ fetchDirMoves d
  where
    func :: Move -> Bool
    func Move{moveSpace = (Void _)} = False
    func Move{moveSpace = Space{spaceCoord = crd}} = canOccupy pp $ M.lookup crd spsMap
    
    fetchDirMoves :: Direction -> [Move]
    fetchDirMoves North = maybeToList (buildMove p b (Coord (row - 2) (col + 1)) True [])
                          ++
                          maybeToList (buildMove p b (Coord (row - 2) (col - 1)) True [])
    fetchDirMoves East = maybeToList (buildMove p b (Coord (row - 1) (col + 2)) True [])
                         ++
                         maybeToList (buildMove p b (Coord (row + 1) (col + 2)) True [])
    fetchDirMoves South = maybeToList (buildMove p b (Coord (row + 2) (col - 1)) True [])
                          ++
                          maybeToList (buildMove p b (Coord (row + 2) (col + 1)) True [])
    fetchDirMoves West = maybeToList (buildMove p b (Coord (row - 1) (col - 2)) True [])
                         ++ 
                         maybeToList (buildMove p b (Coord (row + 1) (col - 2)) True [])
    fetchDirMoves _ = []

-- rook
candidateMoves p@Piece{pieceType = Rook} c b d instigator =
    directionalCandidateMoves' [ North, East, South, West ] p c b d instigator

-- bishop
candidateMoves p@Piece{pieceType = Bishop} c b d instigator =
    directionalCandidateMoves' [ NorthEast, SouthEast, SouthWest, NorthWest ]
                                p
                                c
                                b
                                d
                                instigator

-- queen
candidateMoves p@Piece{pieceType = Queen} c b d instigator =
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
                                instigator

{- Returns candidate 'special' moves for given piece type. -}
specialCandidateMoves :: Piece -> Coord -> Board -> Direction -> [Piece] -> [Move]
specialCandidateMoves Piece{pieceType = Rook} _ _ _ _ = []
specialCandidateMoves Piece{pieceType = Knight} _ _ _ _ = []
specialCandidateMoves Piece{pieceType = Bishop} _ _ _ _ = []
specialCandidateMoves Piece{pieceType = Queen} _ _ _ _ = []

{- King castling moves. -}
specialCandidateMoves Piece{pieceType = King, pieceMoved = True} _ _ _ _ = []
specialCandidateMoves p@Piece{pieceType = King, piecePlayer = pp@Player{playerDirection = pd}} c b d instigatorPiece
  | d == cLeftDir = castleDir lRookCoord          
  | d == cRightDir = castleDir rRookCoord
  | otherwise = []

  where

    cLeftDir :: Direction
    cLeftDir = rotateLeft . rotateLeft $ pd

    cRightDir :: Direction
    cRightDir = rotateRight . rotateRight $ pd

    oppCastlingDir :: Direction
    oppCastlingDir = rotateRight . rotateRight . rotateRight . rotateRight $ d

    lRookCoord, rRookCoord :: Maybe Coord
    lRookCoord = fetchRookCoord cLeftDir
    rRookCoord = fetchRookCoord cRightDir

    fetchRookCoord :: Direction -> Maybe Coord
    fetchRookCoord rookDir
      | threePiece == Just Rook = Just threeCoord
      | fourPiece == Just Rook = Just fourCoord
      | otherwise = Nothing
      where
        fourCoord, threeCoord :: Coord
        fourCoord = moveD c rookDir 4
        threeCoord = moveD c rookDir 3
        fourPiece, threePiece :: Maybe PieceType
        fourPiece = func $ fetchSpace fourCoord b
        threePiece = func $ fetchSpace threeCoord b
        
        func :: Maybe Space -> Maybe PieceType
        func Nothing = Nothing
        func (Just (Void _)) = Nothing
        func (Just Space{spacePiece = Nothing}) = Nothing
        func (Just Space{spacePiece = Just Piece{pieceType = pt}}) = Just pt
        
    castleDir :: Maybe Coord -> [Move]
    castleDir Nothing = []
    castleDir (Just rookCoord) 
      | not rookMoved && spacesPassable && not spacesThreatened = maybeToList kingMove
      | otherwise = []

      where

        betweenCoords :: [Coord]
        betweenCoords = fetchBetweenCoords c rookCoord
        
        kingDestCoord, rookDestCoord :: Coord
        kingDestCoord = moveD c d 2
        rookDestCoord = moveD kingDestCoord oppCastlingDir 1
        
        kingMove, rookMove :: Maybe Move
        kingMove = buildMove p b kingDestCoord False (maybeToList $ buildSideEffectMove <$> rookMove)
        rookMove = buildMove (fromJust $ fetchPiece rookCoord b) b rookDestCoord False []

        rookSpace :: Maybe Space
        rookSpace = fetchSpace rookCoord b
  
        rookMoved :: Bool
        rookMoved = func rookSpace 
          where func :: Maybe Space -> Bool
                func Nothing = False
                func (Just (Void _)) = False
                func (Just Space{spacePiece = Nothing}) = False
                func (Just Space{spacePiece = Just pc}) = pieceMoved pc

        spacesPassable :: Bool
        spacesPassable =
          DL.all (\coord -> isValid b coord && not (isObstructed coord)) betweenCoords

        isObstructed :: Coord -> Bool
        isObstructed coord = sPiece $ fetchSpace coord b
          where sPiece :: Maybe Space -> Bool
                sPiece Nothing = False
                sPiece (Just (Void _)) = False
                sPiece (Just s) = isJust $ spacePiece s

        kingTraverseCoords :: [Coord]
        kingTraverseCoords = kingDestCoord : fetchBetweenCoords c kingDestCoord

        spacesThreatened :: Bool
        spacesThreatened
          | p `elem` instigatorPiece = False
          | otherwise = DL.any (\crd -> isOverlapSpace crd (threatenedSpacesInner (virtBoard b pp crd) pp instigatorPiece)) kingTraverseCoords
          where
            virtBoard :: Board -> Player -> Coord -> Maybe Board
            virtBoard brd pp' coord = addPieceToBoard (testPiece pp' coord) coord brd

            testPiece :: Player -> Coord -> Piece
            testPiece pp' coord = buildPiece (PieceId (-1)) Pawn White pp' (Just coord) 
                                                                                     
specialCandidateMoves p@Piece{pieceType = Pawn, piecePlayer = Player{playerId = plId, playerDirection = pd}} c b d _
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
      turnOnLeftNotMissed = null (movesSince leftNeighbour)
      turnOnRightNotMissed = null (movesSince rightNeighbour) 

      movesSince :: Maybe Piece -> [(PieceId, Coord)]
      movesSince Nothing = []
      movesSince (Just Piece{pieceId = pId}) =
        filter matchesPlayer $ takeWhile notMatchesPiece (boardMoves b)

        where
          notMatchesPiece :: (PieceId, Coord) -> Bool
          notMatchesPiece (mPieceId, _) = mPieceId /= pId

          matchesPlayer :: (PieceId, Coord) -> Bool
          matchesPlayer (mPieceId, _) = Just plId == (playerId . piecePlayer <$> fetchPieceById mPieceId b)
      
      leftIsPawn, rightIsPawn :: Bool
      leftIsPawn = (pieceType <$> leftNeighbour) == Just Pawn
      rightIsPawn = (pieceType <$> rightNeighbour) == Just Pawn

      leftJumpedOpenning, rightJumpedOpenning :: Bool
      leftJumpedOpenning = isJump (playerDirection . piecePlayer <$> leftNeighbour)
                                  (join $ pieceOrigin <$> leftNeighbour) leftCoord
      rightJumpedOpenning = isJump (playerDirection . piecePlayer <$> rightNeighbour)
                                   (join $ pieceOrigin <$> rightNeighbour) rightCoord

      isJump :: Maybe Direction -> Maybe Coord -> Coord -> Bool
      isJump Nothing _ _ = False
      isJump _ Nothing _ = False
      isJump (Just od) (Just oc) cc = moveD oc od 2 == cc

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
      leftEnPassant = maybeToList $ buildMove p b leftEnPassantCoord True [buildSideEffectCapture leftCoord]
      rightEnPassant = maybeToList $ buildMove p b rightEnPassantCoord True [buildSideEffectCapture rightCoord]

{- Pieces that move directionally, returns candidate coordinates  -}
directionalCandidateMoves' :: [Direction] -> Piece -> Coord -> Board -> Direction -> [Piece] -> [Move]
directionalCandidateMoves' ds p@Piece{piecePlayer = cPlayer} c b@Board{spacesMap = spsMap} d instigatorPiece
    | d `DL.notElem` ds = []
    | validSpaceWithOpp = maybeToList $ buildMove p b (spaceCoord cSpace) True []
    | validSpace = maybeToList (buildMove p b (spaceCoord cSpace) True []) ++ candidateMoves p nextCoord b d instigatorPiece
    | otherwise = []
  where
    validSpace :: Bool
    validSpace = M.member nextCoord spsMap && canOccupy cPlayer (M.lookup nextCoord spsMap)

    validSpaceWithOpp :: Bool
    validSpaceWithOpp = validSpace && hasOpponent cPlayer (M.lookup nextCoord spsMap)

    cSpace :: Space
    cSpace = fromJust $ M.lookup nextCoord spsMap

    nextCoord :: Coord
    nextCoord = moveD c d 1

{- Returns true if space has opponent  -}
hasOpponent :: Player -> Maybe Space -> Bool
hasOpponent _ Nothing = False
hasOpponent _ (Just (Void _)) = False
hasOpponent _ (Just Space{spacePiece = Nothing}) = False
hasOpponent cPlayer (Just Space{spacePiece = Just Piece{piecePlayer = pp}}) =
    cPlayer /= pp

{- Returns true if space has a teammate. -}
hasTeammate :: Player -> Maybe Space -> Bool
hasTeammate _ Nothing = False
hasTeammate _ (Just (Void _)) = False
hasTeammate _ (Just Space{spacePiece = Nothing}) = True
hasTeammate cPlayer (Just Space{spacePiece = Just Piece{piecePlayer = pp}}) =
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
isOverlapSpace c sps = c `DL.elem` DL.map spaceCoord sps

{- Determines if space exists on board. -}
isValid :: Board -> Coord -> Bool
isValid Board{spacesMap = spsMap} c = M.member c spsMap

{- Determines side-effects based on piece and space. -}
applySideEffectMove :: Piece -> Space -> Maybe BoardSideEffect
applySideEffectMove p@Piece{pieceType = Pawn} s@Space{spaceSideEffectType = Just PawnPromotion} = Just $ buildSideEffectPiece p{pieceType = Queen} s 
applySideEffectMove _ _ = Nothing
