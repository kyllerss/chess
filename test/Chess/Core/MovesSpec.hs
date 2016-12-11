module Chess.Core.MovesSpec ( spec ) where

import           Test.Hspec
import           Chess.Core.Domain
import           Chess.Core.Moves
import qualified Data.Maybe        as DM
import qualified Debug.Trace       as T

buildTestPiece :: Int -> PieceType -> Int -> Direction -> Piece
buildTestPiece pId pType playerId playerDir =
  buildPiece (PieceId pId)
             pType
             White
             (Player { playerName = "dummy"
                     , playerType = Human
                     , playerId = playerId
                     , playerDirection = playerDir
                     })


spec :: Spec
spec = describe "Pieces" $ do
  describe "Pawn" $ do
  
    it "valid moves consists of forward two spots when unobstructed" $ do
      
        -- initial board setup
        let emptyBoard = initBoard 3 3 defaultSpaceBuilder
            originCoord = Coord 0 1
            pawn = buildTestPiece 1 Pawn 1 South

            board :: Maybe Board
            board = addPieceToBoard emptyBoard pawn originCoord

        board `shouldNotBe` Nothing

        let ms :: [Move]
            ms = validMoves (DM.fromJust board) pawn originCoord

        length ms `shouldBe` 2

        let coords :: [Coord]
            coords = map (\m -> spaceCoord $ moveSpace m) ms

        elem (Coord 1 1) coords `shouldBe` True
        elem (Coord 2 1) coords `shouldBe` True

    it "no valid moves when facing edge of board" $ do
      
        -- initial board setup
        let emptyBoard = initBoard 3 3 defaultSpaceBuilder
            originCoord = Coord 0 1
            pawn = buildTestPiece 1 Pawn 1 North

            board :: Maybe Board
            board = addPieceToBoard emptyBoard pawn originCoord

        board `shouldNotBe` Nothing

        let ms :: [Move]
            ms = validMoves (DM.fromJust board) pawn originCoord

        length ms `shouldBe` 0

    it "can move to valid space vertically when unmoved" $ do
      
        -- initial board setup
        let emptyBoard = initBoard 3 3 defaultSpaceBuilder
            originCoord = Coord 0 1
            pawn = buildTestPiece 1 Pawn 1 South

            board :: Maybe Board
            board = addPieceToBoard emptyBoard pawn originCoord

        board `shouldNotBe` Nothing

        -- move piece
        let destCoord = Coord 1 1
            newBoard = move (DM.fromJust board) pawn destCoord

        newBoard `shouldNotBe` Nothing

        -- verify spaces still present
        let origSpace, destSpace :: Maybe Space
            origSpace = fetchSpace (DM.fromJust newBoard) originCoord
            destSpace = fetchSpace (DM.fromJust newBoard) destCoord

        origSpace `shouldNotBe` Nothing
        destSpace `shouldNotBe` Nothing

        -- verify piece moved
        let origSpace', destSpace' :: Space
            origSpace' = DM.fromJust origSpace
            destSpace' = DM.fromJust destSpace

        spacePiece origSpace' `shouldBe` Nothing
        spacePiece destSpace' `shouldNotBe` Nothing

    it "can move only once forward when previously moved" $ do
      
        -- initial board setup
        let emptyBoard = initBoard 9 9 defaultSpaceBuilder
            originCoord = Coord 0 4
            pawn = buildTestPiece 1 Pawn 1 South

            board :: Maybe Board
            board = addPieceToBoard emptyBoard pawn originCoord

        board `shouldNotBe` Nothing

        -- move piece
        let destCoord = Coord 1 4
            newBoard = move (DM.fromJust board) pawn destCoord

        newBoard `shouldNotBe` Nothing

        -- verify piece marked as moved
        let destSpace :: Maybe Space
            destSpace = fetchSpace (DM.fromJust newBoard) destCoord

        destSpace `shouldNotBe` Nothing

        let destSpace' :: Space
            destSpace' = DM.fromJust destSpace
            movedPiece = spacePiece destSpace'

        movedPiece `shouldNotBe` Nothing

        let movedPawn :: Piece
            movedPawn = DM.fromJust movedPiece

        pieceMoved movedPawn `shouldBe` True

        let ms :: [Move]
            ms = validMoves (DM.fromJust newBoard) movedPawn (spaceCoord destSpace')

        length ms `shouldBe` 1
        (spaceCoord . moveSpace) (ms !! 0) `shouldBe` Coord 2 4

    it "valid moves contains diagonals when opp present" $ do
      
        -- initial board setup
        let emptyBoard = initBoard 9 9 defaultSpaceBuilder
            originCoord = Coord 1 4
            pawn = buildTestPiece 1 Pawn 1 South
            pawn2 = buildTestPiece 2 Pawn 2 North
            pawn3 = buildTestPiece 3 Pawn 2 North

            board :: Maybe Board
            board = foldl (\b (p, c) -> addPieceToBoard (DM.fromJust b) p c)
                          (Just emptyBoard)
                          [ (pawn, originCoord)
                          , (pawn2, Coord 2 5)
                          , (pawn3, Coord 2 3)]

        board `shouldNotBe` Nothing

        let ms :: [Move]
            ms = validMoves (DM.fromJust board) pawn originCoord

        length ms `shouldBe` 4

        let coords :: [Coord]
            coords = map (\m -> spaceCoord $ moveSpace m) ms

        elem (Coord 2 4) coords `shouldBe` True
        elem (Coord 3 4) coords `shouldBe` True
        elem (Coord 2 3) coords `shouldBe` True
        elem (Coord 2 5) coords `shouldBe` True

    it "cannot move to invalid space" $ do

        -- initial board setup
        let emptyBoard = initBoard 2 2 defaultSpaceBuilder
            originCoord = Coord 0 0
            pawn = buildTestPiece 1 Pawn 1 South

            board :: Maybe Board
            board = addPieceToBoard emptyBoard pawn originCoord

        board `shouldNotBe` Nothing

        -- move piece
        let destCoord = Coord 1 1
            newBoard = move (DM.fromJust board) pawn destCoord

        newBoard `shouldBe` Nothing

  describe "Rook" $ do

    it "has valid moves when unobstructed" $ do

        let emptyBoard = initBoard 9 9 defaultSpaceBuilder
            originCoord = Coord 4 4
            rook = buildTestPiece 1 Rook 1 South

            board :: Maybe Board
            board = addPieceToBoard emptyBoard rook originCoord

        board `shouldNotBe` Nothing

        -- fetch moves
        let ms :: [Move]
            ms = validMoves (DM.fromJust board) rook originCoord

        length ms `shouldBe` 16

        let coords :: [Coord]
            coords = map (\m -> spaceCoord $ moveSpace m) ms

        elem (Coord 0 4) coords `shouldBe` True
        elem (Coord 1 4) coords `shouldBe` True
        elem (Coord 2 4) coords `shouldBe` True
        elem (Coord 3 4) coords `shouldBe` True
        elem (Coord 5 4) coords `shouldBe` True
        elem (Coord 6 4) coords `shouldBe` True
        elem (Coord 7 4) coords `shouldBe` True
        elem (Coord 8 4) coords `shouldBe` True
        elem (Coord 4 0) coords `shouldBe` True
        elem (Coord 4 1) coords `shouldBe` True
        elem (Coord 4 2) coords `shouldBe` True
        elem (Coord 4 3) coords `shouldBe` True
        elem (Coord 4 5) coords `shouldBe` True
        elem (Coord 4 6) coords `shouldBe` True
        elem (Coord 4 7) coords `shouldBe` True
        elem (Coord 4 8) coords `shouldBe` True

    it "has valid moves when obstructed by own pieces" $ do

        let emptyBoard = initBoard 9 9 defaultSpaceBuilder
            originCoord = Coord 4 4
            rook = buildTestPiece 1 Rook 1 South
            pawn1 = buildTestPiece 2 Pawn 1 South
            pawn2 = buildTestPiece 3 Pawn 1 South
            pawn3 = buildTestPiece 4 Pawn 1 South
            pawn4 = buildTestPiece 5 Pawn 1 South

            board :: Maybe Board
            board = foldl (\b (p, c) -> addPieceToBoard (DM.fromJust b) p c)
                          (Just emptyBoard)
                          [ (rook, originCoord)
                          , (pawn1, Coord 4 7)
                          , (pawn2, Coord 4 1)
                          , (pawn3, Coord 7 4)
                          , (pawn4, Coord 1 4)]

        board `shouldNotBe` Nothing

        -- fetch moves
        let ms :: [Move]
            ms = validMoves (DM.fromJust board) rook originCoord

        length ms `shouldBe` 8

        let coords :: [Coord]
            coords = map (\m -> spaceCoord $ moveSpace m) ms

        -- elem (Coord 0 4) coords `shouldBe` True
        -- elem (Coord 1 4) coords `shouldBe` True
        elem (Coord 2 4) coords `shouldBe` True
        elem (Coord 3 4) coords `shouldBe` True
        elem (Coord 5 4) coords `shouldBe` True
        elem (Coord 6 4) coords `shouldBe` True
        -- elem (Coord 7 4) coords `shouldBe` True
        -- elem (Coord 8 4) coords `shouldBe` True
        -- elem (Coord 4 0) coords `shouldBe` True
        -- elem (Coord 4 1) coords `shouldBe` True
        elem (Coord 4 2) coords `shouldBe` True
        elem (Coord 4 3) coords `shouldBe` True
        elem (Coord 4 5) coords `shouldBe` True
        elem (Coord 4 6) coords `shouldBe` True
        -- elem (Coord 4 7) coords `shouldBe` True
        -- elem (Coord 4 8) coords `shouldBe` True

    it "has valid moves when obstructed by other pieces" $ do

        let emptyBoard = initBoard 9 9 defaultSpaceBuilder
            originCoord = Coord 4 4
            rook = buildTestPiece 1 Rook 1 South
            pawn1 = buildTestPiece 2 Pawn 2 South
            pawn2 = buildTestPiece 3 Pawn 2 South
            pawn3 = buildTestPiece 4 Pawn 2 South
            pawn4 = buildTestPiece 5 Pawn 2 South

            board = foldl (\b (p, c) -> addPieceToBoard (DM.fromJust b) p c)
                          (Just emptyBoard)
                          [ (rook, originCoord)
                          , (pawn1, Coord 4 7)
                          , (pawn2, Coord 4 1)
                          , (pawn3, Coord 7 4)
                          , (pawn4, Coord 1 4)]

        board `shouldNotBe` Nothing
        
        -- fetch moves
        let ms :: [Move]
            ms = validMoves (DM.fromJust board) rook originCoord

        length ms `shouldBe` 12

        let coords :: [Coord]
            coords = map (\m -> spaceCoord $ moveSpace m) ms

        -- elem (Coord 0 4) coords `shouldBe` True
        elem (Coord 1 4) coords `shouldBe` True
        elem (Coord 2 4) coords `shouldBe` True
        elem (Coord 3 4) coords `shouldBe` True
        elem (Coord 5 4) coords `shouldBe` True
        elem (Coord 6 4) coords `shouldBe` True
        elem (Coord 7 4) coords `shouldBe` True
        -- elem (Coord 8 4) coords `shouldBe` True
        -- elem (Coord 4 0) coords `shouldBe` True
        elem (Coord 4 1) coords `shouldBe` True
        elem (Coord 4 2) coords `shouldBe` True
        elem (Coord 4 3) coords `shouldBe` True
        elem (Coord 4 5) coords `shouldBe` True
        elem (Coord 4 6) coords `shouldBe` True
        elem (Coord 4 7) coords `shouldBe` True
        -- elem (Coord 4 8) coords `shouldBe` True

  describe "Bishop" $ do

    it "has valid moves when unobstructed" $ do

        let emptyBoard = initBoard 9 9 defaultSpaceBuilder
            originCoord = Coord 4 4
            bishop = buildTestPiece 1 Bishop 1 South

            board :: Maybe Board
            board = addPieceToBoard emptyBoard bishop originCoord

        board `shouldNotBe` Nothing

        -- fetch moves
        let ms :: [Move]
            ms = validMoves (DM.fromJust board) bishop originCoord

        length ms `shouldBe` 16

        let coords :: [Coord]
            coords = map (\m -> spaceCoord $ moveSpace m) ms

        elem (Coord 0 0) coords `shouldBe` True
        elem (Coord 1 1) coords `shouldBe` True
        elem (Coord 2 2) coords `shouldBe` True
        elem (Coord 3 3) coords `shouldBe` True
        elem (Coord 5 3) coords `shouldBe` True
        elem (Coord 6 2) coords `shouldBe` True
        elem (Coord 7 1) coords `shouldBe` True
        elem (Coord 8 0) coords `shouldBe` True
        elem (Coord 0 8) coords `shouldBe` True
        elem (Coord 1 7) coords `shouldBe` True
        elem (Coord 2 6) coords `shouldBe` True
        elem (Coord 3 5) coords `shouldBe` True
        elem (Coord 5 5) coords `shouldBe` True
        elem (Coord 6 6) coords `shouldBe` True
        elem (Coord 7 7) coords `shouldBe` True
        elem (Coord 8 8) coords `shouldBe` True

    it "has valid moves when obstructed by own pieces" $ do

        let emptyBoard = initBoard 9 9 defaultSpaceBuilder
            originCoord = Coord 4 4
            bishop = buildTestPiece 1 Bishop 1 South
            pawn1 = buildTestPiece 2 Pawn 1 South
            pawn2 = buildTestPiece 3 Pawn 1 South
            pawn3 = buildTestPiece 4 Pawn 1 South
            pawn4 = buildTestPiece 5 Pawn 1 South

            board :: Maybe Board
            board = foldl (\b (p, c) -> addPieceToBoard (DM.fromJust b) p c)
                          (Just emptyBoard)
                          [ (bishop, originCoord)
                          , (pawn1, Coord 2 2)
                          , (pawn2, Coord 2 6)
                          , (pawn3, Coord 6 2)
                          , (pawn4, Coord 6 6)]

        board `shouldNotBe` Nothing

        -- fetch moves
        let ms :: [Move]
            ms = validMoves (DM.fromJust board) bishop originCoord

        length ms `shouldBe` 4

        let coords :: [Coord]
            coords = map (\m -> spaceCoord $ moveSpace m) ms

        --elem (Coord 0 0) coords `shouldBe` True
        --elem (Coord 1 1) coords `shouldBe` True
        --elem (Coord 2 2) coords `shouldBe` True
        elem (Coord 3 3) coords `shouldBe` True
        elem (Coord 5 3) coords `shouldBe` True
        --elem (Coord 6 2) coords `shouldBe` True
        --elem (Coord 7 1) coords `shouldBe` True
        --elem (Coord 8 0) coords `shouldBe` True
        --elem (Coord 0 8) coords `shouldBe` True
        --elem (Coord 1 7) coords `shouldBe` True
        --elem (Coord 2 6) coords `shouldBe` True
        elem (Coord 3 5) coords `shouldBe` True
        elem (Coord 5 5) coords `shouldBe` True
        --elem (Coord 6 6) coords `shouldBe` True
        --elem (Coord 7 7) coords `shouldBe` True
        --elem (Coord 8 8) coords `shouldBe` True

    it "has valid moves when obstructed by other pieces" $ do

        let emptyBoard = initBoard 9 9 defaultSpaceBuilder
            originCoord = Coord 4 4
            bishop = buildTestPiece 1 Bishop 1 South
            pawn1 = buildTestPiece 2 Pawn 2 South
            pawn2 = buildTestPiece 3 Pawn 2 South
            pawn3 = buildTestPiece 4 Pawn 2 South
            pawn4 = buildTestPiece 5 Pawn 2 South

            board = foldl (\b (p, c) -> addPieceToBoard (DM.fromJust b) p c)
                          (Just emptyBoard)
                          [ (bishop, originCoord)
                          , (pawn1, Coord 2 2)
                          , (pawn2, Coord 2 6)
                          , (pawn3, Coord 6 2)
                          , (pawn4, Coord 6 6)]

        board `shouldNotBe` Nothing
        
        -- fetch moves
        let ms :: [Move]
            ms = validMoves (DM.fromJust board) bishop originCoord

        length ms `shouldBe` 8

        let coords :: [Coord]
            coords = map (\m -> spaceCoord $ moveSpace m) ms

        --elem (Coord 0 0) coords `shouldBe` True
        --elem (Coord 1 1) coords `shouldBe` True
        elem (Coord 2 2) coords `shouldBe` True
        elem (Coord 3 3) coords `shouldBe` True
        elem (Coord 5 3) coords `shouldBe` True
        elem (Coord 6 2) coords `shouldBe` True
        --elem (Coord 7 1) coords `shouldBe` True
        --elem (Coord 8 0) coords `shouldBe` True
        --elem (Coord 0 8) coords `shouldBe` True
        --elem (Coord 1 7) coords `shouldBe` True
        elem (Coord 2 6) coords `shouldBe` True
        elem (Coord 3 5) coords `shouldBe` True
        elem (Coord 5 5) coords `shouldBe` True
        elem (Coord 6 6) coords `shouldBe` True
        --elem (Coord 7 7) coords `shouldBe` True
        --elem (Coord 8 8) coords `shouldBe` True

  describe "Queen" $ do

    it "has valid moves when unobstructed" $ do

        let emptyBoard = initBoard 9 9 defaultSpaceBuilder
            originCoord = Coord 4 4
            queen = buildTestPiece 1 Queen 1 South

            board :: Maybe Board
            board = addPieceToBoard emptyBoard queen originCoord

        board `shouldNotBe` Nothing

        -- fetch moves
        let ms :: [Move]
            ms = validMoves (DM.fromJust board) queen originCoord

        length ms `shouldBe` 32

        let coords :: [Coord]
            coords = map (\m -> spaceCoord $ moveSpace m) ms

        -- horizontals
        elem (Coord 0 4) coords `shouldBe` True
        elem (Coord 1 4) coords `shouldBe` True
        elem (Coord 2 4) coords `shouldBe` True
        elem (Coord 3 4) coords `shouldBe` True
        elem (Coord 5 4) coords `shouldBe` True
        elem (Coord 6 4) coords `shouldBe` True
        elem (Coord 7 4) coords `shouldBe` True
        elem (Coord 8 4) coords `shouldBe` True
        elem (Coord 4 0) coords `shouldBe` True
        elem (Coord 4 1) coords `shouldBe` True
        elem (Coord 4 2) coords `shouldBe` True
        elem (Coord 4 3) coords `shouldBe` True
        elem (Coord 4 5) coords `shouldBe` True
        elem (Coord 4 6) coords `shouldBe` True
        elem (Coord 4 7) coords `shouldBe` True
        elem (Coord 4 8) coords `shouldBe` True
        
        -- diagonals
        elem (Coord 0 0) coords `shouldBe` True
        elem (Coord 1 1) coords `shouldBe` True
        elem (Coord 2 2) coords `shouldBe` True
        elem (Coord 3 3) coords `shouldBe` True
        elem (Coord 5 3) coords `shouldBe` True
        elem (Coord 6 2) coords `shouldBe` True
        elem (Coord 7 1) coords `shouldBe` True
        elem (Coord 8 0) coords `shouldBe` True
        elem (Coord 0 8) coords `shouldBe` True
        elem (Coord 1 7) coords `shouldBe` True
        elem (Coord 2 6) coords `shouldBe` True
        elem (Coord 3 5) coords `shouldBe` True
        elem (Coord 5 5) coords `shouldBe` True
        elem (Coord 6 6) coords `shouldBe` True
        elem (Coord 7 7) coords `shouldBe` True
        elem (Coord 8 8) coords `shouldBe` True

    it "has valid moves when obstructed by own pieces" $ do

        let emptyBoard = initBoard 9 9 defaultSpaceBuilder
            originCoord = Coord 4 4
            queen = buildTestPiece 1 Queen 1 South
            pawn1 = buildTestPiece 2 Pawn 1 South
            pawn2 = buildTestPiece 3 Pawn 1 South
            pawn3 = buildTestPiece 4 Pawn 1 South
            pawn4 = buildTestPiece 5 Pawn 1 South
            pawn5 = buildTestPiece 6 Pawn 1 South
            pawn6 = buildTestPiece 7 Pawn 1 South
            pawn7 = buildTestPiece 8 Pawn 1 South
            pawn8 = buildTestPiece 9 Pawn 1 South

            board :: Maybe Board
            board = foldl (\b (p, c) -> addPieceToBoard (DM.fromJust b) p c)
                          (Just emptyBoard)
                          [ (queen, originCoord)
                          , (pawn1, Coord 4 7)
                          , (pawn2, Coord 4 1)
                          , (pawn3, Coord 7 4)
                          , (pawn4, Coord 1 4)
                          , (pawn5, Coord 2 2)
                          , (pawn6, Coord 2 6)
                          , (pawn7, Coord 6 2)
                          , (pawn8, Coord 6 6)]

        board `shouldNotBe` Nothing

        -- fetch moves
        let ms :: [Move]
            ms = validMoves (DM.fromJust board) queen originCoord

        length ms `shouldBe` 12

        let coords :: [Coord]
            coords = map (\m -> spaceCoord $ moveSpace m) ms

        -- horizontals
        -- elem (Coord 0 4) coords `shouldBe` True
        -- elem (Coord 1 4) coords `shouldBe` True
        elem (Coord 2 4) coords `shouldBe` True
        elem (Coord 3 4) coords `shouldBe` True
        elem (Coord 5 4) coords `shouldBe` True
        elem (Coord 6 4) coords `shouldBe` True
        -- elem (Coord 7 4) coords `shouldBe` True
        -- elem (Coord 8 4) coords `shouldBe` True
        -- elem (Coord 4 0) coords `shouldBe` True
        -- elem (Coord 4 1) coords `shouldBe` True
        elem (Coord 4 2) coords `shouldBe` True
        elem (Coord 4 3) coords `shouldBe` True
        elem (Coord 4 5) coords `shouldBe` True
        elem (Coord 4 6) coords `shouldBe` True
        -- elem (Coord 4 7) coords `shouldBe` True
        -- elem (Coord 4 8) coords `shouldBe` True
        
        -- diagonals
        --elem (Coord 0 0) coords `shouldBe` True
        --elem (Coord 1 1) coords `shouldBe` True
        --elem (Coord 2 2) coords `shouldBe` True
        elem (Coord 3 3) coords `shouldBe` True
        elem (Coord 5 3) coords `shouldBe` True
        --elem (Coord 6 2) coords `shouldBe` True
        --elem (Coord 7 1) coords `shouldBe` True
        --elem (Coord 8 0) coords `shouldBe` True
        --elem (Coord 0 8) coords `shouldBe` True
        --elem (Coord 1 7) coords `shouldBe` True
        --elem (Coord 2 6) coords `shouldBe` True
        elem (Coord 3 5) coords `shouldBe` True
        elem (Coord 5 5) coords `shouldBe` True
        --elem (Coord 6 6) coords `shouldBe` True
        --elem (Coord 7 7) coords `shouldBe` True
        --elem (Coord 8 8) coords `shouldBe` True

    it "has valid moves when obstructed by other pieces" $ do

        let emptyBoard = initBoard 9 9 defaultSpaceBuilder
            originCoord = Coord 4 4
            queen = buildTestPiece 1 Queen 1 South
            pawn1 = buildTestPiece 2 Pawn 2 South
            pawn2 = buildTestPiece 3 Pawn 2 South
            pawn3 = buildTestPiece 4 Pawn 2 South
            pawn4 = buildTestPiece 5 Pawn 2 South
            pawn5 = buildTestPiece 6 Pawn 2 South
            pawn6 = buildTestPiece 7 Pawn 2 South
            pawn7 = buildTestPiece 8 Pawn 2 South
            pawn8 = buildTestPiece 9 Pawn 2 South

            board = foldl (\b (p, c) -> addPieceToBoard (DM.fromJust b) p c)
                          (Just emptyBoard)
                          [ (queen, originCoord)
                          , (pawn1, Coord 4 7)
                          , (pawn2, Coord 4 1)
                          , (pawn3, Coord 7 4)
                          , (pawn4, Coord 1 4)
                          , (pawn5, Coord 2 2)
                          , (pawn6, Coord 2 6)
                          , (pawn7, Coord 6 2)
                          , (pawn8, Coord 6 6)]

        board `shouldNotBe` Nothing
        
        -- fetch moves
        let ms :: [Move]
            ms = validMoves (DM.fromJust board) queen originCoord

        length ms `shouldBe` 20

        let coords :: [Coord]
            coords = map (\m -> spaceCoord $ moveSpace m) ms

        -- horizontals
        -- elem (Coord 0 4) coords `shouldBe` True
        elem (Coord 1 4) coords `shouldBe` True
        elem (Coord 2 4) coords `shouldBe` True
        elem (Coord 3 4) coords `shouldBe` True
        elem (Coord 5 4) coords `shouldBe` True
        elem (Coord 6 4) coords `shouldBe` True
        elem (Coord 7 4) coords `shouldBe` True
        -- elem (Coord 8 4) coords `shouldBe` True
        -- elem (Coord 4 0) coords `shouldBe` True
        elem (Coord 4 1) coords `shouldBe` True
        elem (Coord 4 2) coords `shouldBe` True
        elem (Coord 4 3) coords `shouldBe` True
        elem (Coord 4 5) coords `shouldBe` True
        elem (Coord 4 6) coords `shouldBe` True
        elem (Coord 4 7) coords `shouldBe` True
        -- elem (Coord 4 8) coords `shouldBe` True
        
        -- diagonals
        --elem (Coord 0 0) coords `shouldBe` True
        --elem (Coord 1 1) coords `shouldBe` True
        elem (Coord 2 2) coords `shouldBe` True
        elem (Coord 3 3) coords `shouldBe` True
        elem (Coord 5 3) coords `shouldBe` True
        elem (Coord 6 2) coords `shouldBe` True
        --elem (Coord 7 1) coords `shouldBe` True
        --elem (Coord 8 0) coords `shouldBe` True
        --elem (Coord 0 8) coords `shouldBe` True
        --elem (Coord 1 7) coords `shouldBe` True
        elem (Coord 2 6) coords `shouldBe` True
        elem (Coord 3 5) coords `shouldBe` True
        elem (Coord 5 5) coords `shouldBe` True
        elem (Coord 6 6) coords `shouldBe` True
        --elem (Coord 7 7) coords `shouldBe` True
        --elem (Coord 8 8) coords `shouldBe` True

  describe "Knight" $ do

    it "has valid moves when unobstructed" $ do

        let emptyBoard = initBoard 9 9 defaultSpaceBuilder
            originCoord = Coord 4 4
            knight = buildTestPiece 1 Knight 1 South

            board :: Maybe Board
            board = addPieceToBoard emptyBoard knight originCoord

        board `shouldNotBe` Nothing

        -- fetch moves
        let ms :: [Move]
            ms = validMoves (DM.fromJust board) knight originCoord

        length ms `shouldBe` 8

        let coords :: [Coord]
            coords = map (\m -> spaceCoord $ moveSpace m) ms

        elem (Coord 2 3) coords `shouldBe` True
        elem (Coord 2 5) coords `shouldBe` True
        elem (Coord 3 6) coords `shouldBe` True
        elem (Coord 5 6) coords `shouldBe` True
        elem (Coord 6 3) coords `shouldBe` True
        elem (Coord 6 5) coords `shouldBe` True
        elem (Coord 3 2) coords `shouldBe` True
        elem (Coord 5 2) coords `shouldBe` True

    it "has valid moves when obstructed by own pieces" $ do

        let emptyBoard = initBoard 9 9 defaultSpaceBuilder
            originCoord = Coord 4 4
            knight = buildTestPiece 1 Knight 1 South
            pawn1 = buildTestPiece 2 Pawn 1 South
            pawn2 = buildTestPiece 3 Pawn 1 South
            pawn3 = buildTestPiece 4 Pawn 1 South
            pawn4 = buildTestPiece 5 Pawn 1 South

            board :: Maybe Board
            board = foldl (\b (p, c) -> addPieceToBoard (DM.fromJust b) p c)
                          (Just emptyBoard)
                          [ (knight, originCoord)
                          , (pawn1, Coord 2 5)
                          , (pawn2, Coord 5 6)
                          , (pawn3, Coord 6 5)
                          , (pawn4, Coord 5 2)]

        board `shouldNotBe` Nothing

        -- fetch moves
        let ms :: [Move]
            ms = validMoves (DM.fromJust board) knight originCoord

        length ms `shouldBe` 4

        let coords :: [Coord]
            coords = map (\m -> spaceCoord $ moveSpace m) ms

        elem (Coord 2 3) coords `shouldBe` True
        --elem (Coord 2 5) coords `shouldBe` True
        elem (Coord 3 6) coords `shouldBe` True
        --elem (Coord 5 6) coords `shouldBe` True
        elem (Coord 6 3) coords `shouldBe` True
        --elem (Coord 6 5) coords `shouldBe` True
        elem (Coord 3 2) coords `shouldBe` True
        --elem (Coord 5 2) coords `shouldBe` True

    it "has valid moves when obstructed by other pieces" $ do

        let emptyBoard = initBoard 9 9 defaultSpaceBuilder
            originCoord = Coord 4 4
            knight = buildTestPiece 1 Knight 1 South
            pawn1 = buildTestPiece 2 Pawn 2 South
            pawn2 = buildTestPiece 3 Pawn 2 South
            pawn3 = buildTestPiece 4 Pawn 2 South
            pawn4 = buildTestPiece 5 Pawn 2 South

            board = foldl (\b (p, c) -> addPieceToBoard (DM.fromJust b) p c)
                          (Just emptyBoard)
                          [ (knight, originCoord)
                          , (pawn1, Coord 2 5)
                          , (pawn2, Coord 5 6)
                          , (pawn3, Coord 6 5)
                          , (pawn4, Coord 5 2)]

        board `shouldNotBe` Nothing
        
        -- fetch moves
        let ms :: [Move]
            ms = validMoves (DM.fromJust board) knight originCoord

        length ms `shouldBe` 8

        let coords :: [Coord]
            coords = map (\m -> spaceCoord $ moveSpace m) ms

        elem (Coord 2 3) coords `shouldBe` True
        elem (Coord 2 5) coords `shouldBe` True
        elem (Coord 3 6) coords `shouldBe` True
        elem (Coord 5 6) coords `shouldBe` True
        elem (Coord 6 3) coords `shouldBe` True
        elem (Coord 6 5) coords `shouldBe` True
        elem (Coord 3 2) coords `shouldBe` True
        elem (Coord 5 2) coords `shouldBe` True

  describe "King" $ do

    it "has valid moves when unobstructed" $ do

        let emptyBoard = initBoard 9 9 defaultSpaceBuilder
            originCoord = Coord 4 4
            king = buildTestPiece 1 King 1 South

            board :: Maybe Board
            board = addPieceToBoard emptyBoard king originCoord

        board `shouldNotBe` Nothing

        -- fetch moves
        let ms :: [Move]
            ms = validMoves (DM.fromJust board) king originCoord

        length ms `shouldBe` 8

        let coords :: [Coord]
            coords = map (\m -> spaceCoord $ moveSpace m) ms

        elem (Coord 3 4) coords `shouldBe` True
        elem (Coord 3 5) coords `shouldBe` True
        elem (Coord 4 5) coords `shouldBe` True
        elem (Coord 5 5) coords `shouldBe` True
        elem (Coord 5 4) coords `shouldBe` True
        elem (Coord 5 3) coords `shouldBe` True
        elem (Coord 4 3) coords `shouldBe` True
        elem (Coord 3 3) coords `shouldBe` True

    it "has valid moves when obstructed by own pieces" $ do

        let emptyBoard = initBoard 9 9 defaultSpaceBuilder
            originCoord = Coord 4 4
            king = buildTestPiece 1 King 1 South
            pawn1 = buildTestPiece 2 Pawn 1 South
            pawn2 = buildTestPiece 3 Pawn 1 South
            pawn3 = buildTestPiece 4 Pawn 1 South
            pawn4 = buildTestPiece 5 Pawn 1 South

            board :: Maybe Board
            board = foldl (\b (p, c) -> addPieceToBoard (DM.fromJust b) p c)
                          (Just emptyBoard)
                          [ (king, originCoord)
                          , (pawn1, Coord 3 4)
                          , (pawn2, Coord 4 5)
                          , (pawn3, Coord 5 4)
                          , (pawn4, Coord 4 3)]

        board `shouldNotBe` Nothing

        -- fetch moves
        let ms :: [Move]
            ms = validMoves (DM.fromJust board) king originCoord

        length ms `shouldBe` 4

        let coords :: [Coord]
            coords = map (\m -> spaceCoord $ moveSpace m) ms

        --elem (Coord 3 4) coords `shouldBe` True
        elem (Coord 3 5) coords `shouldBe` True
        --elem (Coord 4 5) coords `shouldBe` True
        elem (Coord 5 5) coords `shouldBe` True
        --elem (Coord 5 4) coords `shouldBe` True
        elem (Coord 5 3) coords `shouldBe` True
        --elem (Coord 4 3) coords `shouldBe` True
        elem (Coord 3 3) coords `shouldBe` True

    it "has valid moves when obstructed by other pieces" $ do

        let emptyBoard = initBoard 9 9 defaultSpaceBuilder
            originCoord = Coord 4 4
            king = buildTestPiece 1 King 1 South
            pawn1 = buildTestPiece 2 Pawn 2 South
            pawn2 = buildTestPiece 3 Pawn 2 South
            pawn3 = buildTestPiece 4 Pawn 2 South
            pawn4 = buildTestPiece 5 Pawn 2 South

            board = foldl (\b (p, c) -> addPieceToBoard (DM.fromJust b) p c)
                          (Just emptyBoard)
                          [ (king, originCoord)
                          , (pawn1, Coord 3 4)
                          , (pawn2, Coord 4 5)
                          , (pawn3, Coord 5 4)
                          , (pawn4, Coord 4 3)]

        board `shouldNotBe` Nothing
        
        -- fetch moves
        let ms :: [Move]
            ms = validMoves (DM.fromJust board) king originCoord

        length ms `shouldBe` 8

        let coords :: [Coord]
            coords = map (\m -> spaceCoord $ moveSpace m) ms

        elem (Coord 3 4) coords `shouldBe` True
        elem (Coord 3 5) coords `shouldBe` True
        elem (Coord 4 5) coords `shouldBe` True
        elem (Coord 5 5) coords `shouldBe` True
        elem (Coord 5 4) coords `shouldBe` True
        elem (Coord 5 3) coords `shouldBe` True
        elem (Coord 4 3) coords `shouldBe` True
        elem (Coord 3 3) coords `shouldBe` True

    it "has valid moves when threatened by other pieces" $ do

        let emptyBoard = initBoard 9 9 defaultSpaceBuilder
            originCoord = Coord 4 4
            king = buildTestPiece 1 King 1 South
            pawn1 = buildTestPiece 2 Pawn 2 North
            pawn2 = buildTestPiece 3 Pawn 2 North

            board = foldl (\b (p, c) -> addPieceToBoard (DM.fromJust b) p c)
                          (Just emptyBoard)
                          [ (king, originCoord)
                          , (pawn1, Coord 6 3)
                          , (pawn2, Coord 5 6)]

        board `shouldNotBe` Nothing
        
        -- fetch moves
        let ms :: [Move]
            ms = validMoves (DM.fromJust board) king originCoord

        length ms `shouldBe` 6

        let coords :: [Coord]
            coords = map (\m -> spaceCoord $ moveSpace m) ms

        elem (Coord 3 4) coords `shouldBe` True
        elem (Coord 3 5) coords `shouldBe` True
        elem (Coord 4 5) coords `shouldBe` False
        elem (Coord 5 5) coords `shouldBe` True
        elem (Coord 5 4) coords `shouldBe` False
        elem (Coord 5 3) coords `shouldBe` True
        elem (Coord 4 3) coords `shouldBe` True
        elem (Coord 3 3) coords `shouldBe` True
