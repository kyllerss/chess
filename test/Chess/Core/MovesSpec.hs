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
             Nothing


spec :: Spec
spec = describe "Pieces" $ do
  describe "Pawn" $ do

    it "valid moves consists of forward two spots when unobstructed" $ do

        -- initial board setup
        let emptyBoard = initBoard 3 3 defaultSpaceBuilder
            originCoord = Coord 0 1
            pawn = buildTestPiece 1 Pawn 1 South

            board :: Maybe Board
            board = addPieceToBoard pawn originCoord emptyBoard 

        board `shouldNotBe` Nothing

        let ms :: [Move]
            ms = validMoves (DM.fromJust board) (pieceId pawn) originCoord

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
            board = addPieceToBoard pawn originCoord emptyBoard

        board `shouldNotBe` Nothing

        let ms :: [Move]
            ms = validMoves (DM.fromJust board) (pieceId pawn) originCoord

        length ms `shouldBe` 0

    it "can move to valid space vertically when unmoved" $ do
      
        -- initial board setup
        let emptyBoard = initBoard 3 3 defaultSpaceBuilder
            originCoord = Coord 0 1
            pawn = buildTestPiece 1 Pawn 1 South

            board :: Maybe Board
            board = addPieceToBoard pawn originCoord emptyBoard

        board `shouldNotBe` Nothing

        -- move piece
        let destCoord = Coord 1 1
            newBoard = move (pieceId pawn) destCoord (DM.fromJust board) 

        newBoard `shouldNotBe` Nothing

        -- verify spaces still present
        let origSpace, destSpace :: Maybe Space
            origSpace = fetchSpace originCoord (DM.fromJust newBoard)
            destSpace = fetchSpace destCoord (DM.fromJust newBoard)

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
            pawnOrphaned = buildTestPiece 1 Pawn 1 South

            board :: Maybe Board
            board = addPieceToBoard pawnOrphaned originCoord emptyBoard
        
        board `shouldNotBe` Nothing

        -- move piece
        let destCoord = Coord 1 4
            pawn = DM.fromJust $ spacePiece $ DM.fromJust $ fetchSpace originCoord (DM.fromJust board)
            newBoard = move (pieceId pawn) destCoord (DM.fromJust board) 

        newBoard `shouldNotBe` Nothing

        -- verify piece marked as moved
        let destSpace :: Maybe Space
            destSpace = fetchSpace destCoord (DM.fromJust newBoard)

        destSpace `shouldNotBe` Nothing

        let destSpace' :: Space
            destSpace' = DM.fromJust destSpace
            movedPiece = spacePiece destSpace'

        movedPiece `shouldNotBe` Nothing

        let movedPawn :: Piece
            movedPawn = DM.fromJust movedPiece

        pieceMoved movedPawn `shouldBe` True

        let ms :: [Move]
            ms = validMoves (DM.fromJust newBoard) (pieceId movedPawn) (spaceCoord destSpace')

        length ms `shouldBe` 1
        (spaceCoord . moveSpace) (ms !! 0) `shouldBe` Coord 2 4

    it "cannot move forward when obstructed by opponent" $ do

        -- initial board setup
        let emptyBoard = initBoard 3 3 defaultSpaceBuilder
            originCoord = Coord 0 1
            pawn = buildTestPiece 1 Pawn 1 South
            opponent = buildTestPiece 2 Pawn 2 North

            board :: Maybe Board
            board = Just emptyBoard >>=
                    addPieceToBoard pawn originCoord >>=
                    addPieceToBoard opponent (Coord 1 1)

        board `shouldNotBe` Nothing

        let ms :: [Move]
            ms = validMoves (DM.fromJust board) (pieceId pawn) originCoord

        length ms `shouldBe` 0

    it "valid moves contains diagonals when opp present" $ do
      
        -- initial board setup
        let emptyBoard = initBoard 9 9 defaultSpaceBuilder
            originCoord = Coord 1 4
            pawn = buildTestPiece 1 Pawn 1 South
            pawn2 = buildTestPiece 2 Pawn 2 North
            pawn3 = buildTestPiece 3 Pawn 2 North

            board :: Maybe Board
            board = Just emptyBoard >>=
                    addPieceToBoard pawn originCoord >>=
                    addPieceToBoard pawn2 (Coord 2 5) >>=
                    addPieceToBoard pawn3 (Coord 2 3)

        board `shouldNotBe` Nothing

        let ms :: [Move]
            ms = validMoves (DM.fromJust board) (pieceId pawn) originCoord

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
            board = addPieceToBoard pawn originCoord emptyBoard

        board `shouldNotBe` Nothing

        -- move piece
        let destCoord = Coord 1 1
            newBoard = move (pieceId pawn) destCoord (DM.fromJust board) 

        newBoard `shouldBe` Nothing

    it "can do left 'en passant'" $ do

        -- initial board setup
        let emptyBoard = initBoard 4 3 defaultSpaceBuilder
            vPawn = buildTestPiece 1 Pawn 1 South
            aPawn = buildTestPiece 2 Pawn 2 North

            board :: Maybe Board
            board = Just emptyBoard >>=
                    addPieceToBoard vPawn (Coord 0 0) >>=
                    addPieceToBoard aPawn (Coord 3 1) >>=
                    move (pieceId aPawn) (Coord 2 1) >>=
                    move (pieceId vPawn) (Coord 2 0)

        board `shouldNotBe` Nothing

        let ms :: [Move]
            ms = validMoves (DM.fromJust board) (pieceId aPawn) (Coord 2 1)

        length ms `shouldBe` 2

        let coords :: [Coord]
            coords = map (\m -> spaceCoord $ moveSpace m) ms

        elem (Coord 1 0) coords `shouldBe` True
        elem (Coord 1 1) coords `shouldBe` True

    it "can do right 'en passant'" $ do

        -- initial board setup
        let emptyBoard = initBoard 4 3 defaultSpaceBuilder
            vPawn = buildTestPiece 1 Pawn 1 South
            aPawn = buildTestPiece 2 Pawn 2 North

            board :: Maybe Board
            board = Just emptyBoard >>=
                    addPieceToBoard vPawn (Coord 0 2) >>=
                    addPieceToBoard aPawn (Coord 3 1) >>=
                    move (pieceId aPawn) (Coord 2 1) >>=
                    move (pieceId vPawn) (Coord 2 2)

        board `shouldNotBe` Nothing

        let ms :: [Move]
            ms = validMoves (DM.fromJust board) (pieceId aPawn) (Coord 2 1)

        length ms `shouldBe` 2

        let coords :: [Coord]
            coords = map (\m -> spaceCoord $ moveSpace m) ms

        elem (Coord 1 2) coords `shouldBe` True
        elem (Coord 1 1) coords `shouldBe` True
    
    it "looses ability to do 'en passant' if not done immediately" $ do

      pending

  describe "Rook" $ do

    it "has valid moves when unobstructed" $ do

        let emptyBoard = initBoard 9 9 defaultSpaceBuilder
            originCoord = Coord 4 4
            rook = buildTestPiece 1 Rook 1 South

            board :: Maybe Board
            board = addPieceToBoard rook originCoord emptyBoard

        board `shouldNotBe` Nothing

        -- fetch moves
        let ms :: [Move]
            ms = validMoves (DM.fromJust board) (pieceId rook) originCoord

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
            board = Just emptyBoard >>=
                    addPieceToBoard rook originCoord >>=
                    addPieceToBoard pawn1 (Coord 4 7) >>=
                    addPieceToBoard pawn2 (Coord 4 1) >>=
                    addPieceToBoard pawn3 (Coord 7 4) >>=
                    addPieceToBoard pawn4 (Coord 1 4)

        board `shouldNotBe` Nothing

        -- fetch moves
        let ms :: [Move]
            ms = validMoves (DM.fromJust board) (pieceId rook) originCoord

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

            board = Just emptyBoard >>=
                    addPieceToBoard rook originCoord >>=
                    addPieceToBoard pawn1 (Coord 4 7) >>=
                    addPieceToBoard pawn2 (Coord 4 1) >>=
                    addPieceToBoard pawn3 (Coord 7 4) >>=
                    addPieceToBoard pawn4 (Coord 1 4)

        board `shouldNotBe` Nothing
        
        -- fetch moves
        let ms :: [Move]
            ms = validMoves (DM.fromJust board) (pieceId rook) originCoord

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
            board = addPieceToBoard bishop originCoord emptyBoard

        board `shouldNotBe` Nothing

        -- fetch moves
        let ms :: [Move]
            ms = validMoves (DM.fromJust board) (pieceId bishop) originCoord

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
            board = Just emptyBoard >>=
                    addPieceToBoard bishop originCoord >>=
                    addPieceToBoard pawn1 (Coord 2 2) >>=
                    addPieceToBoard pawn2 (Coord 2 6) >>=
                    addPieceToBoard pawn3 (Coord 6 2) >>=
                    addPieceToBoard pawn4 (Coord 6 6)
                    
        board `shouldNotBe` Nothing

        -- fetch moves
        let ms :: [Move]
            ms = validMoves (DM.fromJust board) (pieceId bishop) originCoord

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

            board = Just emptyBoard >>=
                    addPieceToBoard bishop originCoord >>=
                    addPieceToBoard pawn1 (Coord 2 2) >>=
                    addPieceToBoard pawn2 (Coord 2 6) >>=
                    addPieceToBoard pawn3 (Coord 6 2) >>=
                    addPieceToBoard pawn4 (Coord 6 6)

        board `shouldNotBe` Nothing
        
        -- fetch moves
        let ms :: [Move]
            ms = validMoves (DM.fromJust board) (pieceId bishop) originCoord

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
            board = addPieceToBoard queen originCoord emptyBoard

        board `shouldNotBe` Nothing

        -- fetch moves
        let ms :: [Move]
            ms = validMoves (DM.fromJust board) (pieceId queen) originCoord

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
            board = Just emptyBoard >>=
                    addPieceToBoard queen originCoord >>=
                    addPieceToBoard pawn1 (Coord 4 7) >>=
                    addPieceToBoard pawn2 (Coord 4 1) >>=
                    addPieceToBoard pawn3 (Coord 7 4) >>=
                    addPieceToBoard pawn4 (Coord 1 4) >>=
                    addPieceToBoard pawn5 (Coord 2 2) >>=
                    addPieceToBoard pawn6 (Coord 2 6) >>=
                    addPieceToBoard pawn7 (Coord 6 2) >>=
                    addPieceToBoard pawn8 (Coord 6 6)

        board `shouldNotBe` Nothing

        -- fetch moves
        let ms :: [Move]
            ms = validMoves (DM.fromJust board) (pieceId queen) originCoord

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

            board = Just emptyBoard >>=
                    addPieceToBoard queen originCoord >>=
                    addPieceToBoard pawn1 (Coord 4 7) >>=
                    addPieceToBoard pawn2 (Coord 4 1) >>=
                    addPieceToBoard pawn3 (Coord 7 4) >>=
                    addPieceToBoard pawn4 (Coord 1 4) >>=
                    addPieceToBoard pawn5 (Coord 2 2) >>=
                    addPieceToBoard pawn6 (Coord 2 6) >>=
                    addPieceToBoard pawn7 (Coord 6 2) >>=
                    addPieceToBoard pawn8 (Coord 6 6)

        board `shouldNotBe` Nothing
        
        -- fetch moves
        let ms :: [Move]
            ms = validMoves (DM.fromJust board) (pieceId queen) originCoord

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
            board = addPieceToBoard knight originCoord emptyBoard

        board `shouldNotBe` Nothing

        -- fetch moves
        let ms :: [Move]
            ms = validMoves (DM.fromJust board) (pieceId knight) originCoord

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
            board = Just emptyBoard >>=
                    addPieceToBoard knight originCoord >>=
                    addPieceToBoard pawn1 (Coord 2 5) >>=
                    addPieceToBoard pawn2 (Coord 5 6) >>=
                    addPieceToBoard pawn3 (Coord 6 5) >>=
                    addPieceToBoard pawn4 (Coord 5 2)

        board `shouldNotBe` Nothing

        -- fetch moves
        let ms :: [Move]
            ms = validMoves (DM.fromJust board) (pieceId knight) originCoord

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

            board = Just emptyBoard >>=
                    addPieceToBoard knight originCoord >>=
                    addPieceToBoard pawn1 (Coord 2 5) >>=
                    addPieceToBoard pawn2 (Coord 5 6) >>=
                    addPieceToBoard pawn3 (Coord 6 5) >>=
                    addPieceToBoard pawn4 (Coord 5 2)

        board `shouldNotBe` Nothing
        
        -- fetch moves
        let ms :: [Move]
            ms = validMoves (DM.fromJust board) (pieceId knight) originCoord

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
            board = addPieceToBoard king originCoord emptyBoard

        board `shouldNotBe` Nothing

        -- fetch moves
        let ms :: [Move]
            ms = validMoves (DM.fromJust board) (pieceId king) originCoord

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
            board = Just emptyBoard >>=
                    addPieceToBoard king originCoord >>=
                    addPieceToBoard pawn1 (Coord 3 4) >>=
                    addPieceToBoard pawn2 (Coord 4 5) >>=
                    addPieceToBoard pawn3 (Coord 5 4) >>=
                    addPieceToBoard pawn4 (Coord 4 3)

        board `shouldNotBe` Nothing

        -- fetch moves
        let ms :: [Move]
            ms = validMoves (DM.fromJust board) (pieceId king) originCoord

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

    it "has valid moves when obstructed by opponent pieces" $ do

        let emptyBoard = initBoard 9 9 defaultSpaceBuilder
            originCoord = Coord 4 4
            king = buildTestPiece 1 King 1 South
            pawn1 = buildTestPiece 2 Pawn 2 South
            pawn2 = buildTestPiece 3 Pawn 2 South
            pawn3 = buildTestPiece 4 Pawn 2 South
            pawn4 = buildTestPiece 5 Pawn 2 South

            board = Just emptyBoard >>=
                    addPieceToBoard king originCoord >>=
                    addPieceToBoard pawn1 (Coord 3 4) >>=
                    addPieceToBoard pawn2 (Coord 4 5) >>=
                    addPieceToBoard pawn3 (Coord 5 4) >>=
                    addPieceToBoard pawn4 (Coord 4 3)

        board `shouldNotBe` Nothing
        
        -- fetch moves
        let ms :: [Move]
            ms = validMoves (DM.fromJust board) (pieceId king) originCoord

        length ms `shouldBe` 5

        let coords :: [Coord]
            coords = map (\m -> spaceCoord $ moveSpace m) ms

        elem (Coord 3 3) coords `shouldBe` True
        elem (Coord 3 4) coords `shouldBe` True
        elem (Coord 3 5) coords `shouldBe` True
        elem (Coord 4 3) coords `shouldBe` False
        elem (Coord 4 5) coords `shouldBe` False
        elem (Coord 5 3) coords `shouldBe` True
        elem (Coord 5 4) coords `shouldBe` False
        elem (Coord 5 5) coords `shouldBe` True

    it "cannot move to open space if results in check" $ do

        let emptyBoard = initBoard 9 9 defaultSpaceBuilder
            originCoord = Coord 4 4
            king = buildTestPiece 1 King 1 South
            pawn1 = buildTestPiece 2 Pawn 2 North
            pawn2 = buildTestPiece 3 Pawn 2 North

            board = Just emptyBoard >>=
                    addPieceToBoard king originCoord >>=
                    addPieceToBoard pawn1 (Coord 6 3) >>=
                    addPieceToBoard pawn2 (Coord 5 6)

        board `shouldNotBe` Nothing
        
        -- fetch moves
        let ms :: [Move]
            ms = validMoves (DM.fromJust board) (pieceId king) originCoord

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

    it "cannot move to opponent space if results in check" $ do

        let emptyBoard = initBoard 9 9 defaultSpaceBuilder
            originCoord = Coord 4 4
            king = buildTestPiece 1 King 1 South
            pawn1 = buildTestPiece 2 Pawn 2 North
            pawn2 = buildTestPiece 3 Pawn 2 North

            board = Just emptyBoard >>=
                    addPieceToBoard king originCoord >>=
                    addPieceToBoard pawn1 (Coord 5 4) >>=
                    addPieceToBoard pawn2 (Coord 6 5)

        board `shouldNotBe` Nothing
        
        -- fetch moves
        let ms :: [Move]
            ms = validMoves (DM.fromJust board) (pieceId king) originCoord

        length ms `shouldBe` 5

        let coords :: [Coord]
            coords = map (\m -> spaceCoord $ moveSpace m) ms

        elem (Coord 3 3) coords `shouldBe` True
        elem (Coord 3 4) coords `shouldBe` True
        elem (Coord 3 5) coords `shouldBe` True
        elem (Coord 4 3) coords `shouldBe` False
        elem (Coord 4 5) coords `shouldBe` False
        elem (Coord 5 3) coords `shouldBe` True
        elem (Coord 5 4) coords `shouldBe` False
        elem (Coord 5 5) coords `shouldBe` True

    it "can castle when unobstructed (vertical orientation)" $ do

        let emptyBoard = initBoard 8 8 defaultSpaceBuilder
            originCoord = Coord 7 4
            king = buildTestPiece 1 King 1 North
            rook1 = buildTestPiece 2 Rook 1 North
            rook2 = buildTestPiece 3 Rook 1 North

            board = Just emptyBoard >>=
                    addPieceToBoard king originCoord >>= 
                    addPieceToBoard rook1 (Coord 7 0) >>= 
                    addPieceToBoard rook2 (Coord 7 7)

        {-
            board = do
                       b <- Just emptyBoard
                       b' <- addPieceToBoard b king originCoord
                       b'' <- addPieceToBoard b' rook1 $ Coord 7 0
                       addPieceToBoard b'' rook2 $ Coord 7 7
         -}
        board `shouldNotBe` Nothing
        
        -- fetch moves
        let ms :: [Move]
            ms = validMoves (DM.fromJust board) (pieceId king) originCoord

        length ms `shouldBe` 7

        let coords :: [Coord]
            coords = map (\m -> spaceCoord $ moveSpace m) ms

        elem (Coord 7 2) coords `shouldBe` True
        elem (Coord 7 3) coords `shouldBe` True
        elem (Coord 6 3) coords `shouldBe` True
        elem (Coord 6 4) coords `shouldBe` True
        elem (Coord 6 5) coords `shouldBe` True
        elem (Coord 7 5) coords `shouldBe` True
        elem (Coord 7 6) coords `shouldBe` True

        -- verify move works (rook1)
        let boardL :: Maybe Board
            boardL = move (pieceId king) (Coord 7 2) (DM.fromJust board) 

        boardL `shouldNotBe` Nothing

        let king1Space, rook1Space :: Maybe Space
            king1Space = fetchPieceSpace king (DM.fromJust boardL)
            rook1Space = fetchPieceSpace rook1 (DM.fromJust boardL)

        king1Space `shouldNotBe` Nothing
        spaceCoord (DM.fromJust king1Space) `shouldBe` Coord 7 2
        
        rook1Space `shouldNotBe` Nothing
        spaceCoord (DM.fromJust rook1Space) `shouldBe` Coord 7 3

        let rook1Piece, king1Piece :: Piece
            rook1Piece = DM.fromJust $ fetchPiece (Coord 7 3) (DM.fromJust boardL) 
            king1Piece = DM.fromJust $ fetchPiece (Coord 7 2) (DM.fromJust boardL) 

        pieceMoved rook1Piece `shouldBe` True
        pieceMoved king1Piece `shouldBe` True
        
        -- verify move works (rook2)
        let boardR :: Maybe Board
            boardR = move (pieceId king) (Coord 7 6) (DM.fromJust board) 

        boardR `shouldNotBe` Nothing

        let king2Space, rook2Space :: Maybe Space
            king2Space = fetchPieceSpace king (DM.fromJust boardR) 
            rook2Space = fetchPieceSpace rook2 (DM.fromJust boardR) 

        king2Space `shouldNotBe` Nothing
        spaceCoord (DM.fromJust king2Space) `shouldBe` Coord 7 6
        
        rook2Space `shouldNotBe` Nothing
        spaceCoord (DM.fromJust rook2Space) `shouldBe` Coord 7 5

    it "can castle when unobstructed (horizontal orientation)" $ do

        let emptyBoard = initBoard 8 8 defaultSpaceBuilder
            originCoord = Coord 4 7
            king = buildTestPiece 1 King 1 East
            rook1 = buildTestPiece 2 Rook 1 East
            rook2 = buildTestPiece 3 Rook 1 East

            board = Just emptyBoard >>=
                    addPieceToBoard king originCoord >>= 
                    addPieceToBoard rook1 (Coord 0 7) >>= 
                    addPieceToBoard rook2 (Coord 7 7)

        board `shouldNotBe` Nothing
        
        -- fetch moves
        let ms :: [Move]
            ms = validMoves (DM.fromJust board) (pieceId king) originCoord

        length ms `shouldBe` 7

        let coords :: [Coord]
            coords = map (\m -> spaceCoord $ moveSpace m) ms

        elem (Coord 2 7) coords `shouldBe` True
        elem (Coord 3 7) coords `shouldBe` True
        elem (Coord 3 6) coords `shouldBe` True
        elem (Coord 4 6) coords `shouldBe` True
        elem (Coord 5 6) coords `shouldBe` True
        elem (Coord 5 7) coords `shouldBe` True
        elem (Coord 6 7) coords `shouldBe` True

        -- verify move works (rook1)
        let boardL :: Maybe Board
            boardL = move (pieceId king) (Coord 2 7) (DM.fromJust board) 

        boardL `shouldNotBe` Nothing

        let king1Space, rook1Space :: Maybe Space
            king1Space = fetchPieceSpace king (DM.fromJust boardL) 
            rook1Space = fetchPieceSpace rook1 (DM.fromJust boardL) 

        king1Space `shouldNotBe` Nothing
        spaceCoord (DM.fromJust king1Space) `shouldBe` Coord 2 7
        
        rook1Space `shouldNotBe` Nothing
        spaceCoord (DM.fromJust rook1Space) `shouldBe` Coord 3 7

        let rook1Piece, king1Piece :: Piece
            rook1Piece = DM.fromJust $ fetchPiece (Coord 3 7) (DM.fromJust boardL) 
            king1Piece = DM.fromJust $ fetchPiece (Coord 2 7) (DM.fromJust boardL) 

        pieceMoved rook1Piece `shouldBe` True
        pieceMoved king1Piece `shouldBe` True
        
        -- verify move works (rook2)
        let boardR :: Maybe Board
            boardR = move (pieceId king) (Coord 6 7) (DM.fromJust board) 

        boardR `shouldNotBe` Nothing

        let king2Space, rook2Space :: Maybe Space
            king2Space = fetchPieceSpace king (DM.fromJust boardR) 
            rook2Space = fetchPieceSpace rook2 (DM.fromJust boardR) 

        king2Space `shouldNotBe` Nothing
        spaceCoord (DM.fromJust king2Space) `shouldBe` Coord 6 7
        
        rook2Space `shouldNotBe` Nothing
        spaceCoord (DM.fromJust rook2Space) `shouldBe` Coord 5 7
      
    it "cannot castle when obstructed" $ do

        let emptyBoard = initBoard 8 8 defaultSpaceBuilder
            originCoord = Coord 7 4
            king = buildTestPiece 1 King 1 North
            rook1 = buildTestPiece 2 Rook 1 North
            rook2 = buildTestPiece 3 Rook 1 North
            pawn1 = buildTestPiece 4 Pawn 1 North
            pawn2 = buildTestPiece 5 Pawn 1 North
        
            board = Just emptyBoard >>=
                    addPieceToBoard king originCoord >>= 
                    addPieceToBoard rook1 (Coord 7 0) >>= 
                    addPieceToBoard rook2 (Coord 7 7) >>=
                    addPieceToBoard pawn1 (Coord 7 1) >>=
                    addPieceToBoard pawn2 (Coord 7 6)

        board `shouldNotBe` Nothing
        
        -- fetch moves
        let ms :: [Move]
            ms = validMoves (DM.fromJust board) (pieceId king) originCoord

        length ms `shouldBe` 5

        let coords :: [Coord]
            coords = map (\m -> spaceCoord $ moveSpace m) ms

        elem (Coord 7 2) coords `shouldBe` False
        elem (Coord 7 3) coords `shouldBe` True
        elem (Coord 6 3) coords `shouldBe` True
        elem (Coord 6 4) coords `shouldBe` True
        elem (Coord 6 5) coords `shouldBe` True
        elem (Coord 7 5) coords `shouldBe` True
        elem (Coord 7 6) coords `shouldBe` False

        -- verify move works (rook1)
        let boardL :: Maybe Board
            boardL = move (pieceId king) (Coord 7 2) (DM.fromJust board) 

        boardL `shouldBe` Nothing

        -- verify move works (rook2)
        let boardR :: Maybe Board
            boardR = move (pieceId king) (Coord 7 6) (DM.fromJust board) 

        boardR `shouldBe` Nothing

    it "cannot castle when king's move spaces in between threatened" $ do

        let emptyBoard = initBoard 8 8 defaultSpaceBuilder
            originCoord = Coord 7 4
            king = buildTestPiece 1 King 1 North
            rook1 = buildTestPiece 2 Rook 1 North
            rook2 = buildTestPiece 3 Rook 1 North
            rook3 = buildTestPiece 4 Rook 2 South
            rook4 = buildTestPiece 5 Rook 2 South
        
            board = Just emptyBoard >>=
                    addPieceToBoard king originCoord >>= 
                    addPieceToBoard rook1 (Coord 7 0) >>= 
                    addPieceToBoard rook2 (Coord 7 7) >>=
                    addPieceToBoard rook3 (Coord 5 3) >>=
                    addPieceToBoard rook4 (Coord 5 5)

        board `shouldNotBe` Nothing
        
        -- fetch moves
        let ms :: [Move]
            ms = validMoves (DM.fromJust board) (pieceId king) originCoord
            --ms = T.traceShow (threatenedSpaces board $ piecePlayer king) $ validMoves (DM.fromJust board) king originCoord
            --ms = T.traceShow board $ validMoves (DM.fromJust board) king originCoord
            
        length ms `shouldBe` 1

        let coords :: [Coord]
            coords = map (\m -> spaceCoord $ moveSpace m) ms

        elem (Coord 7 2) coords `shouldBe` False
        elem (Coord 7 3) coords `shouldBe` False
        elem (Coord 6 3) coords `shouldBe` False
        elem (Coord 6 4) coords `shouldBe` True
        elem (Coord 6 5) coords `shouldBe` False
        elem (Coord 7 5) coords `shouldBe` False
        elem (Coord 7 6) coords `shouldBe` False

        -- verify move works (rook1)
        let boardL :: Maybe Board
            boardL = move (pieceId king) (Coord 2 7) (DM.fromJust board) 

        boardL `shouldBe` Nothing

        -- verify move works (rook2)
        let boardR :: Maybe Board
            boardR = move (pieceId king) (Coord 6 7) (DM.fromJust board) 

        boardR `shouldBe` Nothing

    it "can castle when rook's move spaces in between threatened" $ do

        let emptyBoard = initBoard 8 8 defaultSpaceBuilder
            originCoord = Coord 7 4
            king = buildTestPiece 1 King 1 North
            rook1 = buildTestPiece 2 Rook 1 North
            rook2 = buildTestPiece 3 Rook 1 North
            rook3 = buildTestPiece 4 Rook 2 South
            rook4 = buildTestPiece 5 Rook 2 South
        
            board = Just emptyBoard >>=
                    addPieceToBoard king originCoord >>= 
                    addPieceToBoard rook1 (Coord 7 0) >>= 
                    addPieceToBoard rook2 (Coord 7 7) >>=
                    addPieceToBoard rook3 (Coord 5 1) >>=
                    addPieceToBoard rook4 (Coord 5 7)

        board `shouldNotBe` Nothing
        
        -- fetch moves
        let ms :: [Move]
            ms = validMoves (DM.fromJust board) (pieceId king) originCoord
            
        length ms `shouldBe` 7

        let coords :: [Coord]
            coords = map (\m -> spaceCoord $ moveSpace m) ms

        elem (Coord 7 2) coords `shouldBe` True
        elem (Coord 7 3) coords `shouldBe` True
        elem (Coord 6 3) coords `shouldBe` True
        elem (Coord 6 4) coords `shouldBe` True
        elem (Coord 6 5) coords `shouldBe` True
        elem (Coord 7 5) coords `shouldBe` True
        elem (Coord 7 6) coords `shouldBe` True

        -- verify move works (rook1)
        let boardL :: Maybe Board
            boardL = move (pieceId king) (Coord 7 2) (DM.fromJust board) 

        boardL `shouldNotBe` Nothing

        -- verify move works (rook2)
        let boardR :: Maybe Board
            boardR = move (pieceId king) (Coord 7 6) (DM.fromJust board) 

        boardR `shouldNotBe` Nothing

    it "cannot castle when result is check" $ do

        let emptyBoard = initBoard 8 8 defaultSpaceBuilder
            originCoord = Coord 7 4
            king = buildTestPiece 1 King 1 North
            rook1 = buildTestPiece 2 Rook 1 North
            rook2 = buildTestPiece 3 Rook 1 North
            rook3 = buildTestPiece 4 Rook 2 South
            rook4 = buildTestPiece 5 Rook 2 South
        
            board = Just emptyBoard >>=
                    addPieceToBoard king originCoord >>= 
                    addPieceToBoard rook1 (Coord 7 0) >>= 
                    addPieceToBoard rook2 (Coord 7 7) >>=
                    addPieceToBoard rook3 (Coord 5 2) >>=
                    addPieceToBoard rook4 (Coord 5 6)

        board `shouldNotBe` Nothing
        
        -- fetch moves
        let ms :: [Move]
            ms = validMoves (DM.fromJust board) (pieceId king) originCoord

        length ms `shouldBe` 5

        let coords :: [Coord]
            coords = map (\m -> spaceCoord $ moveSpace m) ms

        elem (Coord 7 2) coords `shouldBe` False
        elem (Coord 7 3) coords `shouldBe` True
        elem (Coord 6 3) coords `shouldBe` True
        elem (Coord 6 4) coords `shouldBe` True
        elem (Coord 6 5) coords `shouldBe` True
        elem (Coord 7 5) coords `shouldBe` True
        elem (Coord 7 6) coords `shouldBe` False

        -- verify move works (rook1)
        let boardL :: Maybe Board
            boardL = move (pieceId king) (Coord 7 2) (DM.fromJust board) 

        boardL `shouldBe` Nothing

        -- verify move works (rook2)
        let boardR :: Maybe Board
            boardR = move (pieceId king) (Coord 7 6) (DM.fromJust board) 

        boardR `shouldBe` Nothing
     
    it "cannot castle when pieces previously moved" $ do

        let emptyBoard = initBoard 8 8 defaultSpaceBuilder
            originCoord = Coord 7 4
            king = buildTestPiece 1 King 1 North
            rook1 = buildTestPiece 2 Rook 1 North
            rook2 = buildTestPiece 3 Rook 1 North
        
            board = Just emptyBoard >>=
                    addPieceToBoard king originCoord >>= 
                    addPieceToBoard rook1 (Coord 7 0) >>= 
                    addPieceToBoard rook2 (Coord 7 7) >>=
                    move (pieceId rook1) (Coord 6 0) >>=
                    move (pieceId rook1) (Coord 7 0) >>=
                    move (pieceId rook2) (Coord 6 7) >>=
                    move (pieceId rook2) (Coord 7 7) 

        board `shouldNotBe` Nothing
        
        -- fetch moves
        let ms :: [Move]
            ms = validMoves (DM.fromJust board) (pieceId king) originCoord

        length ms `shouldBe` 5

        let coords :: [Coord]
            coords = map (\m -> spaceCoord $ moveSpace m) ms

        elem (Coord 7 2) coords `shouldBe` False
        elem (Coord 7 3) coords `shouldBe` True
        elem (Coord 6 3) coords `shouldBe` True
        elem (Coord 6 4) coords `shouldBe` True
        elem (Coord 6 5) coords `shouldBe` True
        elem (Coord 7 5) coords `shouldBe` True
        elem (Coord 7 6) coords `shouldBe` False

        -- verify move doesn't work (castle left)
        let boardL :: Maybe Board
            boardL = move (pieceId king) (Coord 7 2) (DM.fromJust board) 

        boardL `shouldBe` Nothing

        -- verify move doesn't work (castle right)
        let boardR :: Maybe Board
            boardR = move (pieceId king) (Coord 7 6) (DM.fromJust board) 

        boardR `shouldBe` Nothing
      
    it "cannot have another piece move (pinned) if results in check" $ do

        let emptyBoard = initBoard 4 3 defaultSpaceBuilder
            originCoord = Coord 3 1
            king = buildTestPiece 1 King 1 North
            rook1 = buildTestPiece 2 Rook 1 North
            rook2 = buildTestPiece 3 Rook 2 South
        
            board = Just emptyBoard >>=
                    addPieceToBoard king originCoord >>= 
                    addPieceToBoard rook1 (Coord 2 1) >>= -- pinned 
                    addPieceToBoard rook2 (Coord 0 1)     -- opponent

        board `shouldNotBe` Nothing
        
        -- fetch moves
        let ms :: [Move]
            ms = validMoves (DM.fromJust board) (pieceId king) originCoord

        length ms `shouldBe` 4

        let coords :: [Coord]
            coords = map (\m -> spaceCoord $ moveSpace m) ms

        elem (Coord 3 0) coords `shouldBe` True
        elem (Coord 3 2) coords `shouldBe` True
        elem (Coord 2 0) coords `shouldBe` True
        elem (Coord 2 2) coords `shouldBe` True

        -- move pinned rook
        let newBoard :: Maybe Board
            newBoard = move (pieceId rook1) (Coord 2 0) (DM.fromJust board)

        newBoard `shouldBe` Nothing

    it "is in check intially and move blocks check" $ do

      pending
      
{- GHCi Test Commands

:set +m
let
    isOpponent :: Space -> Player -> Bool
    isOpponent s player = (Data.Maybe.isJust $ spacePiece s)
                   && (piecePlayer (Data.Maybe.fromJust $ spacePiece s) /= player)

let
    oppCoords :: Maybe Board -> Player -> [(Piece, Coord)]
    oppCoords Nothing _ = []
    oppCoords (Just Board{spacesMap = spsMap'}) pp =
      Data.Foldable.foldr (\spc acc -> if isOpponent spc pp
                         then (Data.Maybe.fromJust (spacePiece spc), spaceCoord spc) : acc
                         else acc)
                      []
                      spsMap'

:set +m
let
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
             Nothing

let
            emptyBoard = initBoard 4 3 defaultSpaceBuilder
            originCoord = Coord 3 1
            king = buildTestPiece 1 King 1 North
            rook1 = buildTestPiece 2 Rook 1 North
            rook2 = buildTestPiece 3 Rook 2 South
            board = Just emptyBoard >>=
                    addPieceToBoard king originCoord >>=
                    addPieceToBoard rook1 (Coord 2 1) >>=
                    addPieceToBoard rook2 (Coord 0 1)


threatenedSpaces (transfer (Data.Maybe.fromJust board) rook1 (fetchPieceSpace rook1 $ Data.Maybe.fromJust board) (fetchSpace (Coord 2 0) $ Data.Maybe.fromJust board)) (piecePlayer rook1)
isOverlapSpace (Coord 2 0) $
move rook1 (Coord 2 0) (Data.Maybe.fromJust board)

-}
