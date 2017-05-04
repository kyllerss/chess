module Chess.Core.Logic.GameStateSpec ( spec ) where

import Import
import           Test.Hspec
import           Chess.TestUtils
import           Chess.Core.Domain.Base
import           Chess.Core.Domain.Board
import           Chess.Core.Domain.Coord
import           Chess.Core.Domain.GameState
import           Chess.Core.Domain.Move
import           Chess.Core.Domain.Piece
import           Chess.Core.Domain.Player
import qualified Chess.Core.Domain.Space as S
import Data.Maybe (fromJust)
import qualified Data.List as DL
import qualified Data.Map as Map

hasStandardPieces :: Maybe PlayerBoardProfile -> Bool
hasStandardPieces Nothing = False
hasStandardPieces (Just profile) = hasRooks && hasKnights && hasBishops && hasQueen && hasKing && hasPawns
  where
    validMakeup :: PieceType -> Int -> Bool
    validMakeup pt cnt = maybe False (\m -> DL.length m == cnt) $ Map.lookup pt profile
    
    hasRooks, hasKnights, hasBishops, hasQueen, hasKing, hasPawns :: Bool
    hasRooks = validMakeup Rook 2
    hasKnights = validMakeup Knight 2
    hasBishops = validMakeup Bishop 2
    hasQueen = validMakeup Queen 1
    hasKing = validMakeup King 1
    hasPawns = validMakeup Pawn 8
    
spec :: Spec
spec = describe "Game" $ do
  describe "standard board" $ do

    -- initial board setup
    let player1, player2 :: Player
        player1 = Player { playerName = pack "Player 1"
                         , playerId = 1
                         , playerType = Human
                         , playerDirection = North
                         }
        player2 = Player { playerName = pack "Player 2"
                         , playerId = 2
                         , playerType = Human
                         , playerDirection = South
                         }
        gameState :: GameState
        gameState = initGame Standard [player1, player2] 

        boardProfile :: BoardProfile
        boardProfile = buildBoardProfileMap gameState
    
    it "has two players" $ do
      players gameState `shouldBe` [player1, player2]

    it "doesn't have any recorded moves" $ do
     (boardMoves . board) gameState `shouldBe` []

    it "player turn should be first player" $ do
      playerTurn gameState `shouldBe` player1

    it "should have standard pieces" $ do
      (Map.size . spacesMap . board) gameState `shouldNotBe` 0
      boardProfile `shouldNotBe` Map.empty

      let profile1 = Map.lookup player1 boardProfile 
          profile2 = Map.lookup player2 boardProfile

      hasStandardPieces profile1 `shouldBe` True
      hasStandardPieces profile2 `shouldBe` True
      
    it "should be able to move a piece once" $ do
      let pawn1 = DL.head $ boardProfile Map.! player1 Map.! Pawn
          space1 = fetchPieceSpace pawn1 (board gameState)

      space1 `shouldNotBe` Nothing

      let coord1 = fromJust $ S.spaceCoord <$> space1
          dir1 = playerDirection player1
          nextCoord1 = moveD coord1 dir1 1 
          gameState' = applyMove (pieceId pawn1) nextCoord1 gameState

      gameState' `shouldNotBe` Nothing

    it "should be able to move two pieces" $ do
      let pawn1 = DL.head $ boardProfile Map.! player1 Map.! Pawn
          space1 = fetchPieceSpace pawn1 (board gameState)

      space1 `shouldNotBe` Nothing

      let coord1 = fromJust $ S.spaceCoord <$> space1
          dir1 = playerDirection player1
          nextCoord1 = moveD coord1 dir1 1 
          gameStateMaybe = applyMove (pieceId pawn1) nextCoord1 gameState

      gameStateMaybe `shouldNotBe` Nothing

      let gameState' = fromJust gameStateMaybe
          pawn2 = DL.head $ boardProfile Map.! player2 Map.! Pawn
          space2 = fetchPieceSpace pawn2 (board gameState')

      space2 `shouldNotBe` Nothing

      let coord2 = fromJust $ S.spaceCoord <$> space2
          dir2 = playerDirection player2
          nextCoord2 = moveD coord2 dir2 1 
          gameStateMaybe' = applyMove (pieceId pawn2) nextCoord2 gameState'

      gameStateMaybe' `shouldNotBe` Nothing

    it "should not crash when two players move exposing king (minimal board)" $ do

      let emptyGame = initGameEmpty 1 6 [player1, player2] player1
          pawnA = buildTestPiece 1 Pawn 1 North
          kingA = buildTestPiece 4 King 1 North
          pawnB = buildTestPiece 11 Pawn 2 South
          kingB = buildTestPiece 14 King 2 South
          
          initialGame :: Maybe GameState
          initialGame = Just emptyGame >>=
                      addPiece kingB (Coord 0 0) >>=
                      addPiece pawnB (Coord 1 0) >>=
                      addPiece kingA (Coord 5 0) >>=
                      addPiece pawnA (Coord 4 0) 

      initialGame `shouldNotBe` Nothing

      let newGame = initialGame >>=
                    applyMove (pieceId pawnA) (Coord 3 0) >>=
                    applyMove (pieceId pawnB) (Coord 2 0)

      newGame `shouldNotBe` Nothing
      
    it "should not crash when two players move exposing king (standard board)" $ do
    
      let emptyGame = initGameEmpty 3 6 [player1, player2] player1
          pawnA1 = buildTestPiece 1 Pawn 1 North
          pawnA2 = buildTestPiece 2 Pawn 1 North
          pawnA3 = buildTestPiece 3 Pawn 1 North
          kingA = buildTestPiece 4 King 1 North
          pawnB1 = buildTestPiece 11 Pawn 2 South
          pawnB2 = buildTestPiece 12 Pawn 2 South
          pawnB3 = buildTestPiece 13 Pawn 2 South
          kingB = buildTestPiece 14 King 2 South
          
          initialGame :: Maybe GameState
          initialGame = Just emptyGame >>=
                      addPiece kingB (Coord 0 1) >>=
                      addPiece pawnB1 (Coord 1 0) >>=
                      addPiece pawnB2 (Coord 1 1) >>=
                      addPiece pawnB3 (Coord 1 2) >>=
                      addPiece kingA (Coord 5 1) >>=
                      addPiece pawnA1 (Coord 4 0) >>=
                      addPiece pawnA2 (Coord 4 1) >>=
                      addPiece pawnA3 (Coord 4 2)

      initialGame `shouldNotBe` Nothing

      let newGame = initialGame >>=
                    applyMove (pieceId pawnA2) (Coord 3 1) >>=
                    applyMove (pieceId pawnB2) (Coord 2 1)

      newGame `shouldNotBe` Nothing

    it "should allow pawn to be promoted to Queen" $ do

      let emptyGame = initGameEmpty 2 3 [player1, player2] player1
          pawnA1 = buildTestPiece 1 Pawn 1 North
          pawnB1 = buildTestPiece 11 Pawn 2 South

      
          initialGame :: Maybe GameState
          initialGame = Just emptyGame >>=
                        addPiece pawnA1 (Coord 2 0) >>=
                        addPiece pawnB1 (Coord 0 1) >>=
                        updateGameSpaceSideEffect (Coord 0 0) S.PawnPromotion >>=
                        updateGameSpaceSideEffect (Coord 0 1) S.PawnPromotion >>=
                        updateGameSpaceSideEffect (Coord 2 0) S.PawnPromotion >>=
                        updateGameSpaceSideEffect (Coord 2 1) S.PawnPromotion

      initialGame `shouldNotBe` Nothing

      let updatedGame = initialGame >>=
                        applyMove (pieceId pawnA1) (Coord 0 0)

      updatedGame `shouldNotBe` Nothing

      let spaceQ :: S.Space
          spaceQ = fromJust $ (fetchSpace (Coord 0 0) (board $ fromJust updatedGame))
          spaceP = fromJust $ (fetchSpace (Coord 0 1) (board $ fromJust updatedGame))
          
      pieceType <$> S.spacePiece spaceQ `shouldBe` Just Queen
      pieceType <$> S.spacePiece spaceP `shouldBe` Just Pawn

    {-
        ...k      ...k      xx.k      xx.k
        r...  ->  r...  ->  xxxr  ->  xx.r 
        ..q.      ...q      xxxq      xxx.
        ....      ....      ....      xxxq
    -}
    it "should only offer valid moves when King is in check" $ do
      
      let emptyGame = initGameEmpty 4 4 [player1, player2] player2 -- queen starts
          kingA = buildTestPiece 1 King 1 North
          rookA = buildTestPiece 2 Rook 1 North 
          queenB = buildTestPiece 11 Queen 2 South

          initialGame :: Maybe GameState
          initialGame = Just emptyGame >>=
                        addPiece kingA (Coord 0 3) >>=
                        addPiece rookA (Coord 1 0) >>=
                        addPiece queenB (Coord 2 2)

      initialGame `shouldNotBe` Nothing

      let queenMovedState :: Maybe GameState
          queenMovedState = initialGame >>= applyMove (pieceId queenB) (Coord 2 3)

      queenMovedState `shouldNotBe` Nothing
      playerTurn <$> queenMovedState `shouldBe` Just player1

      let mvs :: [Move]
          mvs = moves $ fromJust queenMovedState

      length mvs `shouldBe` 2

      let mvts :: [(PieceId, Coord)]
          mvts = foldl' (\acc m -> (movePieceId m, S.spaceCoord $ moveSpace m) : acc) [] mvs

      (elem (pieceId rookA, Coord 1 3) mvts) `shouldBe` True
      (elem (pieceId kingA, Coord 0 2) mvts) `shouldBe` True

      -- Rook should be pinned!
      let queenMovedState' :: Maybe GameState
          queenMovedState' = queenMovedState >>=
                             applyMove (pieceId rookA) (Coord 1 3) >>= -- block check
                             applyMove (pieceId queenB) (Coord 3 3)    -- move back one square

      queenMovedState' `shouldNotBe` Nothing
      playerTurn <$> queenMovedState' `shouldBe` Just player1

      let mvs' :: [Move]
          mvs' = moves $ fromJust queenMovedState'

      length mvs' `shouldBe` 4

      let mvts' :: [(PieceId, Coord)]
          mvts' = foldl' (\acc m -> (movePieceId m, S.spaceCoord $ moveSpace m) : acc) [] mvs'

      (elem (pieceId kingA, Coord 0 2) mvts') `shouldBe` True
      (elem (pieceId kingA, Coord 1 2) mvts') `shouldBe` True
      (elem (pieceId rookA, Coord 2 3) mvts') `shouldBe` True
      (elem (pieceId rookA, Coord 3 3) mvts') `shouldBe` True
      
    it "pawn should consume 'en-passant' when opportunity presented" $ do
      let n = Nothing :: Maybe Int
      n `shouldNotBe` Nothing

    it "should allow player 2 to castle" $ do

      let rookA = buildTestPiece 1 Rook 1 North
          kingA = buildTestPiece 4 King 1 North
          rookB = buildTestPiece 11 Rook 2 South
          kingB = buildTestPiece 14 King 2 South
          
          initialGame :: Maybe GameState
          initialGame = initGameBare 8
                                     8
                                     [(rookB, (Coord 0 7))
                                     , (kingB, (Coord 0 4))
                                     , (rookA, (Coord 7 0))
                                     , (kingA, (Coord 7 4))]                                     
                                     [player1, player2]
                                     player1 
      hmmm still failing but test passes
      initialGame `shouldNotBe` Nothing

      let mvs :: [Move]
          mvs = moves $ fromJust initialGame

      length mvs `shouldBe` 16

      {-
      let newGame = initialGame >>=
                    applyMove (pieceId pawnA) (Coord 3 0) >>=
                    applyMove (pieceId pawnB) (Coord 2 0)

      newGame `shouldNotBe` Nothing
      -}
    {-
    it "should not crash when king move enabled" $ do
      let coord1 = Coord 6 4
          pawn1Maybe = fetchPiece coord1 (board gameState)

      pawn1Maybe `shouldNotBe` Nothing

      let pawn1 = fromJust pawn1Maybe
          dir1 = playerDirection player1
          nextCoord1 = moveD coord1 dir1 1
          gameStateMaybe = applyMove (pieceId pawn1) nextCoord1 gameState

      gameStateMaybe `shouldNotBe` Nothing

      let gameState' = fromJust gameStateMaybe
          coord2 = Coord 1 4
          pawn2Maybe = fetchPiece coord2 (board gameState')

      pawn2Maybe `shouldNotBe` Nothing

      let pawn2 = fromJust pawn2Maybe
          dir2 = playerDirection player2
          nextCoord2 = moveD coord2 dir2 1
          gameStateMaybe' = applyMove (pieceId pawn2) nextCoord2 gameState'

      gameStateMaybe' `shouldNotBe` Nothing
      -}
