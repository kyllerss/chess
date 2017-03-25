module Chess.Core.Logic.GameStateSpec ( spec ) where

import Import
import           Test.Hspec
import           Chess.TestUtils
import           Chess.Core.Domain.Base
import           Chess.Core.Domain.Board
import           Chess.Core.Domain.Coord
import           Chess.Core.Domain.GameState
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
      moves gameState `shouldBe` []

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

    it "should not crash when two players move" $ do
      
      let emptyGame = initGameEmpty 3 6 [player1, player2] player2
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
                      addPiece kingB (Coord 1 0) >>=
                      addPiece pawnB1 (Coord 0 1) >>=
                      addPiece pawnB2 (Coord 1 1) >>=
                      addPiece pawnB3 (Coord 2 1) >>=
                      addPiece kingA (Coord 1 5) >>=
                      addPiece pawnA1 (Coord 0 4) >>=
                      addPiece pawnA2 (Coord 1 4) >>=
                      addPiece pawnA3 (Coord 2 4)

      initialGame `shouldNotBe` Nothing

      let newGame = initialGame >>=
                    applyMove (pieceId pawnA2) (Coord 1 3) >>=
                    applyMove (pieceId pawnB2) (Coord 1 2)

      newGame `shouldNotBe` Nothing
      
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
