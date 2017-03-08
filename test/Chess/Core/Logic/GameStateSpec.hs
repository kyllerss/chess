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
import           Chess.Core.Domain.Space
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

      let coord1 = fromJust $ spaceCoord <$> space1
          dir1 = playerDirection player1
          nextCoord1 = moveD coord1 dir1 1 
          gameState' = applyMove (pieceId pawn1) nextCoord1 gameState

      gameState' `shouldNotBe` Nothing

    it "should be able to move two pieces" $ do
      let pawn1 = DL.head $ boardProfile Map.! player1 Map.! Pawn
          space1 = fetchPieceSpace pawn1 (board gameState)

      space1 `shouldNotBe` Nothing

      let coord1 = fromJust $ spaceCoord <$> space1
          dir1 = playerDirection player1
          nextCoord1 = moveD coord1 dir1 1 
          gameStateMaybe = applyMove (pieceId pawn1) nextCoord1 gameState

      gameStateMaybe `shouldNotBe` Nothing

      let gameState' = fromJust gameStateMaybe
          pawn2 = DL.head $ boardProfile Map.! player2 Map.! Pawn
          space2 = fetchPieceSpace pawn2 (board gameState')

      space2 `shouldNotBe` Nothing

      let coord2 = fromJust $ spaceCoord <$> space2
          dir2 = playerDirection player2
          nextCoord2 = moveD coord2 dir2 1 
          gameStateMaybe' = applyMove (pieceId pawn2) nextCoord2 gameState'

      gameStateMaybe' `shouldNotBe` Nothing

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

      
        
