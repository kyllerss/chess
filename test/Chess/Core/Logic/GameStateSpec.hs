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
import           Chess.Core.Domain.Space
import           Chess.Core.Logic.Moves
import Data.Maybe (fromJust)
import Data.List ((!!))
import qualified Data.Map as Map



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

        boardProfile :: Map.Map Player (Map.Map PieceType [Piece])
        boardProfile = buildBoardProfileMap gameState
    
    it "has two players" $ do
      players gameState `shouldBe` [player1, player2]

    it "doesn't have any recorded moves" $ do
      moves gameState `shouldBe` []

    it "player turn should be first player" $ do
      playerTurn gameState `shouldBe` player1

    it "should have a standard pieces" $ do
      (Map.size . spacesMap . board) gameState `shouldNotBe` 0

    it "should be able to move a piece once" $ do
      traceShow boardProfile boardProfile `shouldNotBe` Map.empty

      
        
