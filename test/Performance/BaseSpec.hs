module Performance.BaseSpec ( spec ) where

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
import           Chess.Core.Logic.BoardBuilder
import           Chess.Core.Logic.Moves
import Data.Maybe (fromJust)
import qualified Data.List as DL
import qualified Data.Map as Map

spec :: Spec
spec =
  describe "four-player board" $ do

    let player1, player2, player3, player4 :: Player
    
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
        player3 = Player { playerName = pack "Player 3"
                         , playerId = 3
                         , playerType = Human
                         , playerDirection = East
                         }
        player4 = Player { playerName = pack "Player 4"
                         , playerId = 4
                         , playerType = Human
                         , playerDirection = West
                         }

        initialGame :: GameState
        initialGame = initGame FourPerson [player1, player2, player3, player4]
        

    it "should not have terrible performance" $ do

      let pawn1A = fromJust $ fetchPiece (Coord 12 7) (board initialGame)
          pawn2A = fromJust $ fetchPiece (Coord 6 1) (board initialGame)
          pawn3A = fromJust $ fetchPiece (Coord 1 7) (board initialGame)
          pawn4A = fromJust $ fetchPiece (Coord 6 12) (board initialGame)
          
          newGame = Just initialGame >>=
                    applyMove (pieceId pawn1A) (Coord 10 7) >>=
                    applyMove (pieceId pawn2A) (Coord 6 3) >>=
                    applyMove (pieceId pawn3A) (Coord 3 7) >>=
                    applyMove (pieceId pawn4A) (Coord 6 10) 

      newGame `shouldNotBe` Nothing

