module Chess.Core.Logic.BoardBuilderSpec (spec) where

import Import
import Test.Hspec
import Chess.Core.Domain.Base
import Chess.Core.Domain.Board
import Chess.Core.Domain.Coord
import Chess.Core.Domain.Player
import Chess.Core.Domain.Space
import Chess.Core.Logic.BoardBuilder
import Data.Maybe (fromJust)

spec :: Spec
spec = describe "Board builder" $ do

  let player1 :: Player
      player1 = Player { playerName = pack "Player 1"
                       , playerId = 1
                       , playerType = Human
                       , playerDirection = North
                       }
      
  it "can construct simple 1x1" $ do

    let colorMatrix :: BoardMatrix
        colorMatrix = [['v']]

        nullMatrix :: BoardMatrix
        nullMatrix = [['.']]

        playerPieceMatrix :: [PlayerMatrix]
        playerPieceMatrix = [(player1, nullMatrix)]
    
        board :: Maybe Board
        board = constituteBoard 1 1 colorMatrix nullMatrix playerPieceMatrix

    board `shouldNotBe` Nothing

    let space :: Maybe Space
        space = fetchSpace (Coord 0 0) (fromJust board)

    space `shouldBe` Just (Void (Coord 0 0))

    
