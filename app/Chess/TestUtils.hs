module Chess.TestUtils where

import Import
import           Chess.Core.Domain.Base
import           Chess.Core.Domain.Piece
import           Chess.Core.Domain.Player

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

