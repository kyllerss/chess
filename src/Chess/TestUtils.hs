module Chess.TestUtils where

import           Chess.Core.Domain
import qualified Data.Maybe        as DM

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

