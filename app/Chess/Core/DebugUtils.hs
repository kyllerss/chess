module Chess.Core.DebugUtils where

import Import
import Chess.Core.Domain.Piece
import Chess.Core.Domain.Player

generateDebugInfo :: Maybe Piece -> String
generateDebugInfo Nothing = "Nothing"
generateDebugInfo (Just Piece{piecePlayer = Player{playerId = plId}
                             , pieceType = pType
                             , pieceId = PieceId{pieceIdValue = pIdVal}}) = "Player: " ++ show plId ++ ", type: " ++ show pType ++ ", pieceId: " ++ show pIdVal
