module Chess.TestUtils where

import Import
import           Chess.Core.Domain.Base
import           Chess.Core.Domain.Board
import           Chess.Core.Domain.Piece
import           Chess.Core.Domain.Player
import           Chess.Core.Domain.GameState
import           Chess.Core.Domain.Space
import qualified Data.Map as Map
import qualified Data.Maybe as DM

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

buildBoardProfileMap :: GameState -> Map.Map Player (Map.Map PieceType [Piece])
buildBoardProfileMap GameState{board = Board{spacesMap = spsMap}} =
  foldl' collect Map.empty spsMap
  where
    collect :: Map.Map Player (Map.Map PieceType [Piece]) -> Space -> Map.Map Player (Map.Map PieceType [Piece])
    collect acc (Void _) = acc
    collect acc (Space{spacePiece = Nothing}) = acc
    collect acc (Space{spacePiece = Just p@Piece{piecePlayer = ppl, pieceType = pt}}) =
      Map.insert ppl updatedPlayerMap acc
      where
        currentPlayerMap :: Map.Map PieceType [Piece]
        currentPlayerMap = DM.fromMaybe Map.empty $ Map.lookup ppl acc

        --pieces :: [Piece]
        --pieces = DM.fromMaybe [] $ Map.lookup pt currentPlayerMap
        
        updatedPlayerMap :: Map.Map PieceType [Piece]
        updatedPlayerMap = Map.insertWith (++) pt [p] currentPlayerMap 
