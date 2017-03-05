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

type PlayerBoardProfile = Map.Map PieceType [Piece]
type BoardProfile = Map.Map Player PlayerBoardProfile

buildBoardProfileMap :: GameState -> BoardProfile
buildBoardProfileMap GameState{board = Board{spacesMap = spsMap}} =
  foldl' collect Map.empty spsMap
  where
    collect :: BoardProfile -> Space -> BoardProfile
    collect acc (Void _) = acc
    collect acc (Space{spacePiece = Nothing}) = acc
    collect acc (Space{spacePiece = Just p@Piece{piecePlayer = ppl, pieceType = pt}}) =
      Map.insert ppl updatedPlayerMap acc
      where
        currentPlayerMap :: PlayerBoardProfile
        currentPlayerMap = DM.fromMaybe Map.empty $ Map.lookup ppl acc

        --pieces :: [Piece]
        --pieces = DM.fromMaybe [] $ Map.lookup pt currentPlayerMap
        
        updatedPlayerMap :: PlayerBoardProfile
        updatedPlayerMap = Map.insertWith (++) pt [p] currentPlayerMap 
