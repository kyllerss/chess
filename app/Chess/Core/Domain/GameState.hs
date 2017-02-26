module Chess.Core.Domain.GameState where

import Import
import Chess.Core.Domain.Base
import Chess.Core.Domain.Board
import Chess.Core.Domain.Coord
import Chess.Core.Domain.Move
import Chess.Core.Domain.Piece
import Chess.Core.Domain.Player
import Chess.Core.Domain.Space
import Chess.Core.Logic.Moves
import qualified Data.Maybe as DM
import qualified Data.Map as Map
import Data.List ((!!))

data GameState = GameState { board      :: Board
                           , moves      :: [Move]
                           , players    :: [Player]
                           , playerTurn :: Player
                           , token      :: Text
                           , gameId     :: GameId
                           }
    deriving (Show, Read)

instance ToJSON GameState where
    toJSON GameState{board = b, playerTurn = pl, gameId = gId} =
        object [ "board" .= b
               , "pieces" .= (renderPieces b)
               , "moves" .= (renderMoves b)
               , "gameId" .= (renderGameId gId) ]
      where
        
        renderKeyValueMap :: Board -> (Space -> Maybe (Text, Value)) -> Map Text Value
        renderKeyValueMap Board{spacesMap = spsMap} valueExtractor =
            foldl' (\acc spc -> insertElement spc valueExtractor acc) Map.empty spsMap

        insertElement :: Space -> (Space -> Maybe (Text, Value)) -> Map Text Value -> Map Text Value  
        insertElement spc valueExtractor acc =
          let pair = valueExtractor spc
          in if DM.isNothing pair
             then acc
             else Map.insert (fst $ DM.fromJust pair) (snd $ DM.fromJust pair) acc 
        
        renderPieces :: Board -> Map Text Value
        renderPieces b' = renderKeyValueMap b' appendPiece

        appendPiece :: Space -> Maybe (Text, Value)
        appendPiece (Void _) = Nothing
        appendPiece (Space{spacePiece = Nothing}) = Nothing
        appendPiece (Space{spaceCoord = c, spacePiece = Just p}) =
            Just (pack $ show $ pieceIdValue $ pieceId p, renderPiece p c)

        renderPiece :: Piece -> Coord -> Value
        renderPiece p c = object [ "t" .= (pieceType p)
                                 , "c" .= (pieceColor (p :: Piece))
                                 , "p" .= (playerId $ piecePlayer p)
                                 , "xy" .= c
                                 ]

        renderMoves :: Board -> Map Text Value
        renderMoves b' = renderKeyValueMap b' appendMove

        appendMove :: Space -> Maybe (Text, Value)
        appendMove (Void _) = Nothing
        appendMove Space{spacePiece = Nothing} = Nothing
        appendMove Space{spaceCoord = c, spacePiece = Just p@Piece{pieceId = pId, piecePlayer = ppl}}
          | ppl /= pl = Nothing
          | otherwise =
              let pMoves = validMoves b pId c
              in Just (pack $ show $ pieceIdValue $ pieceId p, toJSON pMoves)

        renderGameId :: GameId -> Value
        renderGameId (GameId gid) = toJSON gid

data GameType = Standard

{- Builds a new game state.  -}
initGame :: GameType -> [Player] -> GameState
initGame Standard pls =
    GameState{ board = board
             , moves = allMoves
             , players = [player1, player2]
             , playerTurn = player1
             , token = ""
             , gameId = GameId "12345ABCDE"}
    where
      board = initStandardBoard player1 player2
      allMoves = []
      player1 = pls !! 0
      player2 = pls !! 1

{- Create a new board.  -}
initGameEmpty :: Int -> Int -> GameState
initGameEmpty width height = GameState { board = initBoard width height defaultSpaceBuilder
                                       , moves = []
                                       , players = [ human1, ai1 ]
                                       , playerTurn = human1
                                       , token = pack "abc"
                                       , gameId = GameId "ABCDE12345"
                                       }
  where
    human1 = Player { playerName = pack "Kyle"
                    , playerId = 1
                    , playerType = Human
                    , playerDirection = North
                    }
    ai1 = Player { playerName = pack "Bot 1"
                 , playerId = 2
                 , playerType = Computer
                 , playerDirection = South
                 }

{- Apply move. -}
applyMove :: PieceId -> Coord -> GameState -> Maybe GameState
applyMove pId coord gs
  | newBoard == Nothing = Nothing
  | otherwise = Just gs{board = DM.fromJust newBoard}
  where
    newBoard :: Maybe Board
    newBoard = move pId coord (board gs)
