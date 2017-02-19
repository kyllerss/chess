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

data GameState = GameState { board      :: Board
                           , moves      :: [Move]
                           , players    :: [Player]
                           , playerTurn :: Player
                           , token      :: Text
                           }
    deriving Show

instance ToJSON GameState where
    toJSON GameState{board = b, playerTurn = pl} =
        object [ "board" .= b, "pieces" .= (renderPieces b), "moves" .= (renderMoves b) ]
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

data GameType = Standard

{- Builds a new game state.  -}
initGame :: GameType -> [Player] -> Maybe GameState
initGame Standard (p1:p2:[]) =
    Just GameState{ board = board
                         , moves = allMoves
                         , players = [p1, p2]
                         , playerTurn = p1
                         , token = "" }
    where
      board = initStandardBoard p1 p2
      allMoves = []
initGame Standard _ = Nothing

{- Create a new board.  -}
initGameEmpty :: Int -> Int -> GameState
initGameEmpty width height = GameState { board = initBoard width
                                                      height
                                                      defaultSpaceBuilder
                                  , moves = []
                                  , players = [ human1, ai1 ]
                                  , playerTurn = human1
                                  , token = pack "abc"
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
