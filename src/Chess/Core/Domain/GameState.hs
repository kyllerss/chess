{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Chess.Core.Domain.GameState where

import Import
import Chess.Core.Domain.Board
import Chess.Core.Domain.Coord
import Chess.Core.Domain.Move
import Chess.Core.Domain.Piece
import Chess.Core.Domain.Player
import Chess.Core.Domain.Space
import Chess.Core.Logic.BoardBuilder
import Chess.Core.Logic.Moves
import qualified Data.Maybe as DM
import qualified Data.Map as Map
import Data.List ((!!), elemIndex)
import qualified Data.List as DL
--import Chess.Core.DebugUtils

{-# ANN module ("HLint: ignore Use String" :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}
{-# ANN module ("HLint: ignore Use head" :: String) #-}

data GameState = GameState { board      :: Board
                           , moves      :: [Move]
                           , players    :: [Player]
                           , playerTurn :: Player
                           , token      :: Text
                           , gameId     :: GameId
                           }
    deriving (Show, Read, Eq, Generic, NFData)

instance ToJSON GameState where
    toJSON GameState{board = b, playerTurn = pl, gameId = gId, moves = ms} =
        object [ "board" .= b
               , "pieces" .= renderPieces b
               , "moves" .= renderMoves ms
               , "gameId" .= renderGameId gId
               , "playerTurn" .= pl]
      where

        renderKeyValueMap :: Board -> (Space -> Maybe (Text, Value)) -> Map Text Value
        renderKeyValueMap Board{spacesMap = spsMap} valueExtractor =
            foldl' (\acc spc -> insertElement spc valueExtractor acc) Map.empty spsMap

        insertElement :: Space -> (Space -> Maybe (Text, Value)) -> Map Text Value -> Map Text Value
        insertElement spc valueExtractor acc =
          let pair = valueExtractor spc
          in if DM.isNothing pair
             then acc
             else uncurry Map.insert (DM.fromJust pair) acc

        renderPieces :: Board -> Map Text Value
        renderPieces b' = renderKeyValueMap b' appendPiece

        appendPiece :: Space -> Maybe (Text, Value)
        appendPiece (Void _) = Nothing
        appendPiece Space{spacePiece = Nothing} = Nothing
        appendPiece Space{spaceCoord = c, spacePiece = Just p} =
            Just (pack $ show $ pieceIdValue $ pieceId p, renderPiece p c)

        renderPiece :: Piece -> Coord -> Value
        renderPiece p c = object [ "t" .= pieceType p
                                 , "c" .= pieceColor (p :: Piece)
                                 , "p" .= playerId (piecePlayer p)
                                 , "xy" .= c
                                 ]

        renderMoves :: [Move] -> Map Text [Value]
        renderMoves mvs = DL.foldl' (\acc (k,v) -> Map.insertWith (++) k v acc) Map.empty assocList
          where
            assocList :: [(Text, [Value])]
            assocList = DL.map (\m -> (pack $ show $ pieceIdValue $ movePieceId m, [toJSON m])) mvs

        renderGameId :: GameId -> Value
        renderGameId (GameId gid) = toJSON gid

{- Builds standard board matrix for player 2. -}
standardBoardPlayer2 :: [[Char]]
standardBoardPlayer2 =  [ ['r', 'n', 'b', 'q', 'k', 'b', 'n', 'r']
                        , ['p', 'p', 'p', 'p', 'p', 'p', 'p', 'p']
                        , ['.', '.', '.', '.', '.', '.', '.', '.']
                        , ['.', '.', '.', '.', '.', '.', '.', '.']
                        , ['.', '.', '.', '.', '.', '.', '.', '.']
                        , ['.', '.', '.', '.', '.', '.', '.', '.']
                        , ['.', '.', '.', '.', '.', '.', '.', '.']
                        , ['.', '.', '.', '.', '.', '.', '.', '.']
                        ]

{- Builds a new game state.  -}
initGame :: GameId -> GameType -> [Player] -> GameState
initGame gId Standard pls =
    GameState{ board = DM.fromJust board
             , moves = allMoves
             , players = [player1, player2]
             , playerTurn = player1
             , token = ""
             , gameId = gId}
    where
      spaceMatrix = [ ['w', 'b', 'w', 'b', 'w', 'b', 'w', 'b']
                    , ['b', 'w', 'b', 'w', 'b', 'w', 'b', 'w']
                    , ['w', 'b', 'w', 'b', 'w', 'b', 'w', 'b']
                    , ['b', 'w', 'b', 'w', 'b', 'w', 'b', 'w']
                    , ['w', 'b', 'w', 'b', 'w', 'b', 'w', 'b']
                    , ['b', 'w', 'b', 'w', 'b', 'w', 'b', 'w']
                    , ['w', 'b', 'w', 'b', 'w', 'b', 'w', 'b']
                    , ['b', 'w', 'b', 'w', 'b', 'w', 'b', 'w']
                    ]

      propsMatrix = [ ['p', 'p', 'p', 'p', 'p', 'p', 'p', 'p']
                    , ['.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.']
                    , ['p', 'p', 'p', 'p', 'p', 'p', 'p', 'p']
                    ]

      player1Matrix = [ ['.', '.', '.', '.', '.', '.', '.', '.']
                      , ['.', '.', '.', '.', '.', '.', '.', '.']
                      , ['.', '.', '.', '.', '.', '.', '.', '.']
                      , ['.', '.', '.', '.', '.', '.', '.', '.']
                      , ['.', '.', '.', '.', '.', '.', '.', '.']
                      , ['.', '.', '.', '.', '.', '.', '.', '.']
                      , ['p', 'p', 'p', 'p', 'p', 'p', 'p', 'p']
                      , ['r', 'n', 'b', 'q', 'k', 'b', 'n', 'r']
                      ]

      player2Matrix = standardBoardPlayer2

      playerPieceMatrix = [(player1, player1Matrix), (player2, player2Matrix)]
      board = constituteBoard 8 8 spaceMatrix propsMatrix playerPieceMatrix
      allMoves = allValidMoves board player1
      player1 = pls !! 0
      player2 = pls !! 1

initGame gId Dunsany pls =
    GameState{ board = DM.fromJust board
             , moves = allMoves
             , players = [player1, player2]
             , playerTurn = player2
             , token = ""
             , gameId = gId}
    where
      spaceMatrix = [ ['w', 'b', 'w', 'b', 'w', 'b', 'w', 'b']
                    , ['b', 'w', 'b', 'w', 'b', 'w', 'b', 'w']
                    , ['w', 'b', 'w', 'b', 'w', 'b', 'w', 'b']
                    , ['b', 'w', 'b', 'w', 'b', 'w', 'b', 'w']
                    , ['w', 'b', 'w', 'b', 'w', 'b', 'w', 'b']
                    , ['b', 'w', 'b', 'w', 'b', 'w', 'b', 'w']
                    , ['w', 'b', 'w', 'b', 'w', 'b', 'w', 'b']
                    , ['b', 'w', 'b', 'w', 'b', 'w', 'b', 'w']
                    ]
      propsMatrix = [ ['p', 'p', 'p', 'p', 'p', 'p', 'p', 'p']
                    , ['.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.']
                    , ['p', 'p', 'p', 'p', 'p', 'p', 'p', 'p']
                    ]

      player1Matrix = [ ['.', '.', '.', '.', '.', '.', '.', '.']
                      , ['.', '.', '.', '.', '.', '.', '.', '.']
                      , ['.', '.', '.', '.', '.', '.', '.', '.']
                      , ['.', '.', '.', '.', '.', '.', '.', '.']
                      , ['p', 'p', 'p', 'p', 'p', 'p', 'p', 'p']
                      , ['p', 'p', 'p', 'p', 'p', 'p', 'p', 'p']
                      , ['p', 'p', 'p', 'p', 'p', 'p', 'p', 'p']
                      , ['p', 'p', 'p', 'p', 'p', 'p', 'p', 'p']
                      ]

      player2Matrix = standardBoardPlayer2

      playerPieceMatrix = [(player1, player1Matrix), (player2, player2Matrix)]
      board = constituteBoard 8 8 spaceMatrix propsMatrix playerPieceMatrix
      allMoves = allValidMoves board player2
      player1 = pls !! 0
      player2 = pls !! 1

initGame gId Chad pls =
    GameState{ board = DM.fromJust board
             , moves = allMoves
             , players = [player1, player2]
             , playerTurn = player1
             , token = ""
             , gameId = gId}
    where
      spaceMatrix = [ ['w', 'w', 'w', 'w', 'w', 'w', 'w', 'w', 'w', 'w', 'w', 'w']
                    , ['w', 'w', 'w', 'w', 'w', 'w', 'w', 'b', 'b', 'b', 'w', 'w']
                    , ['w', 'w', 'w', 'w', 'w', 'w', 'b', 'w', 'w', 'w', 'b', 'w']
                    , ['w', 'w', 'w', 'w', 'w', 'w', 'b', 'w', 'w', 'w', 'b', 'w']
                    , ['w', 'w', 'w', 'w', 'w', 'w', 'b', 'w', 'w', 'w', 'b', 'w']
                    , ['w', 'w', 'w', 'w', 'w', 'w', 'w', 'b', 'b', 'b', 'w', 'w']
                    , ['w', 'w', 'b', 'b', 'b', 'w', 'w', 'w', 'w', 'w', 'w', 'w']
                    , ['w', 'b', 'w', 'w', 'w', 'b', 'w', 'w', 'w', 'w', 'w', 'w']
                    , ['w', 'b', 'w', 'w', 'w', 'b', 'w', 'w', 'w', 'w', 'w', 'w']
                    , ['w', 'b', 'w', 'w', 'w', 'b', 'w', 'w', 'w', 'w', 'w', 'w']
                    , ['w', 'w', 'b', 'b', 'b', 'w', 'w', 'w', 'w', 'w', 'w', 'w']
                    , ['w', 'w', 'w', 'w', 'w', 'w', 'w', 'w', 'w', 'w', 'w', 'w']
                    ]

      propsMatrix = [ ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    ]

      plyr1Matrix = [ ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', 'r', 'r', 'r', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', 'r', 'k', 'r', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', 'r', 'r', 'r', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    ]

      plyr2Matrix = [ ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', 'r', 'r', 'r', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', 'r', 'k', 'r', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', 'r', 'r', 'r', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    ]

      playerPieceMatrix = [(player1, plyr1Matrix), (player2, plyr2Matrix)]
      board = constituteBoard 12 12 spaceMatrix propsMatrix playerPieceMatrix
      allMoves = allValidMoves board player1
      player1 = pls !! 0
      player2 = pls !! 1

initGame gId FourPerson pls =
    GameState{ board = DM.fromJust board
             , moves = allMoves
             , players = [player1, player2, player3, player4]
             , playerTurn = player1
             , token = ""
             , gameId = gId}
    where
      spaceMatrix = [ ['v', 'v', 'v', 'w', 'b', 'w', 'b', 'w', 'b', 'w', 'b', 'v', 'v', 'v']
                    , ['v', 'v', 'v', 'b', 'w', 'b', 'w', 'b', 'w', 'b', 'w', 'v', 'v', 'v']
                    , ['v', 'v', 'v', 'w', 'b', 'w', 'b', 'w', 'b', 'w', 'b', 'v', 'v', 'v']
                    , ['w', 'b', 'w', 'b', 'w', 'b', 'w', 'b', 'w', 'b', 'w', 'b', 'w', 'b']
                    , ['b', 'w', 'b', 'w', 'b', 'w', 'b', 'w', 'b', 'w', 'b', 'w', 'b', 'w']
                    , ['w', 'b', 'w', 'b', 'w', 'b', 'w', 'b', 'w', 'b', 'w', 'b', 'w', 'b']
                    , ['b', 'w', 'b', 'w', 'b', 'w', 'b', 'w', 'b', 'w', 'b', 'w', 'b', 'w']
                    , ['w', 'b', 'w', 'b', 'w', 'b', 'w', 'b', 'w', 'b', 'w', 'b', 'w', 'b']
                    , ['b', 'w', 'b', 'w', 'b', 'w', 'b', 'w', 'b', 'w', 'b', 'w', 'b', 'w']
                    , ['w', 'b', 'w', 'b', 'w', 'b', 'w', 'b', 'w', 'b', 'w', 'b', 'w', 'b']
                    , ['b', 'w', 'b', 'w', 'b', 'w', 'b', 'w', 'b', 'w', 'b', 'w', 'b', 'w']
                    , ['v', 'v', 'v', 'b', 'w', 'b', 'w', 'b', 'w', 'b', 'w', 'v', 'v', 'v']
                    , ['v', 'v', 'v', 'w', 'b', 'w', 'b', 'w', 'b', 'w', 'b', 'v', 'v', 'v']
                    , ['v', 'v', 'v', 'b', 'w', 'b', 'w', 'b', 'w', 'b', 'w', 'v', 'v', 'v']
                    ]


      propsMatrix = [ ['.', '.', '.', 'p', 'p', 'p', 'p', 'p', 'p', 'p', 'p', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['p', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', 'p']
                    , ['p', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', 'p']
                    , ['p', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', 'p']
                    , ['p', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', 'p']
                    , ['p', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', 'p']
                    , ['p', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', 'p']
                    , ['p', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', 'p']
                    , ['p', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', 'p']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', 'p', 'p', 'p', 'p', 'p', 'p', 'p', 'p', '.', '.', '.']
                    ]

      plyr1Matrix = [ ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', 'p', 'p', 'p', 'p', 'p', 'p', 'p', 'p', '.', '.', '.']
                    , ['.', '.', '.', 'r', 'n', 'b', 'q', 'k', 'b', 'n', 'r', '.', '.', '.']
                    ]

      plyr2Matrix = [ ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['r', 'p', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['n', 'p', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['b', 'p', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['k', 'p', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['q', 'p', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['b', 'p', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['n', 'p', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['r', 'p', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    ]

      plyr3Matrix = [ ['.', '.', '.', 'r', 'n', 'b', 'q', 'k', 'b', 'n', 'r', '.', '.', '.']
                    , ['.', '.', '.', 'p', 'p', 'p', 'p', 'p', 'p', 'p', 'p', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    ]

      plyr4Matrix = [ ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', 'p', 'r']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', 'p', 'n']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', 'p', 'b']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', 'p', 'k']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', 'p', 'q']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', 'p', 'b']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', 'p', 'n']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', 'p', 'r']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    , ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
                    ]

      playerPieceMatrix = [ (player1, plyr1Matrix)
                          , (player2, plyr2Matrix)
                          , (player3, plyr3Matrix)
                          , (player4, plyr4Matrix)
                          ]
      board = constituteBoard 14 14 spaceMatrix propsMatrix playerPieceMatrix
      allMoves = allValidMoves board player1
      player1 = pls !! 0
      player2 = pls !! 2
      player3 = pls !! 1
      player4 = pls !! 3

initGameEmpty :: Int -> Int -> [Player] -> Player -> GameState
initGameEmpty width height pls plTurn = DM.fromJust $ initGameBare (GameId "") width height [] pls plTurn

{- Create a new board.  -}
initGameBare :: GameId -> Int -> Int -> [(Piece, Coord)] -> [Player] -> Player -> Maybe GameState
initGameBare gId width height pcs pls plTurn = generateGameState maybeBoard
  where
    board = initBoard width height defaultSpaceBuilder
    maybeBoard = foldl' (\mb (p, c) -> appendPiece p c mb) (Just board) pcs

    moves = allValidMoves maybeBoard plTurn

    appendPiece :: Piece -> Coord -> Maybe Board -> Maybe Board
    appendPiece _ _ Nothing = Nothing
    appendPiece p c (Just b) = addPieceToBoard p c b

    generateGameState :: Maybe Board -> Maybe GameState
    generateGameState Nothing = Nothing
    generateGameState (Just b) =  Just GameState { board = b
                                                 , moves = moves
                                                 , players = pls
                                                 , playerTurn = plTurn
                                                 , token = pack "abc"
                                                 , gameId = gId
                                                 }



addPiece :: Piece -> Coord -> GameState -> Maybe GameState
addPiece p c g@GameState{board = currentBoard}
  | isNothing newBoard = Nothing
  | otherwise = Just g{board = DM.fromJust newBoard}

  where
    newBoard :: Maybe Board
    newBoard = addPieceToBoard p c currentBoard

{- Apply move.
TODO: Add player turn validation
-}
applyMove :: PieceId -> Coord -> GameState -> Maybe GameState
applyMove pId coord gs@GameState {playerTurn = cpl, players = pls, board = b}
  | isNothing newBoard = Nothing
  | otherwise = Just gs{board = DM.fromJust newBoard, playerTurn = nextPlayer, moves = newBoardMoves}
  where
    newBoard :: Maybe Board
    newBoard = move pId coord b

    newBoardMoves :: [Move]
    newBoardMoves = allValidMoves newBoard nextPlayer

    currentPlayerIndex :: Int
    currentPlayerIndex = DM.fromJust $ elemIndex cpl pls

    nextPlayer :: Player
    nextPlayer
      | currentPlayerIndex == (length pls - 1) = DL.head pls
      | otherwise = pls !! (currentPlayerIndex + 1)

updateGameSpaceSideEffect :: Coord -> SpaceSideEffectType -> GameState -> Maybe GameState
--updateGameSpaceSideEffect _ _ Nothing = Nothing
updateGameSpaceSideEffect c se gs@GameState{board = b}
  | isNothing newBoard = Nothing
  | otherwise = Just gs{board = DM.fromJust newBoard}
  where
    newBoard :: Maybe Board
    newBoard = updateBoardSpaceSideEffect c se b

