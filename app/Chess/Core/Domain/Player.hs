module Chess.Core.Domain.Player where

import Import
import Chess.Core.Domain.Base

data PlayerType = Human | Computer
    deriving (Show, Read, Eq)

data Player = Player { playerName      :: Text
                     , playerId        :: Int
                     , playerType      :: PlayerType
                     , playerDirection :: Direction
                     }
    deriving (Show, Read, Eq)

instance Ord Player where
  Player{playerId = pId1} `compare` Player{playerId = pId2} = pId1 `compare` pId2

instance ToJSON Player where
    toJSON (Player{playerName = pName,playerId = pId}) =
        object [ "name" .= pName, "id" .= pId ]
