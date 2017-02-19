module Chess.Core.Domain.Player where

import Import
import Chess.Core.Domain.Base

data PlayerType = Human | Computer
    deriving (Show, Eq)

data Player = Player { playerName      :: Text
                     , playerId        :: Int
                     , playerType      :: PlayerType
                     , playerDirection :: Direction
                     }
    deriving (Show, Eq)

instance ToJSON Player where
    toJSON (Player{playerName = pName,playerId = pId}) =
        object [ "name" .= pName, "id" .= pId ]
