module Chess.Core.Domain.Player where

import Import
import Chess.Core.Domain.Base

data PlayerType = Human | Computer
    deriving (Show, Read, Eq, Generic, NFData)

data Player = Player { playerName      :: Text
                     , playerId        :: Int
                     , playerType      :: PlayerType
                     , playerDirection :: Direction
                     }
    deriving (Show, Read, Generic, NFData)

instance Ord Player where
  Player{playerId = pId1} `compare` Player{playerId = pId2} = pId1 `compare` pId2

instance Eq Player where
  (==) p1 p2 = playerId p1 == playerId p2

instance ToJSON Player where
    toJSON (p@Player{playerName = pName, playerId = pId}) =
        object [ "name" .= pName, "id" .= pId, "color" .= assignPlayerColor p ]

assignPlayerColor :: Player -> Color
assignPlayerColor Player{playerId = pId} = toEnum ((pId - 1) `mod` (fromEnum (maxBound :: Color)))
