module Chess.Core.Domain.Foundation where

import Import.NoFoundation

data GameType = Standard | Dunsany | Chad
  deriving (Show, Read, Eq, Generic, Ord, Enum, Bounded, NFData)

allGameTypes :: [GameType]
allGameTypes = [minBound ..]

data GameId = GameId String
  deriving (Show, Read, Eq, Generic, NFData)

