module Chess.Core.Domain.Foundation where

import Import.NoFoundation

{-# ANN module ("HLint: ignore Eta reduce" :: String) #-}
{-# ANN module ("HLint: Use newtype instead of data" :: String) #-}

data GameType = Standard | Dunsany | Chad | FourPerson
  deriving (Show, Read, Eq, Generic, Ord, Enum, Bounded, NFData)

allGameTypes :: [GameType]
allGameTypes = [minBound ..]

data GameId = GameId String
  deriving (Show, Read, Eq, Generic, NFData)

