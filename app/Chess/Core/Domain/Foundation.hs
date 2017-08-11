module Chess.Core.Domain.Foundation where

import Import.NoFoundation

{-# ANN module ("HLint: ignore Eta reduce" :: String) #-}
data GameType = Standard | Dunsany | Chad | FourPerson
  deriving (Show, Read, Eq, Generic, Ord, Enum, Bounded, NFData)

allGameTypes :: [GameType]
allGameTypes = [minBound ..]

{-# ANN module ("HLint: Use newtype instead of data" :: String) #-}
data GameId = GameId String
  deriving (Show, Read, Eq, Generic, NFData)

