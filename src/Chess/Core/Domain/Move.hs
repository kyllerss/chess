{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Chess.Core.Domain.Move where

import Import
import Chess.Core.Domain.Board
import Chess.Core.Domain.Coord
import Chess.Core.Domain.Piece
import Chess.Core.Domain.Space
import qualified Data.Maybe as DM

data BoardSideEffect = SideEffectMove { sideEffectMove :: Move }
                       | SideEffectPiece { sideEffectPiece :: Piece
                                         , sideEffectSpace :: Space }
                       | SideEffectCapture { sideEffectCoord :: Coord }
     deriving (Show, Read, Eq, Generic, NFData)

data Move = Move { movePieceId        :: PieceId
                 , moveSpace          :: Space
                 , moveIsConsumable   :: Bool
                 , moveSideEffects    :: [BoardSideEffect]
                 }
    deriving (Show, Read, Eq, Generic, NFData)

instance ToJSON Move where
    toJSON Move{moveSpace = Space{spaceCoord = Coord x y}} = toJSON [ x, y ]
    toJSON Move{moveSpace = Void (Coord x y)} = toJSON [ x, y ]

{- Convenience builder for Move -}
buildMove :: Piece -> Board -> Coord -> Bool -> [BoardSideEffect] -> Maybe Move
buildMove p b c offensive sideEffects
  | isNothing (fetchSpace c b) = Nothing
  | otherwise = Just Move { movePieceId = pieceId p
                          , moveSpace = DM.fromJust $ fetchSpace c b
                          , moveIsConsumable = offensive
                          , moveSideEffects = sideEffects
                          }

buildSideEffectMove :: Move -> BoardSideEffect
buildSideEffectMove = SideEffectMove

buildSideEffectPiece :: Piece -> Space -> BoardSideEffect
buildSideEffectPiece p s = SideEffectPiece{sideEffectPiece = p, sideEffectSpace = s}

buildSideEffectCapture :: Coord -> BoardSideEffect
buildSideEffectCapture c = SideEffectCapture{sideEffectCoord = c}
