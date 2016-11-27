module Chess.Core.Serialization where

import           Chess.Core.Domain
import qualified Data.Text.Lazy    as T
import qualified Data.Text         as TT
import           Data.Aeson        ( FromJSON, ToJSON )
import           Data.Aeson.Types
import qualified Data.Aeson.Encode as J
import           GHC.Generics
import           Data.List         as DL ( groupBy, sortBy )
import qualified Data.Map          as M

instance ToJSON Color where
    toJSON Black = toJSON ("b" :: T.Text)
    toJSON White = toJSON ("w" :: T.Text)

instance ToJSON PieceType where
    toJSON Pawn = toJSON ("p" :: T.Text)
    toJSON Rook = toJSON ("r" :: T.Text)
    toJSON Knight = toJSON ("n" :: T.Text)
    toJSON Bishop = toJSON ("b" :: T.Text)
    toJSON King = toJSON ("k" :: T.Text)
    toJSON Queen = toJSON ("q" :: T.Text)

instance ToJSON Piece

instance ToJSON PieceId

instance ToJSON Player where
    toJSON (Player{playerName = pName,playerId = pId}) =
        object [ "name" .= pName, "id" .= pId ]

instance ToJSON Coord where
    toJSON (Coord x y) = toJSON $ [ x, y ]

instance ToJSON Space

instance ToJSON Board where
    toJSON (Board{spacesMap = spMap}) =
        toJSON $ encode . toColor . shape . sortSpaces $ M.foldr (:) [] spMap
      where
        multimap :: (a -> b) -> [[a]] -> [[b]]
        multimap f ss = map (\xs -> map f xs) ss

        -- group by row
        shape :: [Space] -> [[Space]]
        shape vs = DL.groupBy (\a -> (\b -> (extractRow a) == (extractRow b)))
                              vs
          where
            extractRow :: Space -> Int
            extractRow (Space{spaceCoord = Coord row _}) =
                row

        encode :: [[Color]] -> [[Value]]
        encode sp = multimap (\x -> toJSON x) sp

        toColor :: [[Space]] -> [[Color]]
        toColor sp = multimap (\s -> spaceColor (s :: Space)) sp

        sortSpaces :: [Space] -> [Space]
        sortSpaces sp = DL.sortBy orderByColumnAndRow sp

        orderByColumnAndRow :: Space -> Space -> Ordering
        orderByColumnAndRow a b =
            case (a, b) of
                (Void (Coord a1 b1), Void (Coord a2 b2))
                    | a1 > a2 -> GT
                    | a2 > a1 -> LT
                    | a1 == a2 -> if (b1 > b2) then GT else LT
                    | otherwise -> EQ
                (Void (Coord a1 b1), Space{spaceCoord = (Coord a2 b2)})
                    | a1 > a2 -> GT
                    | a2 > a1 -> LT
                    | a1 == a2 -> if (b1 > b2) then GT else LT
                    | otherwise -> EQ
                (Space{spaceCoord = (Coord a1 b1)}, Void (Coord a2 b2))
                    | a1 > a2 -> GT
                    | a2 > a1 -> LT
                    | a1 == a2 -> if (b1 > b2) then GT else LT
                    | otherwise -> EQ
                (Space{spaceCoord = (Coord a1 b1)}, Space{spaceCoord = (Coord a2 b2)})
                    | a1 > a2 -> GT
                    | a2 > a1 -> LT
                    | a1 == a2 -> if (b1 > b2) then GT else LT
                    | otherwise -> EQ

instance ToJSON GameState where
    toJSON GameState{board = b} =
        object [ "board" .= b, "pieces" .= (renderPieces b) ]
      where
        renderPieces :: Board -> M.Map T.Text Value
        renderPieces (Board sp) =
            foldl appendPiece M.empty sp

        appendPiece :: M.Map T.Text Value -> Space -> M.Map T.Text Value
        appendPiece m (Space{spaceCoord = c,spacePiece = Just p}) =
            M.insert (T.pack $ show $ pieceId p) (renderPiece p c) m

        {-
            map (\(Space{coord = c, piece = Just p}) ->
                     object [ (TT.pack (show (pieceId p))) .= (renderPiece p c) ])
                sp

-}
        renderPiece :: Piece -> Coord -> Value
        renderPiece p c = object [ "t" .= (pieceType p)
                                 , "c" .= (pieceColor (p :: Piece))
                                 , "p" .= (playerId $ piecePlayer p)
                                 , "xy" .= c
                                 ]
