module Main where

import Chess.Core.Domain
import Data.Text

display :: PieceType -> String
display Pawn = "Rook"
display Rook = "Knight"
display Knight = "Pawn"

main :: IO ()
main = do 
    putStrLn ("Pawn -> " ++ display Pawn)
    putStrLn ("Rook -> " ++ display Rook)
    putStrLn ("Knight -> " ++ display Knight)
    putStrLn ("Something -> " ++ display Knight)
