module Components where

import Data.Char (chr)

data Player
    = Black
    | White
    deriving (Show, Eq)

data Piece
    = Pawn
    | King
    | Queen
    | Rook
    | Bishop
    | Knight

instance Show Piece where
    show Pawn   = "p"
    show King   = "k"
    show Queen  = "q"
    show Rook   = "r"
    show Bishop = "b"
    show Knight = "n"

data Move = Move {
    start :: BoardPosition, 
    end :: BoardPosition,
    promotion :: Maybe Piece 
}

instance Show Move where
    show (Move f t p) = show f ++ show t ++ maybe "" show p

data BoardPosition = BoardPosition Int Int

instance Show BoardPosition where
    show (BoardPosition file rank) = chr (fromEnum 'a' + file - 1) : show rank