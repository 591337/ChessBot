module MoveParser where

import Text.Megaparsec
import Text.Megaparsec.Char

import Components(Piece(Queen, Knight, Rook, Bishop), Move(Move),  BoardPosition(BoardPosition))

type Parser = Parsec () String

parseInput :: String -> Maybe Move
parseInput s = case parse moveParser "" s of
    Left _ -> Nothing
    Right c -> Just c

moveParser :: Parser Move
moveParser = do
    pos1 <- boardPositionParser
    pos2 <- boardPositionParser
    Move pos1 pos2 <$> promotionParser

promotionParser :: Parser (Maybe Piece)
promotionParser = optional $ choice
    [ char 'q' >> return Queen
    , char 'n' >> return Knight
    , char 'b' >> return Bishop
    , char 'r' >> return Rook
    ]

boardPositionParser :: Parser BoardPosition
boardPositionParser = do
    x <- fileParser
    y <- digitChar
    let col = fromEnum x - fromEnum 'a' + 1
    let row = read [y]
    return (BoardPosition col row)

fileParser :: Parser Char
fileParser = oneOf ['a' .. 'h']