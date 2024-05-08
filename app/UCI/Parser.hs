module UCI.Parser where

import Text.Megaparsec
import Text.Megaparsec.Char

import MoveParser (moveParser)

import UCI.Spesification (InCommand(..))
import Components(Piece(Queen, Knight, Rook, Bishop), Move(Move),  BoardPosition(BoardPosition))

type Parser = Parsec () String

uciParser :: Parser InCommand
uciParser = string "uci" >> return Uci

debugParser :: Parser InCommand
debugParser = do
    _ <- string "debug"
    _ <- space1
    (string "on" >> return (Debug True)) <|> (string "off" >> return (Debug False))

isReadyParser :: Parser InCommand
isReadyParser = string "isready" >> return IsReady

setOptionParser :: Parser InCommand
setOptionParser = string "setoption" >> return SetOption

registerParser :: Parser InCommand
registerParser = string "register" >> return Register

uciNewGameParser :: Parser InCommand
uciNewGameParser = string "ucinewgame" >> return UciNewGame

goParser :: Parser InCommand
goParser = string "go" >> return Go

stopParser :: Parser InCommand
stopParser = string "stop" >> return Stop

quitParser :: Parser InCommand
quitParser = string "quit" >> return Quit

positionParser :: Parser InCommand
positionParser = do
    _ <- string "position"
    _ <- space1
    _ <- optional (string "startpos" *> optional space1)
    _ <- string "moves"
    _ <- space1
    move1 <- moveParser
    moves <- many (space1 *> moveParser)
    return (Position (move1:moves))

unknownParser :: Parser InCommand
unknownParser = do
    _ <- some alphaNumChar
    _ <- space1
    inputParser

inputParser :: Parser InCommand
inputParser = choice
    [
        try debugParser,
        try uciNewGameParser,
        try isReadyParser,
        try setOptionParser,
        try registerParser,
        try positionParser,
        try goParser,
        try stopParser,
        try uciParser,
        unknownParser
    ]

parseInput :: String -> Maybe InCommand
parseInput s = case parse inputParser "" s of
    Left _ -> Nothing
    Right c -> Just c