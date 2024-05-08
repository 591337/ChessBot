module UCI.Spesification where

import Components(Move(..))

data InCommand
    = Uci
    | Debug Bool
    | IsReady
    | SetOption
    | Register
    | UciNewGame
    | Position [Move]
    | Go
    | Stop
    | Quit
    deriving (Show)

data OutCommand
    = Id IdInfo
    | UciOk
    | ReadyOk
    | BestMove Move
    | Info ProcessInfo
    deriving (Show)

data IdInfo = Name String | Auther String
    deriving (Show)

data ProcessInfo
    = Depth Int
    | SelDepth Int
    | Time Int
    | Nodes Int
    | Pv [Move]
    | Score ScoreInfo
    | CurrentMove Move
    | CurrentMoveNumber Int
    deriving (Show)

data ScoreInfo = Cp Float | Mate Int
    deriving (Show)
