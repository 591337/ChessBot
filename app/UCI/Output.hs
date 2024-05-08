module UCI.Output where

import UCI.Spesification (OutCommand(..), IdInfo(..))
import Data.Char (chr)

outString :: OutCommand -> String
outString (Id (Name n))    = "id name " ++ n
outString (Id (Auther a))   = "id auther " ++ a
outString UciOk           = "uciok"
outString ReadyOk         = "readyok"
outString (BestMove m) = "bestmove " ++ show m
-- Info _          = "info "

