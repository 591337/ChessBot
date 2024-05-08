{-# LANGUAGE GADTs, FlexibleContexts, TemplateHaskell, TypeOperators, DataKinds #-}

module UCI.Communication where

import Control.Monad.Freer
import Control.Monad.Freer.TH
import UCI.Spesification (InCommand, OutCommand)

import UCI.Parser (parseInput)
import UCI.Output (outString)
import Board (Board)

data Communication r where
  GetInstruction  :: Communication InCommand
  OutInstruction  :: OutCommand -> Communication ()
  LoggString :: String -> Communication ()
makeEffect ''Communication

runCommunication :: LastMember IO es => Eff (Communication : es) a -> Eff es a
runCommunication = interpretM go
  where go :: Communication r -> IO r
        go GetInstruction = getInstructionLoop
        go (OutInstruction c) = putStrLn (outString c)
        go (LoggString b) = putStrLn b

getInstructionLoop :: IO InCommand
getInstructionLoop = do
    input <- getLine
    maybe getInstructionLoop return (parseInput input)
