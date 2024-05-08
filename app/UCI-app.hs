module Main where

import Board
import UCI.Controller (controllerStart, initialControllerState)
import UCI.Communication (runCommunication)
import Control.Monad.Freer.State (evalState)
import Control.Monad.Freer ( runM )

main :: IO ()
main = do
    let initialState = initialControllerState
    runM . evalState initialState . runCommunication $ controllerStart
