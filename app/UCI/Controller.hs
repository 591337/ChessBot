{-# LANGUAGE DataKinds #-}
module UCI.Controller where

import Control.Monad.Freer.State

import UCI.Communication
import Control.Monad.Freer
import qualified UCI.Spesification as OutCommand
import qualified UCI.Spesification as InCommand
import qualified UCI.Spesification as IdInfo
import Board ( Board, newBoard, movePice, evaluation, bestMoveSearch )

data ControllerStatus = UnInitiated | Ready | Running
    deriving (Show)

data ControllerState = ControllerState {
    controllerStatus :: ControllerStatus,
    board :: Board,
    debug :: Bool
} deriving (Show)

initialControllerState :: ControllerState
initialControllerState = ControllerState {
    controllerStatus = UnInitiated,
    board = newBoard,
    debug = False
}

controllerStart :: Eff '[Communication, State ControllerState, IO] ()
controllerStart = do
    input <- getInstruction
    case input of
        InCommand.Quit -> do return ()
        InCommand.Uci -> do
            outInstruction $ OutCommand.Id $ IdInfo.Name "OwlBot"
            outInstruction $ OutCommand.Id $ IdInfo.Auther "Martin Tunge Sterri"
            outInstruction $ OutCommand.UciOk
            modify (\s ->  s {controllerStatus = Ready} )
            controllerLoop
        _ -> controllerStart

controllerLoop :: Eff '[Communication, State ControllerState, IO] ()
controllerLoop = do
    input <- getInstruction
    case input of
        InCommand.Quit -> do return ()
        InCommand.IsReady -> do
            outInstruction OutCommand.ReadyOk
            controllerLoop
        InCommand.Debug b -> do
            modify (\s -> s {debug = b})
            controllerLoop
        InCommand.UciNewGame -> do
            modify (\s -> s {board = newBoard})
            b <- gets board
            controllerLoop
        InCommand.Position ms -> do
            b <- gets board
            let nb = foldl (flip movePice) b ms
            modify (\s -> s {board = nb})
            b <- gets board
            controllerLoop
        InCommand.Go -> do
            b <- gets board
            let move = bestMoveSearch b 
            outInstruction $ OutCommand.BestMove move
            controllerLoop
        _ -> do
            controllerLoop


