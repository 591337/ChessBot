{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Network.WebSockets as WS
import Control.Monad (forever)
import Control.Exception (finally)

import Server.Parser (parseInput)

import Data.Text (Text, pack, unpack)
import Board (newBoard, movePice, bestMoveSearch, Board)

main :: IO ()
main = do
    WS.runServer "127.0.0.1" 9160 application

application :: WS.PendingConnection -> IO ()
application pending = do
    conn <- WS.acceptRequest pending
    WS.withPingThread conn 30 (return ()) $ do
        finally (talk conn newBoard) (disconnect conn)

talk :: WS.Connection -> Board -> IO ()
talk conn b = do
    print b
    msg <- WS.receiveData conn
    case parseInput (unpack msg) of
        Just move -> do
            let newBoard = movePice move b
            let bestMove = bestMoveSearch newBoard
            WS.sendTextData conn (pack (show bestMove) :: Text)
            talk conn (movePice bestMove newBoard)
        _ -> WS.sendClose conn ("Bye!" :: Text)

disconnect :: WS.Connection -> IO ()
disconnect conn = do
    putStrLn "Client disconnected"
