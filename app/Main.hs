{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Logic
import Render
import Control.Monad.IO.Class (liftIO) 
import Data.IORef (modifyIORef, newIORef, readIORef)

main :: IO ()
main = do
  writeFile "xoGame.html" greeting
  putStrLn "Shape of board n = "
  ns <- getLine 
  let n = read ns :: Int
  moveIO <- newIORef (X :: Move)
  boardIO <- newIORef (replicate (n*n) Empty :: [Cell])
  board <- liftIO $ readIORef boardIO
  writeFile "xoGame.html" $ invitation "X" board n
  scotty 3000 $ do
    get "/" $
      file "xoGame.html"
    post "/" $ do
      location <- param ("cell" )
      move <- liftIO $ readIORef moveIO
      board <- liftIO $ readIORef boardIO
      case assignCell location move board n of
        Fail err _ -> do
          board <- liftIO $ readIORef boardIO
          liftIO . writeFile "xoGame.html" $ failing (show move) err board n
          redirect "/" 
        Success newBoard -> do
          liftIO $ modifyIORef boardIO $ (\_ -> newBoard)
          case isThereAWinner move (oneToTwo newBoard n) n of
            True -> do
              board <- liftIO $ readIORef boardIO
              liftIO . writeFile "xoGame.html" $ winning (show move) newBoard n
              redirect "/finish" 
            False -> do
              liftIO $ modifyIORef moveIO $ nextMove
              move <- liftIO $ readIORef moveIO
              board <- liftIO $ readIORef boardIO
              liftIO . writeFile "xoGame.html" $ invitation (show move) board n
              redirect "/" 
    get "/finish" $
      file "xoGame.html"

    