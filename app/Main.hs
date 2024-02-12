{-# LANGUAGE OverloadedStrings #-}

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
      location <- formParam ("cell" )
      move <- liftIO $ readIORef moveIO
      board1 <- liftIO $ readIORef boardIO
      case assignCell location move board1 n of
        Fail err _ -> do
          board2 <- liftIO $ readIORef boardIO
          liftIO . writeFile "xoGame.html" $ failing (show move) err board2 n
          redirect "/" 
        Success newBoard -> do
          liftIO $ modifyIORef boardIO $ (\_ -> newBoard)
          case isThereAWinner move (oneToTwo newBoard n) n of
            True -> do
              -- board3 <- liftIO $ readIORef boardIO
              liftIO . writeFile "xoGame.html" $ winning (show move) newBoard n
              redirect "/finish" 
            False -> do
              liftIO $ modifyIORef moveIO $ nextMove
              move1 <- liftIO $ readIORef moveIO
              board3 <- liftIO $ readIORef boardIO
              liftIO . writeFile "xoGame.html" $ invitation (show move1) board3 n
              redirect "/" 
    get "/finish" $
      file "xoGame.html"

    