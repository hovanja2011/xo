{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Web.Scotty

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Data.Char(toUpper)
import Network.Wai.Middleware.RequestLogger

import Logic
import Render

main :: IO ()
main = do
  m <- newMVar (3 :: Int, X :: Move, [] :: [Cell])
  scotty 3000 $ do
    middleware logStdoutDev
    get "/start" $
      file "xoStart.html" 

    post "/start" $ do
      shapeS <- formParam ("shape") 
      let shape = stringToInt shapeS
      liftIO $ modifyMVar_ m $ \( _, _, _) -> return (stringToInt shapeS, X, replicate (shape * shape) Empty)
      (n, move, board) <- liftIO $ readMVar m
      liftIO $ writeFile "xoGame.html" $ invitation (show move) board n
      redirect "/" 

    get "/" $
      file "xoGame.html"  

    post "/" $ do
      location <- formParam ("cell" )
      let loc = map toUpper location
      (n, move, board) <- liftIO $ readMVar m
      case assignCell loc move board n of
        Fail err _ -> do
          liftIO . writeFile "xoGame.html" $ failing (show move) err board n
          redirect "/" 
        Success newBoard -> do
          liftIO $ modifyMVar_ m $ \( shape, _, _) -> return (shape, move, newBoard)
          case isThereAWinner move (arrayToTable newBoard n) n of
            True -> do
              liftIO . writeFile "xoGame.html" $ winning (show move) newBoard n
              redirect "/finish" 
            False -> do
              liftIO $ modifyMVar_ m $ \( shape, _, _) -> return (shape, nextMove move, newBoard)
              liftIO . writeFile "xoGame.html" $ invitation (show $ nextMove move) newBoard n
              redirect "/" 

    get "/finish" $
      file "xoGame.html"

  