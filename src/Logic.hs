module Logic(module Logic) where

import Data.List(elemIndex)
import Render

data Move = X | O
data Cell = Occupied Move | Empty
data CellTransform = Success [Cell] | Fail String [Cell]

instance Show Move where
  show X = "X"
  show O = "O"

instance Show Cell where
  show (Occupied X)     = "X"
  show (Occupied O)     = "O"
  show Empty            = " "

instance Eq Cell where
  Occupied X == Occupied X = True
  Occupied O == Occupied O = True
  Empty == Empty           = True
  _ == _                   = False

nextMove :: Move -> Move
nextMove X = O
nextMove O = X

getBoardIndex :: String -> Int -> Maybe Int
getBoardIndex (x:y:[]) n = 
  case (elemIndex [x] $ getWords n, elemIndex (read [y] :: Int) [1..n]) of
      (Just i, Just j)  -> Just (i*n+j)
      _ -> Nothing
getBoardIndex _ _ = Nothing

verifyIsFree ::  [Cell] -> Int -> Maybe Int
verifyIsFree board ix = if board !! ix == Empty then Just ix else Nothing

assignCell :: String -> Move -> [Cell] -> Int -> CellTransform
assignCell location move board n =
  case getBoardIndex location n >>= verifyIsFree board of
    Nothing -> Fail "Invalid move" board
    Just i -> Success ((take i board) ++ [Occupied move] ++ (drop (i+1) board))

isSame :: Move -> Bool -> [String] -> Bool
isSame _ b [] = b
isSame _ b (_:[]) = b
isSame move b (x:y:xs) = isSame move (and [b, x==y , x == show move]) (y:xs)

isThereAWinner :: Move -> [[String]] -> Int -> Bool
isThereAWinner move table n = or $ map (isSame move True) (rows ++ cols ++ diags)
  where
    rows  =  [[table !! i !! j | i <- [0..n-1]] | j <- [0..n-1]]
    cols  =  [[table !! j !! i | i <- [0..n-1]] | j <- [0..n-1]]
    diags =  [[table !! i !! i | i <- [0..n-1]], [table !! i !! j | i <- [0..n-1], let j = n-1-i ]]
