module Render(module Render) where
import Consts

insideTag :: String -> String -> String
insideTag tag content = "<" ++ tag ++ ">" ++ content ++ "</" ++ tag ++ ">"

toHTMLTable :: [[String]] -> String
toHTMLTable = insideTag "table" . concatMap (insideTag "tr") . map (concatMap (insideTag "td" . id))

stringToInt :: String -> Int
stringToInt s = read s 

arrayToTable :: Show a => [a] -> Int -> [[String]]
arrayToTable cells n = map (\xs -> map show xs) [[cells !! (j*n+i) | i <- [0..n-1]] | j <- [0..n-1]]    

getColName :: Int -> [String]
getColName n = [" "] ++  [show i | i <- [1 .. n]]

getRowName :: Int -> [String]
getRowName n = [" "] ++ (getWords n)

getWords :: Int -> [String]
getWords n = map (:[]) (take n ['A' .. ])

boardToTable :: Show a => [a] -> Int -> [[String]]
boardToTable board n = [getColName n] ++ [ ((getRowName n)  !! j ) : [ show(board !! (j*n-n + k)) | k <- [0 .. n-1] ]| j <- [1..n]]

invitation :: Show a => String -> [a] -> Int -> String
invitation move board n = unlines [
    header,
    (insideTag "h1" $ move ++ " 's turn."), 
    (insideTag "h1" $ "Pick a cell from A1 to " ++ (getRowName n) !! n ++ (show n)++"."),
    toHTMLTable (boardToTable board n),
    footer
    ]

winning :: Show a => String -> [a] -> Int -> String
winning move board n = unlines [
    header , 
    (insideTag "h1" $ "Winner! " ++ (show move) ++ " has won!"), 
    toHTMLTable $ boardToTable board n,
    footer_base
    ]

failing :: Show a => String -> String -> [a] -> Int -> String
failing move err board n = unlines [
    header,
    (insideTag "h1" $ move ++ " 's turn."), 
    (insideTag "h1" err),
    toHTMLTable $ boardToTable board n,
    footer
    ]

