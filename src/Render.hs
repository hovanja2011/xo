module Render(module Render) where

    insideTag :: String -> String -> String
    insideTag tag content = "<" ++ tag ++ ">" ++ content ++ "</" ++ tag ++ ">"

    toTable :: [[String]] -> String
    toTable = insideTag "table" . concatMap (insideTag "tr") . map (concatMap (insideTag "td" . id))


    stringToInt :: String -> Int
    stringToInt s = read s 

    oneToTwo :: Show a => [a] -> Int -> [[String]]
    oneToTwo cells n = map (\xs -> map show xs) [[cells !! (j*n+i) | i <- [0..n-1]] | j <- [0..n-1]]    

    getColName :: Int -> [String]
    getColName n = [" "] ++  [show i | i <- [1 .. n]]

    getRowName :: Int -> [String]
    getRowName n = [" "] ++ (getWords n)

    getWords :: Int -> [String]
    getWords n = map (:[]) (take n ['A' .. ])

    boardToTable :: Show a => [a] -> Int -> [[String]]
    boardToTable board n = [getColName n] ++ [ ((getRowName n)  !! j ) : [ show(board !! (j*n-n + k)) | k <- [0 .. n-1] ]| j <- [1..n]]

    greeting :: String
    greeting = header ++ (unlines [(insideTag "h1" "The game is beginning. ")]) ++ footer0

    invitation :: Show a => String -> [a] -> Int -> String
    invitation move board n = unlines [
        header,
        (insideTag "h1" $ move ++ " 's turn."), 
        (insideTag "h1" $ "Pick a cell from A1 to " ++ (getRowName n) !! n ++ (show n)++"."),
        toTable (boardToTable board n),
        footer
        ]

    winning :: Show a => String -> [a] -> Int -> String
    winning move board n = unlines [
        header , 
        (insideTag "h1" $ "Winner! " ++ (show move) ++ " has won!"), 
        toTable $ boardToTable board n,
        footer0
        ]

    failing :: Show a => String -> String -> [a] -> Int -> String
    failing move err board n = unlines [
        header,
        (insideTag "h1" $ move ++ " 's turn."), 
        (insideTag "h1" err),
        toTable $ boardToTable board n,
        footer
        ]

    header :: String
    header = "<!DOCTYPE html>\n\
\<html lang=\"en\">\n\
\<head>\n\
\    <style>\n\
\        tr,\n\
\        td {\n\
\            border: 1px solid grey;\n\
\            width: 80px;\n\
\           height: 80px;\n\
\            text-align: center;\n\
\        }\n\
\    </style>\n\
\</head>\n\
\<body> "

    footer0 :: String
    footer0 = "</html>"

    footer :: String
    footer = "    <h2 class=\"step\">Write your step: </h2>\n\
\    <form class=\"myForm\" method=\"POST\" action=\"/\">\n\
\        <input name=\"cell\">\n\
\        <button>Send</button>\n\
\    </form>\n\
\</html>"
