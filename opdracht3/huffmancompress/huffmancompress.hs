module Main where

import Data.Char
import Data.List
import Data.Ord
import System.Environment

data Tree = Letter Char Int | Node Int Tree Tree deriving (Show, Read)

countlowest::Tree->(Char, Int)->Tree
countlowest (Letter _char _count) (_newchar, _newcount) = Node (_newcount+_count) (Letter _char _count) (Letter _newchar _newcount)
countlowest (Node _count _lowtree (Letter _char _charcount)) (_newchar, _newcount) = Node (_newcount+_count) (countlowest _lowtree (_char, _charcount)) (Letter _newchar _newcount)

countlowestlist::[(Char, Int)]->Tree
countlowestlist list = foldl countlowest (Letter (fst $ head $ reverse list) (snd $ head $ reverse list)) (drop 1 (reverse list))

--helper function
getpath::Tree->String->[(Char, String)]
getpath (Letter _char _) path = [(_char, path)]
getpath (Node _ _lowtree _hightree) path = getpath _lowtree (path++"1") ++ getpath _hightree (path++"0")

converttree::Tree->[(Char, String)]
converttree tree = getpath tree ""

getcharcode::Char->[(Char, String)]->String
getcharcode _char _table = case lookup _char _table of
    Just _code -> _code
    Nothing -> ""

main::IO ()
main = do
    args <- getArgs
    case args of
        [file, output, treefile] -> do
            text <- readFile file
            let charcount = [(grouped, length $ filter (== grouped) text) | grouped <- map head (group $ sort text)]
            let sortcount = reverse $ sortBy (comparing snd) charcount
            let tree = countlowestlist sortcount
            let table = converttree tree
            let newtext = concat [getcharcode x table | x <- text]
            
            putStrLn ("length of text "++(file)++": "++(show $ length text)++" characters, "++(show ((length text)*8))++" bits.")
            putStrLn ("length of compressed file "++(output)++": "++(show $ length newtext)++" bits.")
            putStrLn ("factor: "++(show $ length newtext)++"/"++(show ((length text)*8))++"*100="++(show ((fromIntegral $ length newtext)/(fromIntegral $ (length text)*8)*100))++"%")

            writeFile output newtext
            writeFile treefile (show tree)
            putStrLn ("compression completed.")
            
        _ -> putStrLn "Wrong number of arguments"


--fileToCompress, compressData, compressTree
--Use Serialisation technique
--print percentage
--test with 500 words ascii