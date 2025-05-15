module Main where

import Data.Char
import Data.List
import Data.Ord
import System.Environment

data Tree = Letter Char Int | Node Int Tree Tree deriving (Show, Read)

countlowest::Tree->(Char, Int)->Tree
countlowest (Letter _char _count) (_newchar, _newcount) = Node (_newcount+_count) (Letter _char _count) (Letter _newchar _newcount)
countlowest (Node _count _lowtree _hightree) (_newchar, _newcount) = Node (_newcount+_count) (Node _count _lowtree _hightree) (Letter _newchar _newcount)
--If first argument is a letter, returns a node with collective count of new character and all preceding
--If first argument is a node, returns a node with collective count, lowtree as the inputted node and hightree as new letter

countlowestlist::[(Char, Int)]->Tree
countlowestlist list = foldl countlowest (Letter (fst $ head $ reverse list) (snd $ head $ reverse list)) (drop 1 (reverse list))
--First counts the lowest 2 together and returns a tree. Then, for every next element, counts it with the returned tree and repeats

--helper function
getpath::Tree->String->[(Char, String)]
getpath (Letter _char _) path = [(_char, path)]
getpath (Node _ _lowtree _hightree) path = getpath _lowtree (path++"1") ++ getpath _hightree (path++"0")
--If first argument is a letter, return a tuple with the character and the constructed path to that character
--If first argument is a node, add "1" to the path when tracing lower count character, otherwise add "0" to path, and recall function
--(creates a list of tuples by adding the result of the function to the list, essentially creating a table)

converttree::Tree->[(Char, String)]
converttree tree = getpath tree ""
--Initiates getpath with clean arguments

getcharcode::Char->[(Char, String)]->String
getcharcode _char _table = case lookup _char _table of
    Just _code -> _code
    Nothing -> ""
--Check the table. If the character has a binary code, return it

main::IO ()
main = do
    args <- getArgs
    case args of
        [file, output, treefile] -> do
            text <- readFile file
            let charcount = [(grouped, length $ filter (== grouped) text) | grouped <- map head (group $ sort text)] --for every unique character, get amount of occurrences in text
            let sortcount = reverse $ sortBy (comparing snd) charcount --sort charcount by occurrences
            let tree = countlowestlist sortcount --make tree
            let table = converttree tree --convert tree to table
            let newtext = concat [getcharcode x table | x <- text] --convert characters to corresponding binary code
            
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