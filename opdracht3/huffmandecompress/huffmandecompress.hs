module Main where

import Data.Char
import Data.List
import Data.Ord
import System.Environment

data Tree = Letter Char Int | Node Int Tree Tree deriving (Show, Read)

traverse::Tree->Char->Tree

main::IO ()
main = do
    args <- getArgs
    case args of
        [file, output, tree] -> do
            text <- readFile file
            let charcount = [(grouped, length $ filter (== grouped) text) | grouped <- map head (group $ sort text)]
            let sortcount = reverse $ sortBy (comparing snd) charcount
            let tree = countlowestlist sortcount
            let table = converttree tree
            let newtext = [getcharcode x table | x <- text]
            putStrLn (concat newtext)
            
        _ -> putStrLn "Wrong number of arguments"


--fileToCompress, compressData, compressTree
--Use Serialisation technique
--print percentage
--test with 500 words ascii