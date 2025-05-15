module Main where

import Data.Char
import Data.List
import Data.Ord
import System.Environment

data Tree = Letter Char Int | Node Int Tree Tree deriving (Show, Read)

--helper
pathfind::Tree->String->Tree->String
pathfind _curtree [] _tree = ""
pathfind (Letter _char _count) _text _tree = [_char]++(pathfind _tree _text _tree)
pathfind (Node _val _lowtree _hightree) _text _tree = if head _text == '0' then pathfind _hightree (drop 1 _text) _tree else pathfind _lowtree (drop 1 _text) _tree
--Takes the current tree, remaining binary string, main tree and constructed text
--If there's no binary in the string, return empty string
--If first argument is a letter, add the character to the constructed text and recall function from full tree and remaining bits
--If first argument is a node:
--  if current bit is 0 -> recall function with the higher count character/node and the next bit
--  if current bit is 1 -> recall function with the lower count character/node and the next bit

decompress::Tree->String->String
decompress _tree _text = pathfind _tree _text _tree
--Initiate pathfind with clean arguments

main::IO ()
main = do
    args <- getArgs
    case args of
        [file, output, treefile] -> do
            text <- readFile file --load input tile into text var
            tree <- readFile treefile --load treefile into tree var
            let newtext = decompress (read tree) text --newtext is constructed text from bits and tree
            writeFile output newtext --write newtext to output file
            putStrLn ("length of decompressed file: "++(show $ length newtext)++" characters, "++(show ((length newtext)*8))++" bits.")
            putStrLn ("compression completed.")
            
        _ -> putStrLn "Wrong number of arguments"