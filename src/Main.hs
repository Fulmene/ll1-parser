module Main where

import System.IO
import System.Environment

import Grammar.CFG
import Grammar.LL1Parser

main = do
  args <- getArgs
  if length args == 2 then do
    hGrammar <- openFile (args !! 0) ReadMode
    hText <- openFile (args !! 1) ReadMode
    grammar <- hGetContents hGrammar
    text <- hGetContents hText
    if parseProperly (readCFG grammar) text then
      putStrLn "Accepted"
    else
      putStrLn "Rejected"
    hClose hGrammar
    hClose hText
  else
    putStrLn "Usage: parser [grammar-file] [text-file]"

