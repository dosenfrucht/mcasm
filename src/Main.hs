module Main where

import Lexer
import Token
import Parser
import Syntax
import Transform
import Generator

import System.IO
import System.Environment
import Data.Char

version :: String
version = "0.0"

getAst :: String -> [AST]
getAst src = let toks   = toTokens src ++ [EOL]
                 astL   = parseAll toks
                 labels = readLabels 0 astL
                 astNL  = removeLabels astL
             in replaceLabels astNL labels

hasErrors :: [AST] -> Bool
hasErrors ast = not . null . filter p $ ast
  where p (Error _) = True
        p _         = False

printErrors :: [AST] -> IO ()
printErrors ast = mapM_ putStrLn errors
  where errors = map (\(Error s) -> s) $ filter p ast
        p (Error _) = True
        p _         = False

main :: IO ()
main = do
  args <- getArgs
  src <- readFile "test.asm"
  let ast = getAst src
  if hasErrors ast
   then printErrors ast
   else let assembledData = assemble ast
        in do
             writeFile "test.mcbin" (map (chr . word8ToInt) assembledData)
             if "-v" `elem` args
              then print assembledData
              else return ()