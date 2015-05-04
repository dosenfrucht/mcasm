module Main where

import Lexer
import Token
import Parser
import Syntax
import Transform
import Generator

import System.IO
import System.Environment
import System.Exit
import Data.Char
import Data.Word

import Numeric (showHex)

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
printErrors ast = mapM_ (hPutStrLn stderr) errors
  where errors = map (\(Error s) -> s) $ filter p ast
        p (Error _) = True
        p _         = False

printHex :: [Word8] -> IO ()
printHex []     = return ()
printHex [x]    = putStr $ showHx x
printHex (x:xs) = (putStr $ showHx x) >> putChar ' ' >> printHex xs

showHx :: Word8 -> String
showHx x = if length hex == 1
            then '0' : hex
            else hex
  where hex = showHex x ""

getInput :: [String] -> String
getInput []          = []
getInput ("-v":xs)   = getInput xs
getInput ("-o":_:xs) = getInput xs
getInput ("-o":xs)   = getInput xs
getInput (x:xs)      = x

getVerbose :: [String] -> Bool
getVerbose = ("-v" `elem`)

getOutput :: [String] -> String
getOutput []          = []
getOutput ("-o":x:_) = x
getOutput (_:xs)      = getOutput xs

main :: IO ()
main = do
  args <- getArgs
  let input   = getInput args
      output  = case getOutput args of
                  [] -> "a.mcbin"
                  x  -> x
      verbose = getVerbose args
  if null input
   then hPutStrLn stderr "No input file given" >> exitFailure
   else return ()
  src <- readFile input
  let ast = getAst src
  if hasErrors ast
   then printErrors ast
   else let assembledData = assemble ast
        in do
             writeFile output (map (chr . word8ToInt) assembledData)
             if verbose
              then printHex assembledData >> putStrLn ""
              else return ()