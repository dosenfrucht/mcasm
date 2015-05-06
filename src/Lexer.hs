module Lexer (
  toTokens
) where

import Token

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Applicative ((<$>))
import Data.Char
import Data.List (foldl')

spaces :: Parser String
spaces = readWhile (`elem` " \t")

rawToTokens :: String -> [Token]
rawToTokens s = case parse (spaces >> many1 parseFun) "mcasm" s of
                  Left  _   -> []
                  Right res -> res
 where parseFun = (identifier <|> readNumber <|> stringLit <|> otherSymbol
                     <|> comment) >>= \tok ->
                  spaces >> return tok

stringLit :: Parser Token
stringLit = do
  char '"'
  content <- many1 $ noneOf "\""
  char '"'
  return $ StringLit content

toTokens :: String -> [Token]
toTokens s = removeTrailingEOL $ trimToks toks
  where toks = rawToTokens s
        removeTrailingEOL (EOL:xs) = xs
        removeTrailingEOL xs       = xs

identifier :: Parser Token
identifier = do
  first <- char '_' <|> letter
  raw <- many $ char '_' <|> alphaNum <|> char '.'
  let tok = getToken $ first : raw
  (char ':' >> case tok of
                     Identifier s -> return $ Label s
                     _            -> fail "unexpected :")
   <|> return tok
  where getToken s = case toLower <$> s of

                       -- Registers
                       "rax"    -> RAX
                       "rbx"    -> RBX
                       "rcx"    -> RCX
                       "rdx"    -> RDX
                       "rsi"    -> RSI
                       "rdi"    -> RDI
                       "rbp"    -> RBP
                       "rsp"    -> RSP
                       "r8"     -> R8
                       "r9"     -> R9
                       "r10"    -> R10
                       "r11"    -> R11
                       "r12"    -> R12
                       "r13"    -> R13
                       "r14"    -> R14
                       "r15"    -> R15

                       -- Mnemonics
                       "mov"     -> MOV
                       "load8"   -> LOAD8
                       "load16"  -> LOAD16
                       "store8"  -> STORE8
                       "store16" -> STORE16
                       "push8"   -> PUSH8
                       "push16"  -> PUSH16
                       "pop8"    -> POP8
                       "pop16"   -> POP16
                       "pusha"   -> PUSHA
                       "popa"    -> POPA

                       "add"     -> ADD
                       "sub"     -> SUB
                       "mul"     -> MUL
                       "div"     -> DIV
                       "shr"     -> SHR
                       "shl"     -> SHL
                       "and"     -> AND
                       "or"      -> OR
                       "xor"     -> XOR
                       "not"     -> NOT

                       "call"    -> CALL
                       "ret"     -> RET
                       "nop"     -> NOP
                       "int"     -> INT
                       "iret"    -> IRET
                       "hlt"     -> HLT

                       "cmp"     -> CMP
                       "jmp"     -> JMP
                       "je"      -> JE
                       "jne"     -> JNE
                       "jg"      -> JG
                       "jge"     -> JGE
                       "jl"      -> JL
                       "jle"     -> JLE

                       -- Directives

                       "position" -> POSITION
                       "times"    -> TIMES
                       "db"       -> DB
                       "dw"       -> DW
                       "resb"     -> RESB
                       "resw"     -> RESW
                       name       -> Identifier name

otherSymbol :: Parser Token
otherSymbol = do
  tok <-     try (char ',' >> return Comma)
         <|> try (char '[' >> return ParenO)
         <|> try (char '\n' >> return EOL)
         <|> (char ']' >> return ParenC)
  return tok


trimToks :: [Token] -> [Token]
trimToks (EOL:EOL:xs) = trimToks $ EOL : xs
trimToks (x:xs)       = x : trimToks xs
trimToks []           = []

readWhile :: (Char -> Bool) -> Parser [Char]
readWhile p = try (anyChar >>= \c -> if p c
                                      then (c:) <$> readWhile p
                                      else fail "")
              <|> return []

readNumber :: Parser Token
readNumber =
      try (string "0x" >> readNumberHex)
  <|> try (string "0b" >> readNumberBin)
  <|> try (string "0" >> readNumberOct)
  <|> readNumberDec

readNumberDec :: Parser Token
readNumberDec = readNumbersystem 10 isDigit >>= \t ->
                spaces >> return t

readNumberBin :: Parser Token
readNumberBin = readNumbersystem 2 (\c -> c `elem` "10") >>= \t ->
                spaces >> return t

readNumberOct :: Parser Token
readNumberOct = readNumbersystem 8 isOctDigit >>= \t ->
                spaces >> return t

readNumberHex :: Parser Token
readNumberHex = readNumbersystem 16 isHexDigit >>= \t ->
                spaces >> return t


readNumbersystem :: Integer -> (Char -> Bool) -> Parser Token
readNumbersystem base p = do
  raw <- filter (/= '_') <$> readWhile (\c -> p c || c == '_')
  if raw == []
   then fail "No number given"
   else return ()
  let result = parseIntBase base raw
  return $ Number result

parseIntBase :: Integer -> String -> Integer
parseIntBase n src = foldl' step 0 src
  where step acc x = toInteger $ acc * n + numFromChar x
        numFromChar x | isDigit x = toInteger $ charVal - ord '0'
                      | otherwise = toInteger $ charVal - (ord 'a') + 10
          where charVal = ord . toLower $ x

comment :: Parser Token
comment = do
  string "--"
  readWhile (/= '\n')
  otherSymbol