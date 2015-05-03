module Transform where

import Syntax
import Token

import qualified Data.Map as Map

type Error = String
type Pos = Integer

readLabels :: Pos -> [AST] -> Map.Map String Pos
readLabels _     []                        = Map.empty
readLabels _     (Syntax.EOF : _)          = Map.empty
readLabels start (Error _ : xs)            = readLabels start xs
readLabels start (PosLabel s : xs)         = Map.insert s start $ readLabels start xs
readLabels start (BinopRegReg _ _ _ : xs)  = readLabels (start + 4) xs
readLabels start (BinopRegImm _ _ _ : xs)  = readLabels (start + 4) xs
readLabels start (BinopAddrReg _ _ _ : xs) = readLabels (start + 6) xs
readLabels start (BinopAddrImm _ _ _ : xs) = readLabels (start + 6) xs
readLabels start (UnopReg _ _ : xs)        = readLabels (start + 2) xs
readLabels start (UnopImm _ _ : xs)        = readLabels (start + 4) xs
readLabels start (Op _ : xs)               = readLabels (start + 2) xs
readLabels _     (Position i : xs)         = readLabels i xs

removeLabels :: [AST] -> [AST]
removeLabels []                = []
removeLabels (Syntax.EOF : _)  = []
removeLabels (PosLabel _ : xs) = removeLabels xs
removeLabels (x:xs)            = x : removeLabels xs

getLabelImm :: Imm -> String
getLabelImm (Syntax.Label s) = s

getLabelAddr :: Addr -> String
getLabelAddr (AddrConstant s) = getLabelImm s

insertLabelImm :: Imm -> Map.Map String Integer -> Maybe Imm
insertLabelImm (Syntax.Label s) m = do
  val <- Map.lookup s m
  return $ Literal val
insertLabelImm imm _ = return imm

insertLabelAddr :: Addr -> Map.Map String Integer -> Maybe Addr
insertLabelAddr (AddrConstant s) m = do
  imm <- insertLabelImm s m
  return $ AddrConstant imm
insertLabelAddr addr _ = return addr

ulerr :: String
ulerr = "Unknown Label: "

replaceLabels :: [AST] -> Map.Map String Integer -> [AST]
replaceLabels (BinopRegImm op r imm : xs) m =
  let nImm = insertLabelImm imm m
      nAst = case nImm of
               Just a  -> BinopRegImm op r a
               Nothing -> Error $ ulerr ++ getLabelImm imm
  in nAst : replaceLabels xs m
replaceLabels (BinopAddrReg op addr reg : xs) m =
  let nAddr = insertLabelAddr addr m
      nAst  = case nAddr of
                Just a  -> BinopAddrReg op a reg
                Nothing -> Error $ ulerr ++ getLabelAddr addr
  in nAst : replaceLabels xs m
replaceLabels (BinopAddrImm op addr imm : xs) m =
  let nAddr = insertLabelAddr addr m
      nImm  = insertLabelImm imm m
      nAst  = case nAddr of
                Nothing -> case nImm of
                             Nothing -> Error $ ulerr ++ getLabelAddr addr ++
                                                 " and " ++ getLabelImm imm
                             Just _  -> Error $ ulerr ++ getLabelAddr addr
                Just a  -> case nImm of
                             Nothing -> Error $ ulerr ++ getLabelImm imm
                             Just b  -> BinopAddrImm op a b
  in nAst : replaceLabels xs m
replaceLabels (UnopImm op imm : xs) m =
  let nImm = insertLabelImm imm m
      nAst = case nImm of
               Just a  -> UnopImm op a
               Nothing -> Error $ ulerr ++ getLabelImm imm
  in nAst : replaceLabels xs m
replaceLabels [] _       = []
replaceLabels (x : xs) m = x : replaceLabels xs m