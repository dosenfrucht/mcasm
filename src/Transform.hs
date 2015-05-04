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
readLabels start (Syntax.DB _ : xs)        = readLabels (start + 1) xs
readLabels start (Syntax.DW _ : xs)        = readLabels (start + 1) xs
readLabels start (Syntax.RESB : xs)        = readLabels (start + 1) xs
readLabels _     (Position i : xs)         = readLabels i xs

readPositions :: Pos -> [AST] -> [(Pos, Pos, [AST])]
readPositions _   []                        = []
readPositions _   (Syntax.EOF : _)          = []
readPositions pos (Error _ : xs)            = readPositions pos xs
readPositions pos (PosLabel _ : xs)         = readPositions pos xs
readPositions pos (BinopRegReg _ _ _ : xs)  = readPositions (pos + 4) xs
readPositions pos (BinopRegImm _ _ _ : xs)  = readPositions (pos + 4) xs
readPositions pos (BinopAddrReg _ _ _ : xs) = readPositions (pos + 6) xs
readPositions pos (BinopAddrImm _ _ _ : xs) = readPositions (pos + 6) xs
readPositions pos (UnopReg _ _ : xs)        = readPositions (pos + 2) xs
readPositions pos (UnopImm _ _ : xs)        = readPositions (pos + 4) xs
readPositions pos (Op _ : xs)               = readPositions (pos + 2) xs
readPositions pos (Syntax.DB _ : xs)        = readPositions (pos + 1) xs
readPositions pos (Syntax.DW _ : xs)        = readPositions (pos + 2) xs
readPositions pos (Syntax.RESB : xs)        = readPositions (pos + 1) xs
readPositions pos (Position i : xs)         = (pos, i, xs) : readPositions pos xs

getSize :: Integer -> [AST] -> Integer
getSize size []               = size
getSize size (Syntax.EOF : _) = size
getSize size (Error _ : xs)            = getSize size xs
getSize size (PosLabel _ : xs)         = getSize size xs
getSize size (BinopRegReg _ _ _ : xs)  = getSize (size + 4) xs
getSize size (BinopRegImm _ _ _ : xs)  = getSize (size + 4) xs
getSize size (BinopAddrReg _ _ _ : xs) = getSize (size + 6) xs
getSize size (BinopAddrImm _ _ _ : xs) = getSize (size + 6) xs
getSize size (UnopReg _ _ : xs)        = getSize (size + 2) xs
getSize size (UnopImm _ _ : xs)        = getSize (size + 4) xs
getSize size (Op _ : xs)               = getSize (size + 2) xs
getSize size (Syntax.DB _ : xs)        = getSize (size + 1) xs
getSize size (Syntax.DW _ : xs)        = getSize (size + 2) xs
getSize size (Syntax.RESB : xs)        = getSize (size + 1) xs
getSize size (Position _ : xs)         = getSize size xs

removeLabels :: [AST] -> [AST]
removeLabels []                = []
removeLabels (Syntax.EOF : _)  = []
removeLabels (PosLabel _ : xs) = removeLabels xs
removeLabels (x:xs)            = x : removeLabels xs

removePositions :: [AST] -> [AST]
removePositions []                = []
removePositions (Syntax.EOF : _)  = []
removePositions (Position _ : xs) = removePositions xs
removePositions (x:xs)            = x : removePositions xs

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
replaceLabels (Syntax.DB imm : xs) m =
  let nImm = insertLabelImm imm m
      nAst = case nImm of
               Just a  -> Syntax.DB a
               Nothing -> Error $ ulerr ++ getLabelImm imm
  in nAst : replaceLabels xs m
replaceLabels (Syntax.DW imm : xs) m =
  let nImm = insertLabelImm imm m
      nAst = case nImm of
               Just a  -> Syntax.DW a
               Nothing -> Error $ ulerr ++ getLabelImm imm
  in nAst : replaceLabels xs m
replaceLabels [] _       = []
replaceLabels (x : xs) m = x : replaceLabels xs m