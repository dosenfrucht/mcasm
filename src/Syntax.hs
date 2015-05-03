module Syntax where

import Token

data Addr = AddrReg  Reg
          | AddrConstant Imm
          deriving (Show)

type Reg = Integer

data Imm = Literal Integer
         | Label String
         deriving (Show)

data AST = BinopRegReg Token Reg Reg
         | BinopRegImm Token Reg Imm
         | BinopAddrReg Token Addr Reg
         | BinopAddrImm Token Addr Imm

         | UnopReg Token Reg
         | UnopImm Token Imm

         | Op Token

         | PosLabel String

         | Position Integer

         | EOF

         | Error String
         deriving (Show)