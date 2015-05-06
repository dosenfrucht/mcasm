module Generator where

import Data.Bits
import Data.Word
import Syntax
import Token
import Transform

opcodeOp :: Token -> Word8
opcodeOp tok = let (Just x) = lookup tok opList
               in x
  where opList =
          [ (PUSHA, 0x0E),
            (POPA, 0x0F),
            (RET, 0x2A),
            (NOP, 0x2B),
            (IRET, 0x2E),
            (HLT, 0x2F) ]

opcodeUnopReg :: Token -> Word8
opcodeUnopReg tok = let (Just x) = lookup tok opList
                    in x
  where opList =
          [ (PUSH8, 0x08),
            (PUSH16, 0x0A),
            (POP8, 0x0C),
            (POP16, 0x0D),
            (NOT, 0x22),
            (CALL, 0x28),
            (INT, 0x2C),
            (JMP, 0x32),
            (JE, 0x34),
            (JNE, 0x36),
            (JG, 0x38),
            (JGE, 0x3A),
            (JL, 0x3C),
            (JLE, 0x3E) ]

opcodeUnopImm :: Token -> Word8
opcodeUnopImm tok = let (Just x) = lookup tok opList
                    in x
  where opList =
          [ (PUSH8, 0x09),
            (PUSH16, 0x0B),
            (CALL, 0x29),
            (INT, 0x2D),
            (JMP, 0x33),
            (JE, 0x35),
            (JNE, 0x37),
            (JG, 0x39),
            (JGE, 0x3B),
            (JL, 0x3D),
            (JLE, 0x3F) ]

opcodeBinopRegReg :: Token -> Word8
opcodeBinopRegReg tok = let (Just x) = lookup tok opList
                        in x
  where opList =
          [ (MOV, 0x00),
            (ADD, 0x10),
            (SUB, 0x12),
            (MUL, 0x14),
            (DIV, 0x16),
            (SHR, 0x18),
            (SHL, 0x1A),
            (AND, 0x1C),
            (OR,  0x1E),
            (XOR, 0x20),
            (CMP, 0x30) ]

opcodeBinopRegImm :: Token -> Word8
opcodeBinopRegImm tok = let (Just x) = lookup tok opList
                        in x
  where opList =
          [ (MOV, 0x01),
            (ADD, 0x11),
            (SUB, 0x13),
            (MUL, 0x15),
            (DIV, 0x17),
            (SHR, 0x19),
            (SHL, 0x1B),
            (AND, 0x1D),
            (OR,  0x1F),
            (XOR, 0x21),
            (CMP, 0x31) ]

opcodeBinopAddrReg :: Token -> Word8
opcodeBinopAddrReg tok = let (Just x) = lookup tok opList
                         in x
  where opList =
          [ (LOAD8, 0x02),
            (LOAD16, 0x03),
            (STORE8, 0x04),
            (STORE16, 0x06) ]

opcodeBinopAddrImm :: Token -> Word8
opcodeBinopAddrImm tok = let (Just x) = lookup tok opList
                         in x
  where opList =
          [ (STORE8, 0x05),
            (STORE16, 0x07) ]

integerToWord16 :: Integer -> Word16
integerToWord16 i = fromInteger $ i .&. 0xFFFF

word16ToWord8 :: Word16 -> Word8
word16ToWord8 i = fromInteger $ toInteger i .&. 0xFF

word16ToBytes :: Word16 -> [Word8]
word16ToBytes w = higher : lower : []
  where higher = word16ToWord8 $ (w .&. 0xFF00) `shift` (-8)
        lower  = word16ToWord8 $ (w .&. 0x00FF)

integerToBytes :: Integer -> [Word8]
integerToBytes = word16ToBytes . integerToWord16

integerToWord8 :: Integer -> Word8
integerToWord8 i = fromInteger $ i .&. 0xFF

word8ToInt :: Word8 -> Int
word8ToInt = fromIntegral

immToBytes :: Imm -> [Word8]
immToBytes (Literal i) = integerToBytes i

addrToBytes :: Addr -> [Word8]
addrToBytes (AddrReg r)      = (0x80 .|. integerToWord8 r) : [0x00, 0x00]
addrToBytes (AddrConstant i) = 0x00 : immToBytes i

magicNumber :: [Word8]
magicNumber = [0x26, 0x03, 0x20, 0x13]

generateHeader :: [AST] -> [Word8]
generateHeader ast = len ++ [0x00, 0x00] ++ rawHeaders
  where len       = integerToBytes . toInteger $ length headers
        positions = readPositions 0 ast
        headers   = getHeader `fmap` positions
        rawHeaders = concat $ flattenHeader `fmap` headers
        flattenHeader (a, b, c) = a ++ b ++ c ++ [0x00, 0x00]

getHeader :: (Pos, Pos, [AST]) -> ([Word8], [Word8], [Word8])
getHeader (raw, mapped, ast) = (integerToBytes raw, integerToBytes mapped, size)
  where size         = integerToBytes $ getSize 0 remainingAst
        remainingAst = takeWhile p ast
        p (Position _) = False
        p _            = True

assemble :: [AST] -> [Word8]
assemble ast = magicNumber ++ generateHeader ast ++ assembleAST nAst
  where nAst = removePositions ast

assembleAST :: [AST] -> [Word8]
assembleAST []          = []
assembleAST (Syntax.EOF:_)     = []
assembleAST (Op tok:xs) = opcodeOp tok : 0x00 : assembleAST xs
assembleAST (BinopRegReg op reg0 reg1 : xs) = opcodeBinopRegReg op :
  integerToWord8 reg0 : 0x00 : integerToWord8 reg1 : assembleAST xs
assembleAST (BinopRegImm op reg imm : xs) = opcodeBinopRegImm op :
  integerToWord8 reg : immToBytes imm ++ assembleAST xs
assembleAST (BinopAddrReg op addr reg : xs) = opcodeBinopAddrReg op :
  addrToBytes addr ++ [0x00, integerToWord8 reg] ++ assembleAST xs
assembleAST (BinopAddrImm op addr imm : xs) = opcodeBinopAddrImm op :
  addrToBytes addr ++ immToBytes imm ++ assembleAST xs
assembleAST (UnopReg op reg : xs) = opcodeUnopReg op : integerToWord8 reg :
  assembleAST xs
assembleAST (UnopImm op imm : xs) = opcodeUnopImm op : 0x00 : immToBytes imm ++
  assembleAST xs
assembleAST (Syntax.DB (Literal d) : xs) = integerToWord8 d : assembleAST xs
assembleAST (Syntax.DW (Literal d) : xs) = integerToBytes d ++ assembleAST xs
assembleAST (Syntax.RESB : xs) = 0x00 : assembleAST xs
assembleAST (Position _ : xs) = assembleAST xs
