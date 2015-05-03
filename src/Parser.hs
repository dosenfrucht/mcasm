module Parser where

import Token
import qualified Syntax as S

parseAll :: [Token] -> [S.AST]
parseAll [] = []
parseAll xs = st : parseAll rest
  where (st, rest) = parseLine xs









isBinopRegReg :: Token -> Bool
isBinopRegReg = (`elem` binopRegRegs)
  where binopRegRegs =
          [ MOV,
            ADD,
            SUB,
            MUL,
            DIV,
            SHR,
            SHL,
            AND,
            OR,
            XOR,
            CMP ]

isBinopRegImm :: Token -> Bool
isBinopRegImm = isBinopRegReg

isBinopAddrReg :: Token -> Bool
isBinopAddrReg = (`elem` binopAddrRegs)
  where binopAddrRegs =
          [ LOAD8,
            LOAD16,
            STORE8,
            STORE16 ]

isBinopAddrImm :: Token -> Bool
isBinopAddrImm = (`elem` [ STORE8, STORE16 ])

isUnopReg :: Token -> Bool
isUnopReg = (`elem` unopRegs)
  where unopRegs =
          [ PUSH8,
            PUSH16,
            POP8,
            POP16,
            NOT,
            CALL,
            INT,
            JMP,
            JE,
            JNE,
            JG,
            JGE,
            JL,
            JLE ]

isUnopImm :: Token -> Bool
isUnopImm = (`elem` unopImms)
  where unopImms =
          [ PUSH8,
            PUSH16,
            CALL,
            INT,
            JMP,
            JE,
            JNE,
            JG,
            JGE,
            JL,
            JLE ]

isOp :: Token -> Bool
isOp = (`elem` ops)
  where ops =
          [ RET,
            NOP,
            IRET,
            HLT ]


isImm :: Token -> Bool
isImm (Number _)     = True
isImm (Identifier _) = True
isImm _              = False

immFromTok :: Token -> S.Imm
immFromTok (Number n)     = S.Literal n
immFromTok (Identifier i) = S.Label i

isReg :: Token -> Bool
isReg = (`elem` regs)
  where regs =
          [ RAX,
            RBX,
            RCX,
            RDX,
            RSI,
            RDI,
            RBP,
            RSP,
            R8,
            R9,
            R10,
            R11,
            R12,
            R13,
            RFLAGS,
            RIP ]

regToAST :: Token -> S.Reg
regToAST reg = let (Just i) = lookup reg regList
               in i
  where regList =
          [ (RAX, 0),
	    (RBX, 1),
	    (RCX, 2),
	    (RDX, 3),
	    (RSI, 4),
	    (RDI, 5),
	    (RBP, 6),
	    (RSP, 7),
	    (R8,  8),
	    (R9,  9),
	    (R10, 10),
	    (R11, 11),
	    (R12, 12),
	    (R13, 13),
	    (RFLAGS, 14),
	    (RIP, 15) ]

isAddr :: Token -> Bool
isAddr t | isReg t    = True
         | isImm t    = True
         | otherwise  = False

addrFromTok :: Token -> S.Addr
addrFromTok t | isReg t   = S.AddrReg (regToAST t)
              | isImm t   = S.AddrConstant (immFromTok t)

parseLine :: [Token] -> (S.AST, [Token])
parseLine xs =
  case xs of
    (EOL : rest)     -> parseLine rest
    (Label s : rest) -> (S.PosLabel s, rest)
    (POSITION : Number _ : EOL : _) -> parsePosition xs
    (op : reg0 : Comma : reg1 : EOL : _)
      |    isBinopRegReg op
        && isReg reg0
        && isReg reg1 -> parseBinopRegReg xs
    (op : reg : Comma : imm : EOL : _)
      |    isBinopRegImm op
        && isReg reg
        && isImm imm -> parseBinopRegImm xs
    (op : ParenO : to : ParenC : Comma : reg : EOL : _)
      |    isBinopAddrReg op
        && isAddr to
        && isReg reg -> parseBinopAddrReg xs
    (op : ParenO : to : ParenC : Comma : imm : EOL : _)
      |    isBinopAddrImm op
        && isAddr to
        && isImm imm -> parseBinopAddrImm xs
    (op : reg : EOL : _)
      |    isUnopReg op
        && isReg reg -> parseUnopReg xs
    (op : imm : EOL : _)
      |    isUnopImm op
        && isImm imm -> parseUnopImm xs
    (op : EOL : rest)
      | isOp op -> (S.Op op, rest)
    [] -> (S.EOF, [])
    s -> (S.Error $ "Invalid symbol or mnemonic: " ++ show s, rest)
      where rest = safeTail $ dropWhile (/= EOL) xs
            safeTail []     = []
            safeTail (_:xs) = xs


parsePosition :: [Token] -> (S.AST, [Token])
parsePosition (POSITION : Number i : EOL : xs) = (ast, xs)
  where ast = S.Position i

parseBinopRegReg :: [Token] -> (S.AST, [Token])
parseBinopRegReg (mne : reg0 : Comma : reg1 : EOL : xs) = (ast, xs)
  where ast = S.BinopRegReg mne (regToAST reg0) (regToAST reg1)

parseBinopRegImm :: [Token] -> (S.AST, [Token])
parseBinopRegImm (mne : reg : Comma : imm : EOL : xs) = (ast, xs)
  where ast = S.BinopRegImm mne (regToAST reg) (immFromTok imm)

parseBinopAddrReg :: [Token] -> (S.AST, [Token])
parseBinopAddrReg (mne : ParenO : to : ParenC : Comma : reg : EOL : xs) =
  (ast, xs)
  where ast = S.BinopAddrReg mne (addrFromTok to) (regToAST reg)

parseBinopAddrImm :: [Token] -> (S.AST, [Token])
parseBinopAddrImm (mne : ParenO : to : ParenC : Comma : imm : EOL : xs) =
  (ast, xs)
  where ast = S.BinopAddrImm mne (addrFromTok to) (immFromTok imm)

parseUnopReg :: [Token] -> (S.AST, [Token])
parseUnopReg (op : reg : EOL : xs) = (ast, xs)
  where ast = S.UnopReg op (regToAST reg)

parseUnopImm :: [Token] -> (S.AST, [Token])
parseUnopImm (op : imm : EOL : xs) = (ast, xs)
  where ast = S.UnopImm op (immFromTok imm)