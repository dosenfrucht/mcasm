module Token where

data Token = Identifier String
           | Number Integer
           | Label String
           | StringLit String

           -- Registers

           | RAX
           | RBX
           | RCX
           | RDX
           | RSI
           | RDI
           | RBP
           | RSP
           | R8
           | R9
           | R10
           | R11
           | R12
           | R13
           | R14
           | R15

           -- Mnemonics

           | MOV
           | LOAD8
           | LOAD16
           | STORE8
           | STORE16
           | PUSH8
           | PUSH16
           | POP8
           | POP16
           | PUSHA
           | POPA

           | ADD
           | SUB
           | MUL
           | DIV
           | SHR
           | SHL
           | AND
           | OR
           | XOR
           | NOT

           | CALL
           | RET
           | NOP
           | INT
           | IRET
           | HLT

           | CMP
           | JMP
           | JE
           | JNE
           | JG
           | JGE
           | JL
           | JLE

           -- Directives

           | POSITION

           | TIMES

           | DB
           | DW

           | RESB
           | RESW

           -- Symbols

           | Comma
           | ParenO
           | ParenC
           | EOL
           deriving (Show, Eq)