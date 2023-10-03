{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use guards" #-}
module Rewriting.TargetModule where

import Memoization.Core.Memory
import Memoization.Core.State
import Rewriting.MonadifiedFunctions

data CToken'
  = TID String
  | TOperator String
  | TReservedOp String
  | TCharLit Char
  | TStringLit String
  | TIntegerLit Integer
  | TFloatLit Double
  | TPlusPlus
  | TMinusMinus
  | TAmpr
  | TTimes
  | TPlus
  | TMinus
  | TTilde
  | TBang
  | TDiv
  | TPercent
  | TLTLT
  | TGTGT
  | TLT
  | TLTEQ
  | TGT
  | TGTEQ
  | TEQEQ
  | TBangEQ
  | THat
  | TBar
  | TAmprAmpr
  | TBarBar
  | TQuest
  | TColon
  | TEQ
  | TTimesEQ
  | TDivEQ
  | TPercentEQ
  | TPlusEQ
  | TMinusEQ
  | TLTLTEQ
  | TGTGTEQ
  | TAmprEQ
  | THatEQ
  | TBarEQ
  | TComma
  | TSemi
  | TDot
  | TArrow
  | TParen [CToken]
  | TBracket [CToken]
  | TBrace [CToken]
  | TLParen
  | TRParen
  | TLBracket
  | TRBracket
  | TLBrace
  | TRBrace
  | TSharp
  | TAuto
  | TBreak
  | TCase
  | TChar
  | TConst
  | TContinue
  | TDefault
  | TDo
  | TDouble
  | TElse
  | TEnum
  | TExtern
  | TFloat
  | TFor
  | TGoto
  | TIf
  | TInt
  | TLong
  | TRegister
  | TReturn
  | TShort
  | TSigned
  | TSizeof
  | TStatic
  | TStruct
  | TSwitch
  | TTypedef
  | TUnion
  | TUnsigned
  | TVoid
  | TVolatile
  | TWhile
  | TNil
  deriving (Show, Eq)

newtype SourcePos = SourcePos Int deriving (Show, Eq)

type CToken = (CToken', SourcePos)

tokenCount :: [CToken] -> Int
tokenCount = \xs ->
  if null xs
    then 0
    else
      if fst (head xs) == TNil
        then tokenCount (tail xs)
        else 1 + tokenCount (tail xs)
