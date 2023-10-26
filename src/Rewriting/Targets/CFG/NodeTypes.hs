{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Rewriting.Targets.CFG.NodeTypes where

import Control.DeepSeq
import qualified Data.Text as T
import GHC.Generics
import Language.C.Syntax.AST

data NodeType
  = CFGExpr CExpr
  | CFGStat CStat
  | CFGVarDecl CExtDecl
  | CFGDecl T.Text
  | CFGFunc T.Text
  | CFGFuncRoot T.Text
  | CFGDummy T.Text
  deriving (Show, Generic, NFData)