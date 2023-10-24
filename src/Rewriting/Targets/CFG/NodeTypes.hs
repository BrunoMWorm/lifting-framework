{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Rewriting.Targets.CFG.NodeTypes where

import Control.DeepSeq (NFData)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Language.C.Syntax.AST (CExpr, CExtDecl, CStat)

data NodeType
  = CFGExpr CExpr
  | CFGStat CStat
  | CFGVarDecl CExtDecl
  | CFGDecl T.Text
  | CFGFunc T.Text
  | CFGFuncRoot T.Text
  | CFGDummy T.Text
  deriving (Show, Generic, NFData)