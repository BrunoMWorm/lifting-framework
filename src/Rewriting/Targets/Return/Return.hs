module Rewriting.Targets.Return.Return where

import Language.C.Syntax.AST
import Rewriting.Targets.CFG.CFG
import Rewriting.Targets.CFG.NodeTypes

find :: Int -> [Int] -> Bool
find n _ns =
  case _ns of
    [] -> False
    (h : _t) -> h == n || find n _t

isFnRoot :: CFGNode -> Bool
isFnRoot n =
  case ast n of
    CFGFuncRoot _ -> True
    _ -> False

isReturn :: CFGNode -> Bool
isReturn n =
  case ast n of
    CFGStat (CReturn _ _) -> True
    _ -> False

isFuncCall :: CFGNode -> Bool
isFuncCall n =
  case ast n of
    CFGDecl _ -> True
    CFGStat (CBreak _) -> True
    CFGFuncRoot _ -> True
    _ -> False

followSuccessor :: CFG -> [Int] -> CFGNode -> Bool
followSuccessor cfg _visited n =
  not (find (_nID n) _visited || isFuncCall n)
    && ( isReturn n || followSuccessors cfg (_nID n : _visited) n
       )

followSuccessors :: CFG -> [Int] -> CFGNode -> Bool
followSuccessors cfg _visited n =
  let _ss = _succs cfg n
   in foldr ((||) . followSuccessor cfg _visited) False _ss

hasReturn :: CFG -> CFGNode -> Bool
hasReturn cfg n =
  followSuccessors cfg [_nID n] n

analyze :: CFG -> [CFGNode]
analyze cfg =
  let _ns = _nodes cfg
      fns =
        filter isFnRoot _ns
   in filter (not . hasReturn cfg) fns
