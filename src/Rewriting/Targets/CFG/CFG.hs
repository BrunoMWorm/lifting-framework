{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module Rewriting.Targets.CFG.CFG where

import Control.DeepSeq
import qualified Data.List as L
import qualified Data.Multimap as M
import qualified Data.Text as T
import GHC.Generics
import Rewriting.Targets.CFG.NodeTypes

data CFGNode = CFGNode
  { _nID :: Int,
    _fname :: T.Text,
    text :: T.Text,
    ast :: NodeType,
    _preds :: [Int],
    __succs :: [Int]
  }
  deriving (Generic, NFData)

newtype CFG = CFG
  { nodes :: M.ListMultimap Int CFGNode
  }

instance NFData CFG where
  rnf :: CFG -> ()
  rnf n = _nodes n `seq` n `seq` ()

instance Show CFG where
  show :: CFG -> String
  show cfg = show $ _nodes cfg

_find :: Int -> [Int] -> Bool
_find n _ns =
  case _ns of
    [] -> False
    (h : _t) -> h == n || _find n _t

findNode :: CFG -> Int -> [CFGNode]
findNode cfg id =
  let ns = nodes cfg M.! id
   in ns

_succs :: CFG -> CFGNode -> [CFGNode]
_succs cfg n =
  let rs = map (findNode cfg) (__succs n)
      rs' = filter (not . null) rs
   in map head rs'

_nodes :: CFG -> [CFGNode]
_nodes cfg = map snd . M.toList $ nodes cfg

preds :: CFG -> CFGNode -> [CFGNode]
preds cfg n = map (head . (nodes cfg M.!)) (_preds n)

instance Show CFGNode where
  show :: CFGNode -> String
  show (CFGNode i fname t nt ps ss) =
    "Node: "
      ++ show i
      ++ "\t"
      ++ show t
      ++ "\t"
      ++ show fname
      ++ "\n\tAST: "
      ++ show nt
      ++ "\n\tpredecessors: "
      ++ L.intercalate ", " (map show ps)
      ++ "\n\tsuccessors  : "
      ++ L.intercalate ", " (map show ss)
      ++ "\n"
