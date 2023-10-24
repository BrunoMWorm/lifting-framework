{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Rewriting.Targets.CFG.VCFG where

import Control.DeepSeq (NFData (..))
import Control.Exception (assert)
import qualified Data.Multimap as M
import qualified Data.Text as T
import Debug.Trace ()
import GHC.Generics (Generic)
import Language.C.Syntax.AST ()
import qualified Rewriting.Targets.CFG.CFG as C
import Rewriting.Targets.CFG.NodeTypes (NodeType (CFGDummy))
import Variability.VarLib
  ( PresenceCondition,
    Var (..),
    fixCompleteness,
    mkVars,
    notPC,
    truePC,
    union,
    (^|),
  )

lv2vl :: [Var a] -> Var [a]
lv2vl = foldr (foo (:)) (pure [])

foo :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
foo f a b = fmap f a <*> b

data CFGNode where
  CFGNode ::
    { _nID :: Int,
      _fname :: T.Text,
      text :: T.Text,
      ast :: NodeType,
      _preds :: [Var Int],
      __succs :: [Var Int]
    } ->
    CFGNode
  deriving (Show, Generic, NFData)

dummyCNode :: C.CFGNode
dummyCNode = C.CFGNode 0 (T.pack "") (T.pack "") (CFGDummy (T.pack "")) [] []

toShallowNode :: (CFGNode, PresenceCondition) -> Var C.CFGNode
toShallowNode (n, pc) =
  let ps = lv2vl $ _preds n
      ss = lv2vl $ __succs n
      d =
        (C.CFGNode ^| pc)
          <*> (_nID n ^| pc)
          <*> (_fname n ^| pc)
          <*> (text n ^| pc)
          <*> (ast n ^| pc)
          <*> ps
          <*> ss
   in fixCompleteness dummyCNode d

data CFG = CFG
  { nodes :: M.ListMultimap Int (CFGNode, PresenceCondition)
  }

instance NFData CFG where
  rnf n = n `seq` (M.toList . nodes) n `seq` ()

_nodes :: CFG -> [(CFGNode, PresenceCondition)]
_nodes cfg = (map snd . M.toList) $ nodes cfg

mkShallowCFG :: [C.CFGNode] -> C.CFG
mkShallowCFG ns = C.CFG $! foldr (\n m -> M.append (C._nID n) n m) M.empty ns

mkShallowCFG' :: Var [C.CFGNode] -> Var C.CFG
mkShallowCFG' ns@(Var ns') =
  fmap mkShallowCFG ns

toShallowCFG :: CFG -> Var C.CFG
toShallowCFG c =
  let !ns = _nodes c
      !ns' = map toShallowNode ns
      !vl@(Var vl') = lv2vl ns'
      !ret = mkShallowCFG' vl
   in ret

_succs' :: Var CFG -> Var CFGNode -> [Var CFGNode]
_succs' (Var ((cfg, pc) : ss)) n'@(Var n) =
  assert (null ss) $
    let ss' = concatMap (\(n', _) -> if _nID n' == 0 then [] else __succs n') n
        zs =
          map
            ( \(Var xs) ->
                Var $
                  map
                    ( \(n, pc) ->
                        let ys =
                              nodes cfg M.! n
                         in assert (not (null ys)) $ head ys
                    )
                    (filter (\(n, pc) -> n /= 0) xs)
            )
            ss'
     in map (fixCompleteness dummyNode) zs
_succs' (Var []) _ = []

dummyNode :: CFGNode
dummyNode = CFGNode 0 T.empty T.empty (CFGDummy T.empty) [] []

mkV :: a -> (a, PresenceCondition) -> Var a
mkV dummy (v, pc) =
  if pc == truePC
    then pure v
    else mkVars [(v, pc), (dummy, notPC pc)]

_nodes' :: Var CFG -> [Var CFGNode]
_nodes' (Var ((cfg, pc) : ss)) =
  assert (null ss) $
    assert (pc == truePC) $
      let ns = (map snd . M.toList) $ nodes cfg
       in map (mkV dummyNode) ns

_nID' :: Var CFGNode -> Var Int
_nID' (Var n) = foldr (union . (\(n', pc) -> _nID n' ^| pc)) (Var []) n
