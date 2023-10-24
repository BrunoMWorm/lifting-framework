{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

module Rewriting.Targets.Return.ReturnMemo where

import Data.Maybe ()
import Debug.Trace (trace)
import Language.C.Syntax.AST (CStatement (CBreak, CReturn))
import Memoization.Core.Memory (KeyValueArray, retrieveOrRun)
import Memoization.Core.State (State, evalState, execState, (<.>))
import Rewriting.Targets.CFG.CFG
  ( CFG,
    CFGNode (ast, _nID),
    _nodes,
    _succs,
  )
import Rewriting.Targets.CFG.NodeTypes
  ( NodeType (CFGDecl, CFGFuncRoot, CFGStat),
  )

find :: Int -> [Int] -> Bool
find n _ns =
  case _ns of
    [] -> False
    (h : _t) -> (h == n) || find n _t

findM :: State (KeyValueArray Int Bool) (Int -> State (KeyValueArray Int Bool) ([Int] -> State (KeyValueArray Int Bool) Bool))
findM =
  return
    ( \n ->
        return
          ( \_ns ->
              case _ns of
                [] -> return False
                (h : _t) -> orM <.> return (h == n) <.> (findM <.> return n <.> return _t)
          )
    )

isFnRoot :: CFGNode -> Bool
isFnRoot n =
  case ast n of
    CFGFuncRoot _ -> True
    _ -> False

isFnRootM :: State (KeyValueArray Int Bool) (CFGNode -> State (KeyValueArray Int Bool) Bool)
isFnRootM =
  return
    ( \n ->
        case ast n of
          CFGFuncRoot _ -> return True
          _ -> return False
    )

isReturn :: CFGNode -> Bool
isReturn n =
  case ast n of
    CFGStat (CReturn _ _) -> True
    _ -> False

isReturnM :: State (KeyValueArray Int Bool) (CFGNode -> State (KeyValueArray Int Bool) Bool)
isReturnM =
  return
    ( \n ->
        case ast n of
          CFGStat (CReturn _ _) -> return True
          _ -> return False
    )

isFuncCall :: CFGNode -> Bool
isFuncCall n =
  case ast n of
    CFGDecl _ -> True
    CFGStat (CBreak _) -> True
    CFGFuncRoot _ -> True
    _ -> False

isFuncCallM :: State (KeyValueArray Int Bool) (CFGNode -> State (KeyValueArray Int Bool) Bool)
isFuncCallM =
  return
    ( \n ->
        case ast n of
          CFGDecl _ -> return True
          CFGStat (CBreak _) -> return True
          CFGFuncRoot _ -> return True
          _ -> return False
    )

followSuccessor :: CFG -> [Int] -> CFGNode -> Bool
followSuccessor cfg _visited n
  | find (_nID n) _visited || isFuncCall n = False
  | isReturn n = True
  | otherwise = followSuccessors cfg (_nID n : _visited) n

followSuccessorM :: State (KeyValueArray Int Bool) (CFG -> State (KeyValueArray Int Bool) ([Int] -> State (KeyValueArray Int Bool) (CFGNode -> State (KeyValueArray Int Bool) Bool)))
followSuccessorM =
  return
    ( \cfg ->
        return
          ( \_visited ->
              return
                ( \n -> do
                    findResult <- findM <.> return (_nID n) <.> return _visited
                    if
                      | findResult || isFuncCall n -> return False
                      | isReturn n -> return True
                      | otherwise -> followSuccessorsM <.> return cfg <.> return (_nID n : _visited) <.> return n
                )
          )
    )

followSuccessors :: CFG -> [Int] -> CFGNode -> Bool
followSuccessors cfg _visited n =
  let _ss = _succs cfg n
   in foldr ((||) . followSuccessor cfg _visited) False _ss

followSuccessorsM :: State (KeyValueArray Int Bool) (CFG -> State (KeyValueArray Int Bool) ([Int] -> State (KeyValueArray Int Bool) (CFGNode -> State (KeyValueArray Int Bool) Bool)))
followSuccessorsM =
  return
    ( \cfg ->
        return
          ( \_visited ->
              return
                ( \n ->
                    let _ss = _succs cfg n
                     in foldrM
                          <.> return (\curr -> return (\acc -> orM <.> return acc <.> (followSuccessorM <.> return cfg <.> return _visited <.> return curr)))
                          <.> return False
                          <.> return _ss
                )
          )
    )

hasReturn :: CFG -> CFGNode -> Bool
hasReturn cfg n =
  followSuccessors cfg [_nID n] n

hasReturnM :: State (KeyValueArray Int Bool) (CFG -> State (KeyValueArray Int Bool) (CFGNode -> State (KeyValueArray Int Bool) Bool))
hasReturnM =
  return
    ( \cfg ->
        return
          ( \n ->
              -- A memoização aqui não funciona nos casos variacionais"
              -- Afinal, um ID de CFG aponta para um CFG cujas arestas dependem da condição de presença
              -- retrieveOrRun
              --   (_nID n)
              --   ( \_ ->
              followSuccessorsM <.> return cfg <.> return [_nID n] <.> return n
              -- )
          )
    )

analyze :: CFG -> [CFGNode]
analyze cfg =
  let _ns = _nodes cfg
      fns =
        filter isFnRoot _ns
   in filter (not . hasReturn cfg) fns

analyzeM :: State (KeyValueArray Int Bool) (CFG -> State (KeyValueArray Int Bool) [CFGNode])
analyzeM =
  return
    ( \cfg ->
        let _ns = _nodes cfg
            fns = filterM <.> isFnRootM <.> return _ns
         in filterM <.> return (\el -> notM <.> (hasReturnM <.> return cfg <.> return el)) <.> fns
    )

-- Auxiliary functions:

notM :: State (KeyValueArray Int Bool) (Bool -> State (KeyValueArray Int Bool) Bool)
notM = return (return . not)

orM :: State (KeyValueArray Int Bool) (Bool -> State (KeyValueArray Int Bool) (Bool -> State (KeyValueArray Int Bool) Bool))
orM = return (\a -> return (\b -> return (a || b)))

foldrM :: State (KeyValueArray Int Bool) ((a -> State (KeyValueArray Int Bool) (b -> State (KeyValueArray Int Bool) b)) -> State (KeyValueArray Int Bool) (b -> State (KeyValueArray Int Bool) ([a] -> State (KeyValueArray Int Bool) b)))
foldrM =
  return
    ( \f ->
        return
          ( \acc ->
              return
                ( \case
                    (x : xs) -> f x <.> (foldrM <.> return f <.> return acc <.> return xs)
                    [] -> return acc
                )
          )
    )

filterM :: State (KeyValueArray Int Bool) ((a -> State (KeyValueArray Int Bool) Bool) -> State (KeyValueArray Int Bool) ([a] -> State (KeyValueArray Int Bool) [a]))
filterM =
  return
    ( \f ->
        return
          ( \case
              (x : xs) -> do
                res <- f x
                if res then consM <.> return x <.> (filterM <.> return f <.> return xs) else filterM <.> return f <.> return xs
              [] -> return []
          )
    )

consM :: State (KeyValueArray Int Bool) (a -> State (KeyValueArray Int Bool) ([a] -> State (KeyValueArray Int Bool) [a]))
consM = return (\x1 -> return (\x2 -> return (x1 : x2)))
