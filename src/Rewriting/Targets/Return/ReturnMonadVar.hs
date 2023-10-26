{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Rewriting.Targets.Return.ReturnMonadVar where

import Control.Monad hiding (filterM)
import Control.Monad.Trans.Class
import qualified Data.Multimap as M
import Language.C.Syntax.AST
import Memoization.Core.Memory
import Memoization.Core.State
import Rewriting.Targets.CFG.NodeTypes
import Rewriting.Targets.CFG.VCFG
import Variability.VarLib
import Variability.VarTransformer

type VarState = VarT (State (KeyValueArray [Int] Int))

findM :: VarState (Int -> VarState ([VarState Int] -> VarState Bool))
findM =
  return
    ( \n ->
        return
          ( \case
              [] -> return False
              (h : _t) -> do
                h' <- h
                orM <#> return (h' == n) <#> (findM <#> return n <#> return _t)
          )
    )

isFnRootM :: VarState (CFGNode -> VarState Bool)
isFnRootM =
  return
    ( \n ->
        case ast n of
          CFGFuncRoot _ -> return True
          _ -> return False
    )

isReturnM :: VarState (CFGNode -> VarState Bool)
isReturnM =
  return
    ( \n ->
        case ast n of
          CFGStat (CReturn _ _) -> return True
          _ -> return False
    )

isFuncCallM :: VarState (CFGNode -> VarState Bool)
isFuncCallM =
  return
    ( \n ->
        case ast n of
          CFGDecl _ -> return True
          CFGStat (CBreak _) -> return True
          CFGFuncRoot _ -> return True
          _ -> return False
    )

followSuccessorM :: VarState (CFG -> VarState ([VarState Int] -> VarState (CFGNode -> VarState Bool)))
followSuccessorM =
  return
    ( \cfg ->
        return
          ( \_visited ->
              return
                ( \n -> do
                    findResult <- findM <#> return (_nID n) <#> return _visited
                    isFuncCallResult <- isFuncCallM <#> return n
                    isReturnResult <- isReturnM <#> return n
                    if
                      | findResult || isFuncCallResult -> return False
                      | isReturnResult -> return True
                      | otherwise -> followSuccessorsM <#> return cfg <#> return (fmap (VarT . return) _nID' (return n) : _visited) <#> return n
                )
          )
    )

followSuccessorsM :: VarState (CFG -> VarState ([VarState Int] -> VarState (CFGNode -> VarState Bool)))
followSuccessorsM =
  return
    ( \cfg ->
        return
          ( \_visited ->
              return
                ( \n ->
                    let _ss = _succs' (return cfg) (return n)
                     in foldrM
                          <#> return (\curr -> return (\acc -> orM <#> return acc <#> (followSuccessorM <#> return cfg <#> return _visited <#> return curr)))
                          <#> return False
                          <#> return (fmap (VarT . return) _ss)
                )
          )
    )

hasReturnM :: VarState (CFG -> VarState (CFGNode -> VarState Bool))
hasReturnM =
  return
    ( \cfg ->
        return
          ( \n ->
              -- A memoização aqui não funciona nos casos VarStateiacionais"
              -- Afinal, um ID de CFG aponta para um CFG cujas arestas dependem da condição de presença
              -- retrieveOrRun
              --   (_nID n)
              --   ( \_ ->
              followSuccessorsM <#> return cfg <#> return [(VarT . return) $ _nID' (return n)] <#> return n
              -- )
          )
    )

analyzeM :: VarState (CFG -> VarState [VarState CFGNode])
analyzeM =
  return
    ( \cfg ->
        let _ns = _nodes' (return cfg)
            fns = filterM <#> isFnRootM <#> return (fmap (VarT . return) _ns)
            bimbows = filterM <#> return (\el -> notM <#> (hasReturnM <#> return cfg <#> return el)) <#> fns
         in bimbows
    )

-- Auxiliary functions:

notM :: VarState (Bool -> VarState Bool)
notM = return (return . not)

orM :: VarState (Bool -> VarState (Bool -> VarState Bool))
orM = return (\a -> return (\b -> return (a || b)))

foldrM :: VarState ((a -> VarState (b -> VarState b)) -> VarState (b -> VarState ([VarState a] -> VarState b)))
foldrM =
  return
    ( \f ->
        return
          ( \acc ->
              return
                ( \case
                    (x : xs) -> do
                      x' <- x
                      f x' <#> (foldrM <#> return f <#> return acc <#> return xs)
                    [] -> return acc
                )
          )
    )

filterM :: VarState ((a -> VarState Bool) -> VarState ([VarState a] -> VarState [VarState a]))
filterM =
  return
    ( \f ->
        return
          ( \case
              (x : xs) -> do
                x' <- x
                res <- f x'
                if res then consM <#> x <#> (filterM <#> return f <#> return xs) else filterM <#> return f <#> return xs
              [] -> return []
          )
    )

consM :: VarState (a -> VarState ([VarState a] -> VarState [VarState a]))
consM = return (\x1 -> return (\x2 -> return (return x1 : x2)))

(<#>) :: VarState (a -> VarState b) -> VarState a -> VarState b
fm <#> xm = do
  f <- fm
  x <- xm
  f x