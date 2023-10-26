{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Rewriting.Targets.Return.ReturnMonadVar where

import Control.Monad (join)
import Language.C.Syntax.AST
import Rewriting.Targets.CFG.NodeTypes
import Rewriting.Targets.CFG.VCFG
import Variability.VarLib

findM :: Var (Int -> Var ([Var Int] -> Var Bool))
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

isFnRootM :: Var (CFGNode -> Var Bool)
isFnRootM =
  return
    ( \n ->
        case ast n of
          CFGFuncRoot _ -> return True
          _ -> return False
    )

isReturnM :: Var (CFGNode -> Var Bool)
isReturnM =
  return
    ( \n ->
        case ast n of
          CFGStat (CReturn _ _) -> return True
          _ -> return False
    )

isFuncCallM :: Var (CFGNode -> Var Bool)
isFuncCallM =
  return
    ( \n ->
        case ast n of
          CFGDecl _ -> return True
          CFGStat (CBreak _) -> return True
          CFGFuncRoot _ -> return True
          _ -> return False
    )

followSuccessorM :: Var (CFG -> Var ([Var Int] -> Var (CFGNode -> Var Bool)))
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
                      | otherwise -> followSuccessorsM <#> return cfg <#> return (_nID' (return n) : _visited) <#> return n
                )
          )
    )

followSuccessorsM :: Var (CFG -> Var ([Var Int] -> Var (CFGNode -> Var Bool)))
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
                          <#> return _ss
                )
          )
    )

hasReturnM :: Var (CFG -> Var (CFGNode -> Var Bool))
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
              followSuccessorsM <#> return cfg <#> return [_nID' (return n)] <#> return n
              -- )
          )
    )

analyzeM :: Var (CFG -> Var [Var CFGNode])
analyzeM =
  return
    ( \cfg ->
        let _ns = _nodes' (return cfg)
            fns = filterM <#> isFnRootM <#> return _ns
            bimbows = filterM <#> return (\el -> notM <#> (hasReturnM <#> return cfg <#> return el)) <#> fns
         in bimbows
    )

-- Auxiliary functions:

notM :: Var (Bool -> Var Bool)
notM = return (return . not)

orM :: Var (Bool -> Var (Bool -> Var Bool))
orM = return (\a -> return (\b -> return (a || b)))

foldrM :: Var ((a -> Var (b -> Var b)) -> Var (b -> Var ([Var a] -> Var b)))
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

filterM :: Var ((a -> Var Bool) -> Var ([Var a] -> Var [Var a]))
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

consM :: Var (a -> Var ([Var a] -> Var [Var a]))
consM = return (\x1 -> return (\x2 -> return (return x1 : x2)))

(<#>) :: Var (a -> Var b) -> Var a -> Var b
fm <#> xm = do
  f <- fm
  x <- xm
  f x