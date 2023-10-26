{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Rewriting.Targets.Return.ReturnMonad where

import Language.C.Syntax.AST
import Rewriting.Targets.CFG.CFG
import Rewriting.Targets.CFG.NodeTypes

findM :: (Monad m) => m (Int -> m ([Int] -> m Bool))
findM =
  return
    ( \n ->
        return
          ( \case
              [] -> return False
              (h : _t) -> orM <#> return (h == n) <#> (findM <#> return n <#> return _t)
          )
    )

isFnRootM :: (Monad m) => m (CFGNode -> m Bool)
isFnRootM =
  return
    ( \n ->
        case ast n of
          CFGFuncRoot _ -> return True
          _ -> return False
    )

isReturnM :: (Monad m) => m (CFGNode -> m Bool)
isReturnM =
  return
    ( \n ->
        case ast n of
          CFGStat (CReturn _ _) -> return True
          _ -> return False
    )

isFuncCallM :: (Monad m) => m (CFGNode -> m Bool)
isFuncCallM =
  return
    ( \n ->
        case ast n of
          CFGDecl _ -> return True
          CFGStat (CBreak _) -> return True
          CFGFuncRoot _ -> return True
          _ -> return False
    )

followSuccessorM :: (Monad m) => m (CFG -> m ([Int] -> m (CFGNode -> m Bool)))
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
                      | otherwise -> followSuccessorsM <#> return cfg <#> return (_nID n : _visited) <#> return n
                )
          )
    )

followSuccessorsM :: (Monad m) => m (CFG -> m ([Int] -> m (CFGNode -> m Bool)))
followSuccessorsM =
  return
    ( \cfg ->
        return
          ( \_visited ->
              return
                ( \n ->
                    let _ss = _succs cfg n
                     in foldrM
                          <#> return (\curr -> return (\acc -> orM <#> return acc <#> (followSuccessorM <#> return cfg <#> return _visited <#> return curr)))
                          <#> return False
                          <#> return _ss
                )
          )
    )

hasReturnM :: (Monad m) => m (CFG -> m (CFGNode -> m Bool))
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
              followSuccessorsM <#> return cfg <#> return [_nID n] <#> return n
              -- )
          )
    )

analyzeM :: (Monad m) => m (CFG -> m [CFGNode])
analyzeM =
  return
    ( \cfg ->
        let _ns = _nodes cfg
            fns = filterM <#> isFnRootM <#> return _ns
         in filterM <#> return (\el -> notM <#> (hasReturnM <#> return cfg <#> return el)) <#> fns
    )

-- Auxiliary functions:

notM :: (Monad m) => m (Bool -> m Bool)
notM = return (return . not)

orM :: (Monad m) => m (Bool -> m (Bool -> m Bool))
orM = return (\a -> return (\b -> return (a || b)))

foldrM :: (Monad m) => m ((a -> m (b -> m b)) -> m (b -> m ([a] -> m b)))
foldrM =
  return
    ( \f ->
        return
          ( \acc ->
              return
                ( \case
                    (x : xs) -> f x <#> (foldrM <#> return f <#> return acc <#> return xs)
                    [] -> return acc
                )
          )
    )

filterM :: (Monad m) => m ((a -> m Bool) -> m ([a] -> m [a]))
filterM =
  return
    ( \f ->
        return
          ( \case
              (x : xs) -> do
                res <- f x
                if res then consM <#> return x <#> (filterM <#> return f <#> return xs) else filterM <#> return f <#> return xs
              [] -> return []
          )
    )

consM :: (Monad m) => m (a -> m ([a] -> m [a]))
consM = return (\x1 -> return (\x2 -> return (x1 : x2)))

(<#>) :: (Monad m) => m (a -> m b) -> m a -> m b
fm <#> xm = do
  f <- fm
  x <- xm
  f x