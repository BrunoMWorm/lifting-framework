{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use guards" #-}
{-# HLINT ignore "Avoid lambda" #-}
module Rewriting.Targets.TokenCount.TokenCountMonadBoth where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.State (StateT)
import Memoization.Core.Memory (KeyValueArray)
import Memoization.Core.State (State, runState)
import Rewriting.Rules.MonadifiedFunctions
  ( ifM,
    monadifyBinaryEqOperator,
    monadifyBinaryNumOperator,
  )
import Variability.VarLib (PresenceCondition, Var (Var), mkPCVar, mkVar, notPC, (/\))
import Variability.VarTransformer (VarT (VarT), runVarT)

tokenCount :: (Monad d) => d ([Int] -> d Int)
tokenCount =
  return
    ( \xs ->
        ifM
          (return (return . null) <@> return xs)
          (return 0)
          ( ifM
              ( monadifyBinaryEqOperator (==)
                  <@> (return (return . head) <@> return xs)
                  <@> return 0
              )
              ( tokenCount
                  <@> (return (return . tail) <@> return xs)
              )
              ( monadifyBinaryNumOperator (+)
                  <@> return 1
                  <@> ( tokenCount
                          <@> (return (return . tail) <@> return xs)
                      )
              )
          )
    )

(<@>) :: (Monad m) => m (a -> m b) -> m a -> m b
fm <@> xm = fm >>= (xm >>=)

(<#>) :: (Monad m) => m (a -> m b) -> m a -> m b
fm <#> xm = do
  f <- fm
  x <- xm
  f x

propA :: PresenceCondition
propA = mkPCVar "A"

propB :: PresenceCondition
propB = mkPCVar "B"

atbt :: PresenceCondition
atbt = propA /\ propB

atbf :: PresenceCondition
atbf = propA /\ notPC propB

afbt :: PresenceCondition
afbt = notPC propA /\ propB

afbf :: PresenceCondition
afbf = notPC propA /\ notPC propB

type VarState = VarT (State (KeyValueArray [Int] Int))

varInput :: Var [Var Int]
varInput =
  pure
    [ Var [(1, atbt), (1, afbf), (1, atbf), (0, afbt)],
      Var [(0, atbt), (1, afbf), (1, atbf), (0, afbt)],
      Var [(0, atbt), (0, afbf), (1, atbf), (0, afbt)]
    ]
