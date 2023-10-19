{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use guards" #-}
{-# HLINT ignore "Avoid lambda" #-}
module Rewriting.Targets.TokenCountAMemo where

import Control.Monad (join)
import Rewriting.Rules.MonadifiedFunctions
  ( ifM,
    monadifyBinaryEqOperator,
    monadifyBinaryNumOperator,
  )
import Variability.VarLib (PresenceCondition, Var (Var), mkPCVar, notPC, (/\), mkVar)

tokenCountA :: (Monad m) => m ([m Int] -> m Int)
tokenCountA =
  return
    ( \xs ->
        ifM
          (return (return . null) <@> return xs)
          (return 0)
          ( ifM
              ( monadifyBinaryEqOperator (==)
                  <@> (return (return . head) <@> sequence xs)
                  <@> return 0
              )
              ( tokenCountA
                  <@> (return (return . tail) <@> return xs)
              )
              ( monadifyBinaryNumOperator (+)
                  <@> return 1
                  <@> ( tokenCountA
                          <@> (return (return . tail) <@> return xs)
                      )
              )
          )
    )

(<@>) :: (Monad m) => m (a -> m b) -> m a -> m b
fm <@> xm = fm >>= (xm >>=)

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

varInput :: [Var Int]
varInput =
  [ Var [(1, atbt), (1, afbf), (1, atbf), (0, afbt)],
    Var [(0, atbt), (1, afbf), (1, atbf), (0, afbt)],
    Var [(0, atbt), (0, afbf), (1, atbf), (0, afbt)]
  ]