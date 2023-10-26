{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Rewriting.Targets.TokenCount.TokenCountMonadVar where

import Rewriting.Rules.MonadifiedFunctions
import Variability.VarLib

tokenCount :: (Monad m) => m ([m Int] -> m Int)
tokenCount =
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