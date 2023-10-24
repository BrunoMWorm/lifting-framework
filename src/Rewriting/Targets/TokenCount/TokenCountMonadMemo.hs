{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use guards" #-}
{-# HLINT ignore "Avoid lambda" #-}
module Rewriting.Targets.TokenCount.TokenCountMonadMemo where

import Control.Monad (join)
import Memoization.Core.Memory (KeyMemory, KeyValueArray, retrieveOrRun)
import Memoization.Core.State (State (runState))
import Rewriting.Rules.MonadifiedFunctions
  ( ifM,
    monadifyBinaryEqOperator,
    monadifyBinaryNumOperator,
  )
import Variability.VarLib (PresenceCondition, Var (Var), mkPCVar, mkVar, notPC, (/\))

tokenCount :: State (KeyValueArray [Int] Int) ([Int] -> State (KeyValueArray [Int] Int) Int)
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
              ( retrieveOrRun
                  xs
                  ( \_ ->
                      tokenCount
                        <@> (return (return . tail) <@> return xs)
                  )
              )
              ( monadifyBinaryNumOperator (+)
                  <@> return 1
                  <@> retrieveOrRun
                    xs
                    ( \_ ->
                        tokenCount
                          <@> (return (return . tail) <@> return xs)
                    )
              )
          )
    )

(<@>) :: State m (a -> State m b) -> State m a -> State m b
fm <@> xm = fm >>= (xm >>=)

input :: [Int]
input = [1, 2, 3, 4]

result :: (Int, KeyValueArray [Int] Int)
result = runState (tokenCount <@> return input) []