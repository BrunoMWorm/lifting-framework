{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Rewriting.Targets.TokenCount.TokenCountMonadMemo where

import Memoization.Core.Memory
import Memoization.Core.State
import Rewriting.Rules.MonadifiedFunctions

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