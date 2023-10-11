{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use guards" #-}
module Rewriting.Targets.TokenCountAMemo where

import Memoization.Core.Memory
import Memoization.Core.State
import Rewriting.Rules.MonadifiedFunctions
import Variability.VarTypes

tokenCountA :: State (KeyValueArray [Int] Int) ([Int] -> State (KeyValueArray [Int] Int) Int)
tokenCountA =
  return
    ( \xs ->
        retrieveOrRun
          xs
          ( \_ ->
              ifM
                (return (\t -> return (null t)) <.> (return (xs)))
                (return 0)
                ( ifM
                    ( monadifyBinaryEqOperator (==)
                        <.> (return (\t -> return (head t)) <.> (return (xs)))
                        <.> (return 0)
                    )
                    ( tokenCountA
                        <.> (return (\t -> return (tail t)) <.> (return (xs)))
                    )
                    ( monadifyBinaryNumOperator (+)
                        <.> (return 1)
                        <.> ( tokenCountA
                                <.> (return (\t -> return (tail t)) <.> (return (xs)))
                            )
                    )
                )
          )
    )
