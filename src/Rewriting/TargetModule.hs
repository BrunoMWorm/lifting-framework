{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}
module Rewriting.TargetModule where

import Memoization.Core.Memory
import Memoization.Core.State
import Rewriting.MonadifiedFunctions


fibRec = \ n -> if n < 2 then 1 else fibRec (n - 1) + fibRec (n - 2)

-- fibRec :: State (KeyValueArray Int Int) (Int -> State (KeyValueArray Int Int) Int)
-- fibRec = return (\n -> (ifM (monadifyBinaryOrdOperator (<) <.> (return (n)) <.> (return 2)) (return 1) (monadifyBinaryNumOperator (+) <.> (fibRec <.> (monadifyBinaryNumOperator (-) <.> (return (n)) <.> (return 1))) <.> (fibRec <.> (monadifyBinaryNumOperator (-) <.> (return (n)) <.> (return 2))))))

-- fibRec :: State (KeyValueArray Int Int) (Int -> State (KeyValueArray Int Int) Int)
-- fibRec = return (\n -> retrieveOrRun n (\_ -> (ifM (monadifyBinaryOrdOperator (<) <.> (return (n)) <.> (return 2)) (return 1) (monadifyBinaryNumOperator (+) <.> (fibRec <.> (monadifyBinaryNumOperator (-) <.> (return (n)) <.> (return 1))) <.> (fibRec <.> (monadifyBinaryNumOperator (-) <.> (return (n)) <.> (return 2)))))))
