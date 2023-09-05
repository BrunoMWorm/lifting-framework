{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
module Rewriting.TargetModule where

import Memoization.Core.Memory
import Memoization.Core.State
import Rewriting.MonadifiedFunctions

fibRec = \n -> if n < 2 then 1 else fibRec (n - 1) + fibRec (n - 2)
