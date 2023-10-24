{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}
module Rewriting.Targets.TokenCountADeep where

import Variability.Legacy.VarTypes (Var, fst', head', liftedCond, mkVarT, null', restrict, tail', (/^), (^|))

tokenCountA :: [Var Int] -> Var Int
tokenCountA = \xs -> liftedCond (mkVarT (null xs)) (\__cntxt__ -> (0 ^| __cntxt__)) (\__cntxt__ -> liftedCond (((==) ^| __cntxt__) <*> (head' (map (restrict __cntxt__) xs)) <*> (0 ^| __cntxt__)) (\__cntxt__ -> tokenCountA (tail' (map (restrict __cntxt__) xs))) (\__cntxt__ -> ((+) ^| __cntxt__) <*> ((1 ^| __cntxt__)) <*> (tokenCountA (tail' (map (restrict __cntxt__) xs)))))
