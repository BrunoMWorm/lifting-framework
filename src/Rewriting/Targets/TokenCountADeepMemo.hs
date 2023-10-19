{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLInt ignore "Redundant lambda" #-}
{-# HLInt ignore "Avoid lambda" #-}
{-# HLInt ignore "Use <$>" #-}
{-# HLINT ignore "Use section" #-}
module Rewriting.Targets.TokenCountADeepMemo where

import Memoization.Core.Memory (KeyValueArray)
import Memoization.Core.State (State, (<.>))
import Rewriting.Rules.MonadifiedFunctions
  ( monadifyBinaryNumOperator,
  )
import qualified Rewriting.Targets.TokenCountAMemo as M
import Variability.VarTypes
  ( PresenceCondition,
    VClass (comb, restrict),
    Var,
    evalCond,
    ffPC,
    head',
    liftedCond,
    mkVarT,
    tail',
    (/^),
    (^|),
  )

type ConcMem = (KeyValueArray [CToken] Int)

type CToken = Int

-- TokenCount DeepLifted quebrado em módulos com binds 'let'
tokenCountA :: [Var CToken] -> Var Int
tokenCountA = \xs ->
  let cond1V :: Var Bool
      cond1V = mkVarT (null xs)

      then1V :: PresenceCondition -> Var Int
      then1V = \__cntxt__ -> 0 ^| __cntxt__

      else1V :: PresenceCondition -> Var Int
      else1V = \__cntxt__ ->
        let cond2V :: Var Bool
            cond2V = ((==) ^| __cntxt__) <*> head' (map (restrict __cntxt__) xs) <*> (0 ^| __cntxt__)

            then2V :: PresenceCondition -> Var Int
            then2V = \__cntxt__ -> tokenCountA (tail' (map (restrict __cntxt__) xs))

            else2V :: PresenceCondition -> Var Int
            else2V = \__cntxt__ -> ((+) ^| __cntxt__) <*> (1 ^| __cntxt__) <*> tokenCountA (tail' (map (restrict __cntxt__) xs))
         in liftedCond cond2V then2V else2V
   in liftedCond cond1V then1V else1V

liftedCond' :: (VClass a) => Var Bool -> a b -> a b -> a b
liftedCond' c x y =
  let (t, f) = evalCond c
   in if t == ffPC
        then y
        else
          if f == ffPC
            then x
            else comb (x /^ t) (y /^ f)

liftedCondM' :: (VClass t) => State ConcMem (Var Bool) -> State ConcMem (t b) -> State ConcMem (t b) -> State ConcMem (t b)
liftedCondM' cM xM yM = do
  c <- cM
  x <- xM
  y <- yM
  return (liftedCond' c x y)

tokenCountAVM :: State ConcMem ([Var CToken] -> State ConcMem (Var Int))
tokenCountAVM =
  return
    ( \xs ->
        let cond1V :: State ConcMem (Var Bool)
            cond1V = return (mkVarT (null xs))

            then1V :: State ConcMem (Var Int)
            then1V = return (mkVarT 0)

            else1V :: State ConcMem (Var Int)
            else1V =
              let cond2V :: State ConcMem (Var Bool)
                  cond2V = return (mkVarT (==) <*> head' xs <*> mkVarT 0)

                  then2V :: State ConcMem (Var Int)
                  then2V = tokenCountAVM <@> return (tail' xs)

                  else2V :: State ConcMem (Var Int)
                  else2V = return (\a -> return (mkVarT ((+) 1) <*> a)) <@> (tokenCountAVM <@> return (tail' xs))
               in liftedCondM' cond2V then2V else2V
         in liftedCondM' cond1V then1V else1V
    )

-- What do we have for functions?
-- A Monadified and Memoized computation (State m (a -> State m b)) on some monadified value (State m b)
-- A deep-lifted function (Var (a -> b)) applied on some variational argument (Var b)
-- And how do we apply them
-- Monadified version: <.> :: State m (a -> State m b) -> State m a -> State m b
-- Lifted 'apply' (defined as <*>) operator: <*> :: Var (a -> b) -> Var a -> Var b

-- How do we merge those?
-- Great question...

(<@>) :: State m (a -> State m b) -> State m a -> State m b
(<@>) fM xM = do
  f <- fM
  x <- xM
  f x

-- (<#>) :: State m (Var a -> State m (Var b)) 
--     -> State m (a -> State m b) 
--     -> State m (Var a) 
--     -> State m (Var b)
-- (<#>) fMV fM xMV = do
--   fV <- fMV
--   xV <- xMV
--   fV xV