{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use guards" #-}
{-# HLINT ignore "Avoid lambda" #-}
module Rewriting.Targets.TokenCountMonadBoth where

import Control.Monad.Trans.Class ( MonadTrans(lift) )
import Control.Monad.Trans.State ( StateT )
import Memoization.Core.Memory (KeyValueArray)
import Rewriting.Rules.MonadifiedFunctions
  ( ifM,
    monadifyBinaryEqOperator,
    monadifyBinaryNumOperator,
  )
import Variability.VarLib (PresenceCondition, Var (Var), mkPCVar, mkVar, notPC, (/\))

tokenCountA :: (Monad d) => StateT (KeyValueArray [Int] Int) d ([StateT (KeyValueArray [Int] Int) d Int] -> StateT (KeyValueArray [Int] Int) d Int)
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

type StateVar a = StateT (KeyValueArray [Int] Int) Var a

varInput :: [StateVar Int]
varInput =
  [ lift $ Var [(1, atbt), (1, afbf), (1, atbf), (0, afbt)],
    lift $ Var [(0, atbt), (1, afbf), (1, atbf), (0, afbt)],
    lift $ Var [(0, atbt), (0, afbf), (1, atbf), (0, afbt)]
  ]

res :: StateT (KeyValueArray [Int] Int) Var Int
res = tokenCountA <@> return varInput

-- Interesting insights
-- Sequence is a lazy transformation for turning [Var Int] into Var [Int]

-- Also very interesting...
-- It seems that I could use StateT to transform a monad into a State Monad

-- https://stackoverflow.com/questions/43438875/confusion-about-statet-state-and-monadstate

-- State is for your normal state monad. This is the simplest of the three. (In some older tutorials, you may see use of the State constructor, but this has been replaced with the state function, because State s is now a type alias for StateT s Identity.)

-- StateT is the monad transformer for the State monad. It adds a layer of generality by allowing you to put an arbitrary monad inside the state. This is useful for simple parsers, which could use e.g. StateT [Token] Maybe Result to represent the parsing as a stateful operation that could fail.

-- MonadState generalizes the situation even farther. There is an instance Monad m => MonadState s (StateT s m), but there are also instances, like one that allows you to perform stateful operations on monad transformers of StateT. All basic state functions (get, set, modify, etc.) can be used with an instance of MonadState.
