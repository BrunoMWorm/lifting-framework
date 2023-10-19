module Rewriting.Targets.MonadifiedFib where

import Memoization.Core.Memory (KeyValueArray)
import Memoization.Core.State (State)
import Rewriting.Rules.MonadifiedFunctions (ifM, monadifyBinaryNumOperator, monadifyBinaryOrdOperator)
import Variability.VarLib (PresenceCondition, Var (Var), mkPCVar, notPC, (/\))

fibRec :: (Monad m) => m (Int -> m Int)
fibRec =
  return
    ( \n ->
        ifM
          (monadifyBinaryOrdOperator (<) <@> return n <@> return 2)
          (return 1)
          ( monadifyBinaryNumOperator (+)
              <@> ( fibRec
                      <@> (monadifyBinaryNumOperator (-) <@> return n <@> return 1)
                  )
              <@> ( fibRec
                      <@> (monadifyBinaryNumOperator (-) <@> return n <@> return 2)
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

varInput :: Var Int
varInput = Var [(8, atbt), (14, afbf), (1, atbf), (0, afbt)]

stInput :: State [KeyValueArray Int Int] Int
stInput = return 20
