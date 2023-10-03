module Rewriting.MonadifiedFunctions where

-- Monadified function definitions that can be injected into the rewritten program

-- Monadified if-then-else transformation
ifM :: Monad m => m Bool -> m b -> m b -> m b
ifM condExprM thenExprM elseExprM = do
  condExpr <- condExprM
  if condExpr then thenExprM else elseExprM

-- Used for (+), (-), (*)
monadifyBinaryNumOperator :: (Monad m, Num a) => (a -> a -> a) -> m (a -> m (a -> m a))
monadifyBinaryNumOperator op = return (\a -> return (\b -> return (a `op` b)))

-- Used for (/)
monadifyBinaryFractionalOperator :: (Monad m, Fractional a) => (a -> a -> a) -> m (a -> m (a -> m a))
monadifyBinaryFractionalOperator op = return (\a -> return (\b -> return (a `op` b)))

-- Used for (<), (<=), (>), (>=)
monadifyBinaryOrdOperator :: (Monad m, Ord a) => (a -> a -> Bool) -> m (a -> m (a -> m Bool))
monadifyBinaryOrdOperator op = return (\a -> return (\b -> return (a `op` b)))
