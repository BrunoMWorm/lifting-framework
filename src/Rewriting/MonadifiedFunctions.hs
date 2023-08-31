module Rewriting.Inject where

-- Monadified function definitions that can be injected into the rewritten program 

ifM :: Monad m => m Bool -> m b -> m b -> m b
ifM condExprM thenExprM elseExprM = do
    condExpr <- condExprM
    if condExpr then thenExprM else elseExprM
