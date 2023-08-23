{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Collapse lambdas" #-}
{-# HLINT ignore "Eta reduce" #-}

module Rewriting.TargetModule where

import Memoization.Core.State

-- plusM :: (Monad m, Num a) => m (a -> m (a -> m a))
-- plusM = return (\a -> return (\b -> return (a + b)))

-- plusApp :: Num a => a -> a -> a
-- plusApp a b = (+) a b

plusInfix :: Num n => n -> n -> n
plusInfix a b = a + b

-- plusInfixM ::
--   (Num n) =>
--   State m (n -> State m (n -> State m n))
-- plusInfixM = return (\x -> return (\y -> (return (\a -> return (\b -> return (a + b)))) <.> (return (x)) <.> (return (y))))

-- plusInfixM :: Num a => State m (a -> State m (a -> State m a))
-- plusInfixM = return (\a -> return (\b -> plusM <.> return a <.> return b))

-- someString :: String
-- someString = "foobar"

-- plusLambda = \a -> \b -> (+) a b
-- someInt = 123
-- someDouble = 1.2341
