{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Collapse lambdas" #-}
{-# HLINT ignore "Eta reduce" #-}

module Rewriting.TargetModule where

import Memoization.Core.State

-- -- plusApp :: Num a => a -> a -> a
-- plusApp a b = (+) a b

-- -- plusInfix :: Num n => n -> n -> n
-- plusInfix a b = a + b

-- -- someString :: String
-- someString = "foobar"

-- -- plusLambda :: Integer -> Integer -> Integer
-- plusLambda = \a -> \b -> (+) a b

-- plusLambdaM ::
--   State
--     m
--     ( Integer ->
--       State m (Integer -> State m Integer)
--     )
-- plusLambdaM = return (\a -> (return (\b -> (((return (\a -> return (\b -> return (a + b)))) <.> (return (a))) <.> (return (b))))))

-- someStringM :: State m String
-- someStringM = return "foobar"

-- plusInfixM ::
--   State
--     m
--     ( Integer ->
--       State m (Integer -> State m Integer)
--     )
-- plusInfixM = return (\a -> return (\b -> ((return (\a -> return (\b -> return (a + b)))) <.> (return (a)) <.> (return (b)))))

-- plusAppM ::
--   State
--     m
--     ( Integer ->
--       State m (Integer -> State m Integer)
--     )
-- plusAppM = return (\a -> return (\b -> ((return (\a -> return (\b -> return (a + b)))) <.> (return (a))) <.> (return (b))))

-- Esta função foi extraída para não termos que lidar ainda com a regra lambda de reescrita, que
-- -- é um pouco chatinha...
-- applyFToRange :: (Num a, Enum a) => (a -> b) -> a -> [b]
-- applyFToRange f n = map f [0 .. n]

-- fib :: Int -> Int
-- fib n = 1 + sum (applyFToRange fib (n - 2))