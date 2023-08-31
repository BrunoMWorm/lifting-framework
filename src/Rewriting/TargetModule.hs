{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Collapse lambdas" #-}
{-# HLINT ignore "Eta reduce" #-}

module Rewriting.TargetModule where

import Memoization.Core.State
import Rewriting.Inject (ifM)
-- import Main

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
-- applyFToRange f n = 

-- plus a b = a + b

-- plusM :: (Monad m) => m
--   (Integer
--    -> m
--         (Integer -> m Integer))
-- plusM = return (\a -> return (\b -> return (a + b)))

-- fibRec :: Integer -> Integer
fibRecN = \n -> if n < 2 then 1 else fibRecN n - 1 + (fibRecN n - 2)


fibRecM = return (\n -> (ifM (return (\a -> return (\b -> return (a < b))) <.> (return (n)) <.> (return 2)) (return 1) (return (\a -> return (\b -> return (a + b))) <.> (return (\a -> return (\b -> return (a - b))) <.> (fibRecM <.> (return (n))) <.> (return 1)) <.> (return (\a -> return (\b -> return (a - b))) <.> (fibRecM <.> (return (n))) <.> (return 2)))))-- fibRecN = return (\n -> (ifM (return (\a -> return (\b -> return (a < b))) <.> (return (n)) <.> (return 2)) (return 1) (return (\a -> return (\b -> return (a + b))) <.> (return (\a -> return (\b -> return (a - b))) <.> ((return (fibRecN)) <.> (return (n))) <.> (return 1)) <.> (return (\a -> return (\b -> return (a - b))) <.> ((return (fibRecN)) <.> (return (n))) <.> (return 2)))))

-- foo a b c = ifM (return (\a -> return (\b -> return (a > b))) <.> (return (a)) <.> (return (c))) (return (b)) (return (c))