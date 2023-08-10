{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Collapse lambdas" #-}

module Rewriting.TargetModule where

plus = \a -> \b -> \c -> a + b + c

someInt = 123

someString = "foobar"

someDouble = 1.2341
