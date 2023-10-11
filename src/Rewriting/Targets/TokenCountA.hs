{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use guards" #-}
module Rewriting.Targets.TokenCountA where

tokenCountA :: [Int] -> Int
tokenCountA = \xs ->
  if null xs
    then 0
    else
      if head xs == 0
        then tokenCountA (tail xs)
        else 1 + tokenCountA (tail xs)