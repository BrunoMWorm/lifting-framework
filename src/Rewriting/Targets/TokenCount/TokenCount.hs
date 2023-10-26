{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}
module Rewriting.Targets.TokenCount.TokenCount where

tokenCount :: [Int] -> Int
tokenCount = \xs ->
  if null xs
    then 0
    else
      if head xs == 0
        then tokenCount (tail xs)
        else 1 + tokenCount (tail xs)