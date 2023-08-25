module Rewriting.TermMapping where

import Data.Maybe (listToMaybe)

-- First try: lets store only String-based monadified expressions in our
-- term mapping.
-- We only parse this raw expressions when doing the actual substitution in the `MatchResultTransformer`.
type RawMonadifiedExpression = String

type Term = String

type TermMapping = [(Term, RawMonadifiedExpression)]

-- Mapping containing the monadified version of core functions and operators
initialMapping :: TermMapping
initialMapping =
  [ ("+", "return (\\a -> return (\\b -> return (a + b)))"),
    ("-", "return (\\a -> return (\\b -> return (a - b)))"),
    ("*", "return (\\a -> return (\\b -> return (a - b)))"),
    ("/", "return (\\a -> return (\\b -> return (a - b)))"),
    ("sum", "return (\\a -> return (sum a))"),
    ("map", "return ( \\f -> return ( \\case\n" ++
            "  [] -> return []\n" ++
            "  (x : xs) -> return (\\x1 -> return (\\x2 -> return (x1 : x2))) <.> f x <.> (cmapM <.> return f <.> return xs)\n" ++
            "  )" ++
            ")")
  ]

lookupTerm :: Term -> TermMapping -> Maybe RawMonadifiedExpression
lookupTerm term mapping = listToMaybe $ map snd (filter ((term ==) . fst) mapping)

updateMapping :: Term -> RawMonadifiedExpression -> TermMapping -> TermMapping
updateMapping term expr mapping = (term, expr) : filter ((term /=) . fst) mapping
