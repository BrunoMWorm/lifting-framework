module Rewriting.Rules.TermMapping where

import Data.Maybe

-- First try: lets store only String-based monadified expressions in our
-- term mapping.
-- We only parse this raw expressions when doing the actual substitution in the `MatchResultTransformer`.
type RawMonadifiedExpression = String

type Term = String

type TermMapping = [(Term, RawMonadifiedExpression)]

-- Mapping containing the monadified version of core functions and operators
initialMapping :: TermMapping
initialMapping =
  [ ("+", "monadifyBinaryNumOperator (+)"),
    ("-", "monadifyBinaryNumOperator (-)"),
    ("*", "monadifyBinaryNumOperator (*)"),
    ("/", "monadifyBinaryFractionalOperator (/)"),
    ("<=", "monadifyBinaryOrdOperator (<=)"),
    ("<", "monadifyBinaryOrdOperator (<)"),
    (">=", "monadifyBinaryOrdOperator (>=)"),
    (">", "monadifyBinaryOrdOperator (>)"),
    ("==", "monadifyBinaryEqOperator (==)"),
    ("/=", "monadifyBinaryEqOperator (/=)"),
    -- We can also add some ad-hoc functions here if convenient
    ("sum", "return (\\a -> return (sum a))"),
    ("fst", "return (\\t -> return (fst t))"),
    ("snd", "return (\\t -> return (snd t))"),
    ("null", "return (\\t -> return (null t))"),
    ("head", "return (\\t -> return (head t))"),
    ("tail", "return (\\t -> return (tail t))")
  ]

lookupTerm :: Term -> TermMapping -> Maybe RawMonadifiedExpression
lookupTerm term mapping = listToMaybe $ map snd (filter ((term ==) . fst) mapping)

updateMapping :: Term -> RawMonadifiedExpression -> TermMapping -> TermMapping
updateMapping term expr mapping = (term, expr) : filter ((term /=) . fst) mapping
