{-# LANGUAGE CPP #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Rewriting.RewriteScript where

import Debug.Trace
import qualified GHC.Paths
import Retrie
import Retrie.ExactPrint
import Rewriting.TermMapping

-- The entrypoint rule matches all expressions in the target module. All expressions
-- are rewritten into their corresponding monadified versions.

-- In the `MatchResultTransformer` is where we define exactly how to create their corresponding monadified
-- versions.
main :: IO ()
main = runScript GHC.Paths.libdir $ \opts -> do
  [rewrite] <- parseRewrites GHC.Paths.libdir opts [Adhoc "forall expr. expr = monadifiedExpr"]
  return $ apply [setRewriteTransformer exprMonadifier rewrite]

exprMonadifier :: MatchResultTransformer
exprMonadifier _ match@(MatchResult substitution template) = case lookupSubst "expr" substitution of
  Just (HoleExpr expr) -> case constructRawMonadifiedExpression expr of
    Just rawMonadifiedExpr -> do
      parsedMonadifiedExpr <- parseExpr GHC.Paths.libdir rawMonadifiedExpr
      return $ MatchResult (extendSubst substitution "monadifiedExpr" (HoleExpr parsedMonadifiedExpr)) template
    Nothing -> do
      print "Failed to construct the monadified version of the current expression."
      return NoMatch
  Nothing -> do
    print "HoleExpr not found. Verify if the rewrite rule in the main function has the correct name of the variable in the 'forall' quantifier."
    return NoMatch

-- TODO: insert the name of the function being rewritten into the initial mapping.
-- It is going to be important for them cases where recursion is involved.
constructRawMonadifiedExpression :: AnnotatedHsExpr -> Maybe String
constructRawMonadifiedExpression expr =
  let (L _ hsExpr) = astA expr
   in trace (" # Starting monadification for expression: " <> exactPrint hsExpr) $ monadificationAlgorithm hsExpr initialMapping

monadificationAlgorithm :: HsExpr GhcPs -> TermMapping -> Maybe String
monadificationAlgorithm expr termMapping = case expr of
  lit@(HsLit _ l) -> trace (" ## Monadifying literal: " <> exactPrint lit) $ monadifyLiteral l
  var@(HsVar _ v) -> trace (" ## Monadifying variable: " <> exactPrint var) monadifyVariable var termMapping
  app@(HsApp {}) -> trace (" ## Monadifying application: " <> exactPrint app) monadifyApp app termMapping
  lambda@(HsLam {}) -> trace (" ## Monadifying lambda: " <> exactPrint lambda) monadifyLambda lambda termMapping
  opApp@(OpApp _ op a b) -> trace (" ## Monadifying literal: " <> exactPrint opApp) $ monadifyOpApp opApp termMapping
  x -> trace (" ## Monadifying something: " <> exactPrint x) Nothing

monadifyOpApp :: HsExpr GhcPs -> TermMapping -> Maybe String
monadifyOpApp (OpApp _ (L _ leftExpr) (L _ op) (L _ rightExpr)) mapping = do
  leftExprM <- monadificationAlgorithm leftExpr mapping
  monadifiedOp <- lookupMonadifiedOperator op mapping
  rightExprM <- monadificationAlgorithm rightExpr mapping
  Just $ wrapIntoParenthesis (monadifiedOp <> " <.> " <> leftExprM <> " <.> " <> rightExprM)

lookupMonadifiedOperator :: HsExpr GhcPs -> TermMapping -> Maybe String
lookupMonadifiedOperator (HsVar _ (L _ opName)) =
  trace (" ## LookupMoniadifiedOperator: " <> show (ppr opName)) $
    lookupTerm (show (ppr opName))

monadifyVariable :: HsExpr GhcPs -> TermMapping -> Maybe String
monadifyVariable (HsVar _ (L _ name)) mapping =
  let result = wrapIntoReturn ("(" <> show (ppr name) <> ")")
   in trace (" ### Variable monadification result: " <> result) $
        Just result

monadifyLambda :: HsExpr GhcPs -> TermMapping -> Maybe String
monadifyLambda _ _ = Nothing

monadifyApp :: HsExpr GhcPs -> TermMapping -> Maybe String
monadifyApp (HsApp _ (L _ f) (L _ a)) mapping = do
  fM <- monadificationAlgorithm f mapping
  aM <- monadificationAlgorithm a mapping
  Just $ wrapIntoParenthesis (fM <> "<.>" <> aM)

monadifyLiteral :: HsLit a -> Maybe String
monadifyLiteral (HsChar _ c) = Just $ wrapIntoReturn (show c)
monadifyLiteral (HsCharPrim _ c) = Just $ wrapIntoReturn (show c)
monadifyLiteral (HsString _ s) = Just $ wrapIntoReturn (show s)
monadifyLiteral (HsStringPrim _ s) = Just $ wrapIntoReturn (show s)
monadifyLiteral (HsInt _ i) = Just $ wrapIntoReturn (show i)
monadifyLiteral (HsIntPrim _ i) = Just $ wrapIntoReturn (show i)
monadifyLiteral (HsInt64Prim _ i) = Just $ wrapIntoReturn (show i)
monadifyLiteral (HsWordPrim _ i) = Just $ wrapIntoReturn (show i)
monadifyLiteral (HsWord64Prim _ i) = Just $ wrapIntoReturn (show i)
-- TODO: Check the third argument for HsInteger
monadifyLiteral (HsInteger _ i _) = Just $ wrapIntoReturn (show i)
-- TODO: Check the third argument for HsRat
monadifyLiteral (HsRat _ r _) = Just $ wrapIntoReturn (show r)
monadifyLiteral (HsFloatPrim _ f) = Just $ wrapIntoReturn (show f)
monadifyLiteral (HsDoublePrim _ d) = Just $ wrapIntoReturn (show d)
monadifyLiteral _ = Nothing

wrapIntoReturn :: String -> String
wrapIntoReturn str = wrapIntoParenthesis ("return " <> str)

wrapIntoParenthesis :: String -> String
wrapIntoParenthesis str = "(" <> str <> ")"