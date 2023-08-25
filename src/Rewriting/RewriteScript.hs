{-# LANGUAGE CPP #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Rewriting.RewriteScript where

import qualified Data.String
import Debug.Trace
import qualified GHC.Paths
import Retrie
import Retrie.ExactPrint
import Rewriting.TermMapping
import Text.Read (Lexeme (String))

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
      print " !! Failed to construct the monadified version of the current expression."
      return NoMatch
  Nothing -> do
    print " !! HoleExpr not found. Verify if the rewrite rule in the main function has the correct name of the variable in the 'forall' quantifier."
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
  olit@(HsOverLit _ l) -> trace (" ## Monadifying literal: " <> exactPrint olit) $ monadifyOverloadedLiteral l
  var@(HsVar _ v) -> trace (" ## Monadifying variable: " <> exactPrint var) monadifyVariable var termMapping
  app@(HsApp {}) -> trace (" ## Monadifying application: " <> exactPrint app) monadifyApp app termMapping
  lambda@(HsLam {}) -> trace (" ## Monadifying lambda: " <> exactPrint lambda) monadifyLambda lambda termMapping
  opApp@(OpApp _ op a b) -> trace (" ## Monadifying infix operator application: " <> exactPrint opApp) $ monadifyOpApp opApp termMapping
  x -> trace (" ## Monadifying something: " <> exactPrint x) Nothing

monadifyOpApp :: HsExpr GhcPs -> TermMapping -> Maybe String
monadifyOpApp (OpApp _ (L _ leftExpr) (L _ op) (L _ rightExpr)) mapping = do
  leftExprM <- monadificationAlgorithm leftExpr mapping
  monadifiedOp <- lookupVariable op mapping
  rightExprM <- monadificationAlgorithm rightExpr mapping
  Just $ wrapIntoParenthesis (monadifiedOp <> " <.> " <> leftExprM <> " <.> " <> rightExprM)

lookupVariable :: HsExpr GhcPs -> TermMapping -> Maybe String
lookupVariable (HsVar _ (L _ opName)) =
  trace (" ## Lookup Variable: " <> show (ppr opName)) $
    -- We remove the parenthesis for the cases where the variable being lookup is a operator. Example: (+)
    lookupTerm (removeParenthesis (show (ppr opName)))

monadifyVariable :: HsExpr GhcPs -> TermMapping -> Maybe String
monadifyVariable v@(HsVar _ (L _ name)) mapping = case lookupVariable v mapping of
  Just match -> Just match
  Nothing ->
    let result = wrapIntoReturn ("(" <> show (ppr name) <> ")")
     in trace (" ### Variable monadification result: " <> result) $
          Just result

monadifyLambda :: HsExpr GhcPs -> TermMapping -> Maybe String
monadifyLambda (HsLam _ (MG _ (L _ [L _ l]) _)) mapping =
  let (L _ (GRHS _ _ body@(L _ bodyExpr))) = head (grhssGRHSs (m_grhss l))
      param = head (m_pats l)
      paramStr = show (ppr param)
   in trace
        (" ### Inside lambda monadification: " <> show (ppr body))
        $ do
          monadifiedBody <- monadificationAlgorithm bodyExpr mapping
          Just $ wrapIntoReturn (wrapIntoParenthesis ("\\" <> paramStr <> " -> " <> monadifiedBody))

monadifyApp :: HsExpr GhcPs -> TermMapping -> Maybe String
monadifyApp (HsApp _ (L _ f) (L _ a)) mapping = do
  fM <- monadificationAlgorithm f mapping
  aM <- monadificationAlgorithm a mapping
  Just $ wrapIntoParenthesis (fM <> "<.>" <> aM)

monadifyOverloadedLiteral :: HsOverLit GhcPs -> Maybe String
monadifyOverloadedLiteral olit = Just $ wrapIntoReturn (show (ppr (ol_val olit)))

monadifyLiteral :: HsLit GhcPs -> Maybe String
monadifyLiteral lit = Just $ wrapIntoReturn (show (ppr lit))

wrapIntoReturn :: String -> String
wrapIntoReturn str = wrapIntoParenthesis ("return " <> str)

wrapIntoParenthesis :: String -> String
wrapIntoParenthesis str = "(" <> str <> ")"

removeParenthesis :: String -> String
removeParenthesis = filter (\c -> c `notElem` ['(', ')'])
