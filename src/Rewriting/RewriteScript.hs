{-# LANGUAGE CPP #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Rewriting.RewriteScript where

import Data.Data (Data (toConstr))
import Data.List (intercalate)
import qualified Data.String
import Debug.Trace (trace)
import qualified GHC.Paths
import Retrie
  ( Annotated (astA),
    AnnotatedHsExpr,
    Context (ctxtBinders),
    GRHS (GRHS),
    GRHSs (grhssGRHSs),
    GenLocated (L),
    GhcPs,
    HoleVal (HoleExpr),
    HsExpr (HsApp, HsIf, HsLam, HsLit, HsOverLit, HsPar, HsVar, OpApp),
    HsLit,
    HsOverLit (ol_val),
    Match (m_grhss, m_pats),
    MatchGroup (MG),
    MatchResult (MatchResult, NoMatch),
    MatchResultTransformer,
    Outputable (ppr),
    RewriteSpec (Adhoc),
    apply,
    extendSubst,
    lookupSubst,
    parseExpr,
    parseRewrites,
    runScript,
    setRewriteTransformer,
  )
import Retrie.ExactPrint (exactPrint)
import Rewriting.TermMapping
  ( TermMapping,
    initialMapping,
    lookupTerm,
  )
import Text.Read (Lexeme (String))

-- The entrypoint rule matches all expressions in the target module. All expressions
-- are rewritten into their corresponding monadified versions.
-- In the `MatchResultTransformer` is where we define exactly how to create their corresponding monadified
-- versions.
main :: IO ()
main = runScript GHC.Paths.libdir $ \opts -> do
  [monadificationTemplate] <- parseRewrites GHC.Paths.libdir opts [Adhoc "forall expr. expr = monadifiedExpr"]
  [memoizationTemplate] <- parseRewrites GHC.Paths.libdir opts [Adhoc "forall expr. expr = memoizedExpr"]
  let monadification = setRewriteTransformer exprMonadifier monadificationTemplate
  let memoization = setRewriteTransformer exprMemoizer memoizationTemplate
  return $ apply [memoization]

-- Let's start hardcoding the name of the target. We can parametrize this later on.
memoizationTargetName :: String
memoizationTargetName = "fibRec"

-- Also hardcoding the name of the params on which we use memoization.
-- We can later explore more this bit as to allow the construction of a key from the params.
memoizationParamNames :: [String]
memoizationParamNames = ["n"]

exprMemoizer :: MatchResultTransformer
exprMemoizer ctxt match@(MatchResult substitution template) = case lookupSubst "expr" substitution of
  Just (HoleExpr expr) ->
    let (L _ hsExpr) = astA expr
        ctxtNames = ctxtBinders ctxt
        -- Name of the function being rewritten
        ctxtNameString = head $ map (show . ppr) ctxtNames
     in if ctxtNameString == memoizationTargetName
          then case constructRawMemoizedExpression expr of
            Just rawMemoizedExpr -> do
              parsedMemoizedExpr <- parseExpr GHC.Paths.libdir rawMemoizedExpr
              return $ MatchResult (extendSubst substitution "memoizedExpr" (HoleExpr parsedMemoizedExpr)) template
            Nothing -> do
              print $ " !! Expression " <> exactPrint hsExpr <> " is not the memoization target."
              return NoMatch
          else return NoMatch
  Nothing -> do
    print " !! HoleExpr not found. Verify if the rewrite rule in the main function has the correct name of the variable in the 'forall' quantifier."
    return NoMatch

constructRawMemoizedExpression :: AnnotatedHsExpr -> Maybe String
constructRawMemoizedExpression expr = let (L _ hsExpr) = astA expr in memoizationAlgorithm hsExpr

memoizationAlgorithm :: HsExpr GhcPs -> Maybe String
memoizationAlgorithm hsExpr = case hsExpr of
  -- If it is a lambda, we continue from the body
  (HsLam _ (MG _ (L _ [L _ l]) _)) -> Nothing
  -- (HsLam _ (MG _ (L _ [L _ l]) _)) -> let (L _ (GRHS _ _ body@(L _ bodyExpr))) = head (grhssGRHSs (m_grhss l)) in memoizationAlgorithm bodyExpr
  -- If it is an application and the name of the function is just a "return", then it is the middle product of the monadification. We just continue as well
  app@(HsApp _ func@(L _ f) args@(L _ a)) ->
    if exactPrint f == "return"
      then Nothing
      else Just $ finishMemoization app
  -- Otherwise, we have reached our stop condition
  var@(HsVar _ (L _ name)) -> if show (ppr name) == "return" then Nothing else Just $ finishMemoization var
  stopCondition -> Just $ finishMemoization stopCondition

-- constructRawMemoizedExpression expr = let params = collectLambdaParams expr in Just $ "retrieveOrRun" <> " n " <> wrapIntoParenthesis ("\\_ -> " <>  show (ppr (astA expr)))
-- -- If it is a lambda, add to the param list and continue with the body
-- lambda@(HsLam {}) -> Nothing
-- -- Stop condition
-- _ -> Nothing

finishMemoization :: HsExpr GhcPs -> String
finishMemoization hsExpr = "retrieveOrRun " <> unwords memoizationParamNames <> " " <> wrapIntoParenthesis ("\\_ -> " <> show (ppr hsExpr))

exprMonadifier :: MatchResultTransformer
exprMonadifier ctxt match@(MatchResult substitution template) = case lookupSubst "expr" substitution of
  Just (HoleExpr expr) -> case constructRawMonadifiedExpression ctxt expr of
    Just rawMonadifiedExpr -> do
      parsedMonadifiedExpr <- parseExpr GHC.Paths.libdir rawMonadifiedExpr
      return $ MatchResult (extendSubst substitution "monadifiedExpr" (HoleExpr parsedMonadifiedExpr)) template
    Nothing -> do
      print " !! Failed to construct the monadified version of the current expression."
      return NoMatch
  Nothing -> do
    print " !! HoleExpr not found. Verify if the rewrite rule in the main function has the correct name of the variable in the 'forall' quantifier."
    return NoMatch

constructRawMonadifiedExpression :: Context -> AnnotatedHsExpr -> Maybe String
constructRawMonadifiedExpression ctxt expr =
  let (L _ hsExpr) = astA expr
      -- Here we insert the name of the function being rewritten into the initial mapping.
      -- It is important for the cases where recursion is involved.
      ctxtNames = ctxtBinders ctxt
      ctxtTerms = map (\name -> (show (ppr name), show (ppr name))) ctxtNames
      mapping = initialMapping ++ ctxtTerms
   in trace (" # Starting monadification for expression: " <> exactPrint hsExpr) $ monadificationAlgorithm hsExpr mapping

monadificationAlgorithm :: HsExpr GhcPs -> TermMapping -> Maybe String
monadificationAlgorithm expr termMapping = case expr of
  lit@(HsLit _ l) -> trace (" ## Monadifying literal: " <> exactPrint lit) $ monadifyLiteral l
  olit@(HsOverLit _ l) -> trace (" ## Monadifying literal: " <> exactPrint olit) $ monadifyOverloadedLiteral l
  var@(HsVar _ v) -> trace (" ## Monadifying variable: " <> exactPrint var) monadifyVariable var termMapping
  parExpr@(HsPar _ (L _ p)) -> trace (" ## Monadifying parenthesis expression: " <> exactPrint p) monadificationAlgorithm p termMapping
  app@(HsApp {}) -> trace (" ## Monadifying application: " <> exactPrint app) monadifyApp app termMapping
  lambda@(HsLam {}) -> trace (" ## Monadifying lambda: " <> exactPrint lambda) monadifyLambda lambda termMapping
  opApp@(OpApp _ op a b) -> trace (" ## Monadifying infix operator application: " <> exactPrint opApp) $ monadifyOpApp opApp termMapping
  ifExpr@(HsIf _ cond thenExpr elseExpr) -> trace (" ## Monadifying if-then-else expression: " <> exactPrint ifExpr) monadifyIfExpr expr termMapping
  unknownExpr -> trace (" ## Monadifying something (?): " <> show (toConstr unknownExpr)) Nothing

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

monadifyIfExpr :: HsExpr GhcPs -> TermMapping -> Maybe String
monadifyIfExpr ifExpr@(HsIf _ (L _ condExpr) (L _ thenExpr) (L _ elseExpr)) mapping = do
  condExprM <- monadificationAlgorithm condExpr mapping
  thenExprM <- monadificationAlgorithm thenExpr mapping
  elseExprM <- monadificationAlgorithm elseExpr mapping
  let result = wrapIntoParenthesis ("ifM" <> " " <> condExprM <> " " <> thenExprM <> " " <> elseExprM)
   in trace (" ### If-then-else monadification result: " <> result) $
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
  Just $ wrapIntoParenthesis (fM <> " <.> " <> aM)

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
