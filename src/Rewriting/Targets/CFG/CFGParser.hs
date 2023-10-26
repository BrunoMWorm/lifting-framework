{-# LANGUAGE OverloadedStrings #-}

module Rewriting.Targets.CFG.CFGParser where

import Control.Exception
import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Data.Multimap as MM
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Debug.Trace
import Language.C.Data.InputStream
import Language.C.Data.Position
import Language.C.Parser
import Language.C.Syntax.AST
import Rewriting.Targets.CFG.NodeTypes
import Rewriting.Targets.CFG.VCFG
import Variability.Parse
import Variability.VarLib

processEdge :: T.Text -> ((Int, Int), PresenceCondition)
processEdge t =
  let [e, from', to', pc'] = T.splitOn ";" t
      from = read (T.unpack from') :: Int
      to = read (T.unpack to') :: Int
      pc = parsePC (T.unpack pc')
   in assert (e == "E") ((from, to), pc)

processNode :: [((Int, Int), PresenceCondition)] -> T.Text -> (CFGNode, PresenceCondition)
processNode edges record =
  let (n : id' : t : lineNum' : rest) = T.splitOn ";" record
      id = read (T.unpack id') :: Int
      lineNum = read (T.unpack lineNum') :: Int
      cCode = T.intercalate ";" $ L.init rest
      pc = parsePC $ T.unpack (L.last rest)
      (ast, pc', fname) = parseNode cCode lineNum t
      preds = map (\((f, _), pc) -> mkV 0 (f, pc)) $ filter (\((_, t), _) -> t == id) edges
      succs = map (\((_, t), pc) -> mkV 0 (t, pc)) $ filter (\((f, _), _) -> f == id) edges
      node = CFGNode id fname cCode ast preds succs
   in assert (n == "N") $
        assert (length rest >= 2) (node, pc /\ pc')

readCFG :: String -> IO CFG
readCFG inputFileName = do
  fileTxt <- TIO.readFile inputFileName
  let lines' = T.lines fileTxt
  let lines = L.nub lines'
  let (nodeRecs, edgeRecs) = L.partition (\t -> T.head t == 'N') lines
  let edges = map processEdge edgeRecs
  let nodes = map (processNode edges) nodeRecs
  return $ mkCFG nodes

mkCFG :: [(CFGNode, PresenceCondition)] -> CFG
mkCFG ns = CFG $ foldr (\n'@(n, pc) m -> MM.append (_nID n) n' m) MM.empty ns

parseDeclaration :: Int -> String -> NodeType
parseDeclaration lineNum s =
  let input = inputStreamFromString s
      p = execParser_ extDeclP input (position 0 "" lineNum 0 Nothing)
   in case p of
        Left e ->
          CFGDummy (T.pack s)
        Right e -> CFGVarDecl e

parseStatement :: Int -> String -> NodeType
parseStatement lineNum s =
  let input = inputStreamFromString (s ++ ";")
      p = execParser_ statementP input (position 0 "" lineNum 0 Nothing)
   in case p of
        Left e ->
          parseExpr lineNum s
        Right e -> CFGStat e

parseExpr :: Int -> String -> NodeType
parseExpr lineNum s =
  let input = inputStreamFromString s
      p = execParser_ expressionP input (position 0 "" lineNum 0 Nothing)
   in case p of
        Left e -> parseDeclaration lineNum (s ++ ";")
        Right e -> CFGExpr e

extractPC :: T.Text -> (T.Text, PresenceCondition)
extractPC t =
  let i = findMatchingParen 1 0 t
      (pc, t') = T.splitAt i t
   in (t', parsePC (T.unpack (T.strip pc)))
  where
    findMatchingParen i c t =
      case T.head t of
        '(' -> findMatchingParen (i + 1) (c + 1) $ T.tail t
        ')' -> if c == 1 then i else findMatchingParen (i + 1) (c - 1) $ T.tail t
        _ -> findMatchingParen (i + 1) c $ T.tail t

splitPCs :: T.Text -> [(T.Text, PresenceCondition)]
splitPCs t =
  let (c0, rest) = T.breakOn "#if" t
      (c1, rest') = T.breakOn "#endif" (T.drop 3 rest)
   in if T.null rest then [(c0, truePC)] else (c0, truePC) : extractPC c1 : splitPCs (T.drop 6 rest')

parseNode :: T.Text -> Int -> T.Text -> (NodeType, PresenceCondition, T.Text)
parseNode t lineNum nodeType =
  let (nodeText, pc, fname) = fixCFragment t
      ast = case T.unpack nodeType of
        "statement" -> parseStatement lineNum (T.unpack nodeText)
        "expression" -> parseExpr lineNum (T.unpack nodeText)
        "declaration" -> CFGDecl $ T.strip nodeText
        "function" -> CFGFuncRoot $ T.strip nodeText
        "function-inline" -> CFGFunc $ T.strip nodeText
        "function-static" -> CFGFuncRoot $ T.strip nodeText
        _ -> trace ("Unknown node type: " ++ T.unpack nodeType) $ CFGDummy nodeText
   in (ast, pc, fname)
  where
    fixCFragment s =
      let (t0', t0'') = T.breakOnEnd "::" s
          t0 = if T.null t0'' then t0' else T.dropEnd 2 t0'
          ps = splitPCs t0
          (cs, pcs) = unzip ps
          pc = foldr (/\) truePC pcs
       in (T.concat cs, pc, t0'')
