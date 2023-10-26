{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module RunAnalysis where

import Control.DeepSeq
import Control.Monad (join)
import qualified Data.Multimap.List
import qualified Data.Set as S
import GHC.Generics
import Memoization.Core.Memory
import Memoization.Core.State
import Rewriting.Targets.CFG.CFG
import Rewriting.Targets.CFG.CFGParser
import Rewriting.Targets.CFG.NodeTypes
import qualified Rewriting.Targets.CFG.VCFG as V
import Rewriting.Targets.Return.Return
import Rewriting.Targets.Return.ReturnMemo (analyzeM)
import qualified Rewriting.Targets.Return.ReturnMonadVar as Deep
import Variability.VarLib
import Variability.VarTransformer

analysis :: String
analysis = "Return"

getFunctionNodes :: [CFGNode] -> [CFGNode]
getFunctionNodes =
  filter
    ( \case
        (CFGNode _ _ _ (CFGFunc _) _ _) -> True
        _ -> False
    )

getFunctionNodes' :: Var [CFGNode] -> Var [CFGNode]
getFunctionNodes' = fmap getFunctionNodes

bruteforce :: (Var CFG, [String]) -> Var [CFGNode]
bruteforce (ns, features) =
  let configs = getAllConfigs features
      inVecs' = zip (map (index ns) configs) configs
      inVecs = filter (\(i, _) -> not (null i)) inVecs'
   in mkVars $
        map
          ( \(input, pc) ->
              (analyze (head input), pc)
          )
          inVecs

shallow :: Var CFG -> Var [CFGNode]
shallow = fmap analyze

applyMapMAndZip :: (a -> State m b) -> State m (Var a -> State m (Var b))
applyMapMAndZip f =
  return
    ( \va -> do
        let fV = Var [(f, truePC)]
            Var vals = apply fV va
            pcs = map snd vals
         in (\ls -> Var (zip ls pcs)) <$> mapM fst vals
    )

shallowLift :: State m (a -> State m b) -> State m (Var a -> State m (Var b))
shallowLift fM = do
  f <- fM
  applyMapMAndZip f

shallowM ::
  State
    (KeyValueArray Int Bool)
    (Var CFG -> State (KeyValueArray Int Bool) (Var [CFGNode]))
shallowM = shallowLift analyzeM

deep :: Deep.VarState (V.CFG -> Deep.VarState [Deep.VarState V.CFGNode])
deep = Deep.analyzeM

nodes' :: Var CFG -> Var (Data.Multimap.List.ListMultimap Int CFGNode)
nodes' = fmap nodes

getFeatures :: IO [String]
getFeatures = do
  vs <- getVars truePC
  return $ map fst vs

data Env = Env
  { deepCFG :: Var V.CFG,
    shallowCFG :: Var CFG,
    fileName :: String,
    features :: [String],
    configs :: Int,
    nodeCount :: Int,
    hdr :: String
  }
  deriving (Generic, NFData)

getAllConfigs :: [String] -> [PresenceCondition]
getAllConfigs [] = []
getAllConfigs (f : fs) =
  let p = mkPCVar f
      n = notPC p
      fs' = getAllConfigs fs
   in if null fs
        then [p, n]
        else map (/\ p) fs' ++ map (/\ n) fs'

setupEnv :: String -> IO Env
setupEnv filename = do
  cfg <- readCFG filename
  let deep = cfg ^| truePC
  -- This bang-pattern bellow is important to correctly initialize the 'features' value.
  -- TODO: find the underlying issue. It is possible to be related to some initialization condition on the PC hash tables.
  let !shallow@(Var sh') = V.toShallowCFG cfg
  let presentConfigs = length sh'
  let nodes = V._nodes cfg
  let nodeCount = nodes `seq` length nodes
  features <- cfg `seq` getFeatures
  let featCount = deep `seq` shallow `seq` length features
  let configCount = length (getAllConfigs features)
  let hdr =
        foldr
          (\s t -> s ++ "," ++ t)
          ""
          [filename, show nodeCount, show featCount, show configCount, show presentConfigs]
  let env = Env deep shallow filename features configCount nodeCount hdr
  putStrLn $ "Analysis:        " ++ analysis
  putStrLn $ "File:            " ++ filename
  putStrLn $ "Node#:           " ++ show nodeCount
  putStrLn $ "Features:        " ++ show features
  putStrLn $ "Feature#:        " ++ show featCount
  putStrLn $ "Config#:         " ++ show configCount
  putStrLn $ "Present config#: " ++ show presentConfigs
  return env

main :: IO ()
main = do
  env <- setupEnv "foo.cfg"
  print "Bruteforce:"
  print $ bruteforce (shallowCFG env, features env)
  print "##############"
  print "Shallow:"
  print $ shallow (shallowCFG env)
  print "ShallowM:"
  print $ runState (shallowM <.> return (shallowCFG env)) []
  print "##############"
  print "Deep:"
  let initialState = []
      deepComp = deep Deep.<#> (VarT . return) (deepCFG env)
      deepCompInner = fmap (fmap runVarT) deepComp
      deepCompOuter = fmap sequence deepCompInner
      runVarTRes = runVarT deepCompOuter
      runSTRes = runState runVarTRes initialState
      traverseResult = uncurry traverseVarSt runSTRes
  print $ fst traverseResult
  print $ snd traverseResult

traverseVarSt :: Var (State m a) -> m -> (m, Var a)
traverseVarSt (Var []) st = (st, Var [])
traverseVarSt (Var ((s, pc) : ls)) st =
  let (execStateRes, currSt) = runState s st
      (newSt, tailRes) = traverseVarSt (mkVars ls) currSt
   in (newSt, mkVar execStateRes pc `union` tailRes)