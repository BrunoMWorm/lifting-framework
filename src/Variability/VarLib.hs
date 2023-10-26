{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module Variability.VarLib where

import Control.DeepSeq (NFData (rnf))
import Control.Exception (assert)
import Cudd.Cudd (DDManager, DDNode, bAnd, bNot, bOr, cuddInit, ithVar, nodeReadIndex, readLogicZero, readOne)
import qualified Data.HashTable.IO as H
import Data.Hashable (Hashable (hashWithSalt))
import qualified Data.List as L
import GHC.Generics (Generic)
import GHC.IO (unsafePerformIO)
import GHC.StableName (makeStableName)
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Text
import Text.Parsec.Token

-- Context structures for storing DD values with their respective names
pc2string :: HashTable DDNode String
{-# NOINLINE pc2string #-}
pc2string = unsafePerformIO H.new

var2index :: HashTable String Int
{-# NOINLINE var2index #-}
var2index = unsafePerformIO H.new

getVars :: PresenceCondition -> IO [(String, Int)]
getVars _ = do
  H.toList var2index

lookupPC :: DDNode -> IO (Maybe String)
lookupPC = H.lookup pc2string

lookupVar :: String -> Int
lookupVar v = unsafePerformIO $ do
  i <- H.lookup var2index v
  case i of
    Nothing -> do
      i' <- htSize var2index
      d0 <- H.insert var2index v i'
      return i'
    Just i' -> return i'

-- Presence Condition Typeclass, functions and instances
newtype PresenceCondition = PresenceCondition
  { b :: DDNode
  }

instance Show PresenceCondition where
  show :: PresenceCondition -> String
  show (PresenceCondition b) =
    case unsafePerformIO $ lookupPC b of
      Nothing -> show b
      Just b' -> b'

type HashTable k v = H.BasicHashTable k v

instance Hashable DDNode where
  hashWithSalt :: Int -> DDNode -> Int
  hashWithSalt s d = hashWithSalt s (nodeReadIndex d)

instance Eq PresenceCondition where
  (==) :: PresenceCondition -> PresenceCondition -> Bool
  (PresenceCondition b0) == (PresenceCondition b1) = b0 == b1

instance Ord PresenceCondition where
  (<=) :: PresenceCondition -> PresenceCondition -> Bool
  (PresenceCondition b0) <= (PresenceCondition b1) = nodeReadIndex b0 <= nodeReadIndex b1

manager :: Cudd.Cudd.DDManager
manager = cuddInit

truePC :: PresenceCondition
truePC = newPC (readOne manager) "TRUE"

falsePC :: PresenceCondition
falsePC = newPC (readLogicZero manager) "FALSE"

lookupBDD :: DDNode -> IO (Maybe String)
lookupBDD = H.lookup pc2string

lookupBDD' :: DDNode -> String
lookupBDD' b = unsafePerformIO $ do
  s <- lookupBDD b
  case s of
    Nothing -> return (show b)
    Just x -> return x

newPC :: DDNode -> String -> PresenceCondition
newPC b s = unsafePerformIO $ do
  x <- lookupPC b
  case x of
    Nothing -> H.insert pc2string b s >> return (PresenceCondition b)
    Just x' -> return $ PresenceCondition b

mkPCVar :: String -> PresenceCondition
mkPCVar name =
  let i = lookupVar name
      r = ithVar manager i
   in newPC r name

andPC :: PresenceCondition -> PresenceCondition -> PresenceCondition
andPC p0@(PresenceCondition b0) p1@(PresenceCondition b1)
  | p0 == falsePC = falsePC
  | p1 == falsePC = falsePC
  | p0 == truePC = p1
  | p1 == truePC = p0
  | otherwise =
      let b = bAnd manager b0 b1
          sb0 = lookupBDD' b0
          sb1 = lookupBDD' b1
       in newPC b ("(" ++ sb0 ++ " /\\ " ++ sb1 ++ ")")

orPC :: PresenceCondition -> PresenceCondition -> PresenceCondition
orPC p0@(PresenceCondition b0) p1@(PresenceCondition b1)
  | p0 == truePC = truePC
  | p1 == truePC = truePC
  | p0 == falsePC = p1
  | p1 == falsePC = p0
  | otherwise =
      let b = bOr manager b0 b1
          sb0 = lookupBDD' b0
          sb1 = lookupBDD' b1
       in newPC b ("(" ++ sb0 ++ " \\/ " ++ sb1 ++ ")")

notPC :: PresenceCondition -> PresenceCondition
notPC p@(PresenceCondition b)
  | p == truePC = falsePC
  | p == falsePC = truePC
  | otherwise =
      let b' = bNot manager b
       in newPC b' ("!" ++ lookupBDD' b)

(/\) :: PresenceCondition -> PresenceCondition -> PresenceCondition
(/\) = andPC

(\/) :: PresenceCondition -> PresenceCondition -> PresenceCondition
(\/) = orPC

-- Hashtable operations for creating new PresenceConditions

htSize :: (Eq k, Hashable k) => HashTable k v -> IO Int
htSize h = do
  xs <- H.toList h
  return $ length xs

sat :: PresenceCondition -> Bool
sat p = p /= falsePC

unsat :: PresenceCondition -> Bool
unsat p = p == falsePC

-- Val and Var definitions and functions

type Val a = (a, PresenceCondition)

newtype Var t = Var [Val t]
  deriving (Generic)

mkVar :: t -> PresenceCondition -> Var t
mkVar v pc = Var [(v, pc)]

mkVars :: [Val t] -> Var t
mkVars = Var

(^|) :: t -> PresenceCondition -> Var t
x ^| pc = mkVar x pc

pairs :: [t] -> [(t, t)]
pairs [] = []
pairs xs = zip xs (tail xs)

restrict :: PresenceCondition -> Var t -> Var t
restrict pc v'@(Var v)
  | pc == truePC = v'
  | pc == falsePC = Var []
  | otherwise = Var [(x, p) | (x, pc') <- v, let p = pc' /\ pc, sat p]

union :: Var t -> Var t -> Var t
union x@(Var a) y@(Var b) =
  let result = Var (a ++ b)
   in result

concatVar :: Var (Var t) -> Var t
concatVar (Var xs') = unions (map (\(x, pc) -> restrict pc x) xs')

disj :: [PresenceCondition] -> PresenceCondition
disj = foldr orPC falsePC

unions :: [Var t] -> Var t
unions = foldr union (Var [])

findVal :: t -> [Val t] -> (t -> t -> Bool) -> [Val t]
findVal _ [] _ = []
findVal v ((x, pc) : xs) cmp = if cmp v x then (x, pc) : rest else rest
  where
    rest = findVal v xs cmp

phelem :: t -> [t] -> (t -> t -> Bool) -> Bool
phelem v xs cmp = any (cmp v) xs

groupValsStep :: [Val t] -> [t] -> (t -> t -> Bool) -> [Val t]
groupValsStep [] _ _ = []
groupValsStep ((x, xpc) : xs) ds cmp =
  if phelem x ds cmp
    then rest
    else
      let ms = findVal x xs cmp
          pc = disj (xpc : map snd ms)
       in (x, pc) : rest
  where
    rest = groupValsStep xs (x : ds) cmp

groupVals :: [Val t] -> (t -> t -> Bool) -> [Val t]
groupVals xs = groupValsStep xs []

(===) :: a -> a -> Bool
x === y = unsafePerformIO $ do
  nx <- makeStableName $! x
  ny <- makeStableName $! y
  return (nx == ny)

compact :: Var t -> Var t
compact (Var v) = Var (groupVals v (===))

definedAt :: Var t -> PresenceCondition
definedAt (Var xs) = disj pcs
  where
    pcs = map snd xs

undefinedAt :: Var t -> PresenceCondition
undefinedAt = notPC . definedAt

index :: Var t -> PresenceCondition -> [t]
index (Var v) pc = map fst v'
  where
    v' = filter (\(x', pc') -> sat (pc /\ pc')) v

-- Completeness and Disjointness Invariants
disjInv :: Var t -> Bool
disjInv (Var v) = all (\((_, pc1), (_, pc2)) -> unsat (pc1 /\ pc2)) (pairs v)

compInv :: Var t -> Bool
compInv (Var v) =
  foldr (\(_, pc) pc' -> pc \/ pc') falsePC v == truePC

fixCompleteness :: a -> Var a -> Var a
fixCompleteness dummy v =
  let r = undefinedAt v
   in if r == falsePC then v else union v $ mkVar dummy r

-- Lifted apply definition

applyVal :: Val (a -> b) -> Var a -> Var b
applyVal (fn, fnpc) x'@(Var x) =
  mkVars $ [(fn v, pcr) | (v, pc) <- x, let pcr = fnpc /\ pc, sat pcr]

apply :: Var (a -> b) -> Var a -> Var b
apply f@(Var fn) x =
  assert (disjInv f) $
    assert (disjInv x) $
      unions [applyVal f x | f <- fn]

-- Functor, Applicative and Var definitions

instance Functor Var where
  fmap :: (a -> b) -> Var a -> Var b
  fmap f = apply (f ^| truePC)

instance Applicative Var where
  pure :: a -> Var a
  pure = (^| truePC)
  (<*>) :: Var (a -> b) -> Var a -> Var b
  (<*>) = apply

instance Monad Var where
  (>>=) :: Var a -> (a -> Var b) -> Var b
  (Var vs) >>= f = unions [applyOnVal f v | v <- vs]
    where
      applyOnVal :: (a -> Var b) -> Val a -> Var b
      applyOnVal f (a, pca) = let (Var bv) = f a in mkVars [(b, pcr) | (b, pcb) <- bv, let pcr = pca /\ pcb, sat pcr]

-- Other instances

showPCs :: Var a -> String
showPCs (Var v) = "{" ++ L.intercalate ", " (map (\(_, pc) -> show pc) v) ++ "}"

instance (Show a) => Show (Var a) where
  show :: (Show a) => Var a -> String
  show v' =
    let (Var v) = compact v'
     in "{" ++ L.intercalate ", " (map show v) ++ "}"

instance NFData (Var a) where
  rnf :: Var a -> ()
  rnf (Var xs) = xs `seq` map (\(x, pc) -> x `seq` pc `seq` ()) xs `seq` ()
