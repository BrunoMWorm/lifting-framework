{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE InstanceSigs #-}

module Variability.Legacy.VarTypes where

import Control.Exception (assert)
import Cudd.Cudd (DDManager, DDNode, bAnd, bNot, bOr, cuddInit, ithVar, nodeReadIndex, readLogicZero, readOne)
import qualified Data.HashTable.IO as H
import Data.Hashable (Hashable (hashWithSalt))
import qualified Data.List as L
import Data.Vector (replicate, toList, (!), (//))
import GHC.IO (unsafePerformIO)
import GHC.StableName (makeStableName)
import Control.Applicative (liftA)

type HashTable k v = H.BasicHashTable k v

instance Hashable DDNode where
  {-# INLINE hashWithSalt #-}
  hashWithSalt :: Int -> DDNode -> Int
  hashWithSalt s d = hashWithSalt s (nodeReadIndex d)

newtype Prop = Prop
  { b :: DDNode
  }

instance Eq Prop where
  {-# INLINE (==) #-}
  (==) :: Prop -> Prop -> Bool
  (Prop b0) == (Prop b1) = b0 == b1

instance Ord Prop where
  (<=) :: Prop -> Prop -> Bool
  (Prop b0) <= (Prop b1) = nodeReadIndex b0 <= nodeReadIndex b1

instance Hashable Prop where
  {-# INLINE hashWithSalt #-}
  hashWithSalt :: Int -> Prop -> Int
  hashWithSalt s (Prop b) = hashWithSalt s b

instance Show Prop where
  {-# INLINE show #-}
  show :: Prop -> String
  show (Prop b) = show b

manager :: Cudd.Cudd.DDManager
manager = cuddInit

{-# INLINE tt #-}
tt :: Prop
tt = newBDD (readOne manager)

{-# INLINE ff #-}
ff :: Prop
ff = newBDD (readLogicZero manager)

{-# INLINE newBDD #-}
newBDD :: DDNode -> Prop
newBDD = Prop

mkBDDVar :: String -> Prop
mkBDDVar name =
  let i = lookupVar name
      r = ithVar manager i
   in newBDD r

{-# INLINE andBDD #-}
andBDD :: Prop -> Prop -> Prop
andBDD p0@(Prop b0) p1@(Prop b1)
  | p0 == ff = ff
  | p1 == ff = ff
  | p0 == tt = p1
  | p1 == tt = p0
  | otherwise = newBDD $ bAnd manager b0 b1

{-# INLINE orBDD #-}
orBDD :: Prop -> Prop -> Prop
orBDD p0@(Prop b0) p1@(Prop b1)
  | p0 == tt = tt
  | p1 == tt = tt
  | p0 == ff = p1
  | p1 == ff = p0
  | otherwise = newBDD $ bOr manager b0 b1

{-# INLINE notBDD #-}
notBDD :: Prop -> Prop
notBDD p@(Prop b)
  | p == tt = ff
  | p == ff = tt
  | otherwise = newBDD $ bNot manager b

sat :: Prop -> Bool
sat p = p /= ff

unsat :: Prop -> Bool
unsat p = p == ff

var2index :: HashTable String Int
{-# NOINLINE var2index #-}
var2index = unsafePerformIO H.new

lookupVar :: String -> Int
lookupVar v = unsafePerformIO $ do
  i <- H.lookup var2index v
  case i of
    Nothing -> do
      i' <- htSize var2index
      !d0 <- H.insert var2index v i'
      return i'
    Just i' -> return i'

htSize :: (Eq k, Hashable k) => HashTable k v -> IO Int
htSize h = do
  xs <- H.toList h
  return $ length xs

disj :: [Prop] -> Prop
disj = foldr orBDD ff

type PresenceCondition = Prop

type Context = PresenceCondition

type PCExpr = Prop

{-# INLINE (/\) #-}
(/\) :: Prop -> Prop -> Prop
(/\) = andBDD

{-# INLINE (\/) #-}
(\/) :: Prop -> Prop -> Prop
(\/) = orBDD

{-# INLINE negPC #-}
negPC :: Prop -> Prop
negPC = notBDD

type Val a = (a, PresenceCondition)

newtype Var t = Var [Val t]

findVal :: t -> [Val t] -> (t -> t -> Bool) -> [Val t]
findVal _ [] _ = []
findVal v ((x, pc) : xs) cmp = if cmp v x then (x, pc) : rest else rest
  where
    rest = findVal v xs cmp

{-# INLINE phelem #-}
phelem :: t -> [t] -> (t -> t -> Bool) -> Bool
phelem v xs cmp = any (cmp v) xs

groupVals_ :: [Val t] -> [t] -> (t -> t -> Bool) -> [Val t]
groupVals_ [] _ _ = []
groupVals_ ((x, xpc) : xs) ds cmp =
  if phelem x ds cmp
    then rest
    else
      let ms = findVal x xs cmp
          pc = disj (xpc : map snd ms)
       in (x, pc) : rest
  where
    rest = groupVals_ xs (x : ds) cmp

groupVals :: [Val t] -> (t -> t -> Bool) -> [Val t]
groupVals xs = groupVals_ xs []

{-# INLINE (===) #-}
(===) :: a -> a -> Bool
(!x) === (!y) = unsafePerformIO $ do
  nx <- makeStableName $! x
  ny <- makeStableName $! y
  return (nx == ny)

compact :: Var t -> Var t
compact (Var v) = Var (groupVals v (===))

instance (Show a) => Show (Var a) where
  show :: (Show a) => Var a -> String
  show v' =
    let (Var v) = compact v'
     in "{" ++ L.intercalate ", " (map show v) ++ "}"

instance Functor Var where
  fmap :: (a -> b) -> Var a -> Var b
  fmap f = apply (f ^| ttPC)

instance Applicative Var where
  pure :: a -> Var a
  pure = (^| ttPC)
  (<*>) :: Var (a -> b) -> Var a -> Var b
  (<*>) = apply

class VClass a where
  nil :: a b
  isNil :: a b -> Bool
  at :: a b -> PresenceCondition
  comb :: a b -> a b -> a b
  caseSplitter :: a b -> (b -> Int) -> Int -> [a b]
  combs :: [a b] -> a b
  combs = foldr comb nil
  restrict :: PresenceCondition -> a b -> a b

union :: Var t -> Var t -> Var t
union x@(Var a) y@(Var b) =
  let result = Var (a ++ b)
   in result

partitionInv :: Var a -> [Var a] -> Bool
partitionInv x xs = definedAt x == cover
  where
    cover = foldr ((\/) . definedAt) ffPC xs

definedAt :: Var t -> PresenceCondition
definedAt (Var xs) = disj pcs
  where
    pcs = map snd xs

instance VClass Var where
  nil :: Var b
  nil = Var []
  isNil :: Var b -> Bool
  at :: Var b -> PresenceCondition
  isNil (Var xs) = null xs
  at = definedAt
  comb :: Var b -> Var b -> Var b
  comb = union
  caseSplitter :: Var b -> (b -> Int) -> Int -> [Var b]
  caseSplitter i@(Var input) splitter range =
    let initV = Data.Vector.replicate range nil
        xs =
          foldl
            ( \vec (v, pc) ->
                let index = splitter v
                    (Var item) = vec ! index
                    item' = Var $ (v, pc) : item
                 in vec // [(index, item')]
            )
            initV
            input
        ret = toList xs
     in assert (partitionInv i ret) ret

  restrict :: PresenceCondition -> Var b -> Var b
  restrict pc v'@(Var v)
    | pc == ttPC = v'
    | pc == ffPC = Var []
    | otherwise = Var [(x, p) | (x, pc') <- v, let p = pc' /\ pc, sat p]

disjInv :: Var t -> Bool
disjInv v'@(Var v) = all (\((_, pc1), (_, pc2)) -> unsat (pc1 /\ pc2)) (pairs v)

compInv :: Var t -> Bool
compInv (Var v) =
  foldr (\(_, pc) pc' -> pc \/ pc') ffPC v == ttPC

mkVar :: t -> PresenceCondition -> Var t
{-# INLINE mkVar #-}
mkVar v pc = Var [(v, pc)]

mkVars :: [(t, PresenceCondition)] -> Var t
mkVars = Var

(^|) :: t -> PresenceCondition -> Var t
x ^| pc = mkVar x pc

infixl 9 ^|

unions :: [Var t] -> Var t
unions = foldr union (Var [])

{-# INLINE apply_ #-}
apply_ :: Val (a -> b) -> Var a -> Var b
apply_ (fn, !fnpc) x'@(Var x) =
  mkVars $ [(fn v, pc') | (v, !pc) <- x, let !pc' = fnpc /\ pc, sat pc']

{-# INLINE apply #-}
apply :: Var (a -> b) -> Var a -> Var b
apply f@(Var fn) x =
  assert (disjInv f) $
    assert (disjInv x) $
      unions [apply_ f x | f <- fn]

pairs :: [t] -> [(t, t)]
pairs [] = []
pairs xs = zip xs (tail xs)

ttPC :: Prop
ttPC = tt

ffPC :: Prop
ffPC = ff

valList :: Var a -> [Val a]
valList (Var ls) = ls

(|||) :: Var a -> PresenceCondition -> Var a
(Var listPCv) ||| pcR = Var ([(v, pc') | (v, pc) <- listPCv, let pc' = pc /\ pcR, sat pc'])

(+++) :: Var a -> Var a -> Var a
(Var lvint1) +++ (Var lvint2) = Var (lvint1 ++ lvint2)

(/^) :: (VClass a) => a b -> PresenceCondition -> a b
(/^) x pc = restrict pc x

evalCond :: Var Bool -> (PresenceCondition, PresenceCondition)
evalCond c'@(Var c) =
  let t = filter fst c
      f = filter (\(v, pc) -> not v) c
      tPC = foldr (\(_, pc) x -> x \/ pc) ffPC t
      fPC = foldr (\(_, pc) x -> x \/ pc) ffPC f
   in assert (tPC /\ fPC == ffPC) $
        assert (tPC \/ fPC == definedAt c') (tPC, fPC)

liftedCond :: (VClass a) => Var Bool -> (PresenceCondition -> a b) -> (PresenceCondition -> a b) -> a b
liftedCond c x y =
  let (t, f) = evalCond c
   in if t == ffPC
        then y f
        else
          if f == ffPC
            then x t
            else comb (x t /^ t) (y f /^ f)

mkVarT :: a -> Var a
{-# INLINE mkVarT #-}
mkVarT v = v ^| tt

not' :: Var Bool -> Var Bool
not' = liftV not

head' :: [Var a] -> Var a
head' = head --liftV head

tail' :: [Var a] -> [Var a]
tail' = tail

null' :: Foldable t => Var (t a) -> Var Bool
null' = liftV null

fst' :: Var (a, b) -> Var a
fst' = liftV fst

snd' :: Var (a, b) -> Var b
snd' = liftV snd

{-# INLINE liftV #-}
liftV :: (a -> b) -> Var a -> Var b
liftV = fmap
