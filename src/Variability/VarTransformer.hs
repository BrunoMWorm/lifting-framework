{-# LANGUAGE InstanceSigs #-}

module Variability.VarTransformer where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Variability.VarLib (Var (Var), concatVar)

newtype VarT m a = VarT {runVarT :: m (Var a)}

mapVarT :: (m (Var a) -> n (Var b)) -> VarT m a -> VarT n b
mapVarT f m = VarT $ f (runVarT m)

instance (Functor m) => Functor (VarT m) where
  fmap :: (Functor m) => (a -> b) -> VarT m a -> VarT m b
  fmap f = mapVarT $ fmap $ fmap f

instance (Applicative m) => Applicative (VarT m) where
  pure :: (Applicative m) => a -> VarT m a
  pure a = VarT $ pure (pure a)
  (<*>) :: (Applicative m) => VarT m (a -> b) -> VarT m a -> VarT m b
  f <*> v = VarT $ (<*>) <$> runVarT f <*> runVarT v

instance (Monad m) => Monad (VarT m) where
  (>>=) :: (Monad m) => VarT m a -> (a -> VarT m b) -> VarT m b
  m >>= k =
    VarT $ do
      a <- runVarT m
      let x = fmap k a
          y@(Var ys) = fmap runVarT x
          vals = mapM fst ys
          pcs = map snd ys
       in fmap concatVar (\ls -> Var (zip ls pcs)) <$> vals

instance MonadTrans VarT where
  lift :: (Monad m) => m a -> VarT m a
  lift m = VarT $ do
    pure <$> m
