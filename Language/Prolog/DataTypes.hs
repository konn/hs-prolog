{-# LANGUAGE DeriveDataTypeable, FlexibleInstances #-}
module Language.Prolog.DataTypes(Term(..), Pred(..), Val(..), Subs(..), Func, Symbol) where
import Data.Data
import Data.Map

data Term = Pred :- [Pred] deriving (Show, Eq, Ord, Typeable, Data)
data Pred   = App Func [Pred] | Val Val deriving (Eq, Ord, Show, Typeable, Data)
data Val    = Exists Symbol | Any Symbol deriving (Eq, Ord, Show, Typeable, Data)
type Subs   = Map Val Pred
type Func   = String
type Symbol = String

instance Monad (Either String) where
    return a = Right a

    (Right a) >>= f = f a
    (Left st) >>= _ = Left st
    fail str = Left str

