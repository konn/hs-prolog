{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, RankNTypes #-}
module Language.Prolog.DataTypes(Term(..), Pred(..), Val(..), Subs(..), Func, Symbol, rewriteBy) where
import Data.Generics
import Data.Map (Map, lookup)
import Data.List hiding (lookup)
import Prelude hiding (lookup)
import Control.Monad.State

data Term = Pred :- [Pred] deriving (Eq, Ord, Typeable, Data)
data Pred   = App Func [Pred] | Val Val deriving (Eq, Ord, Typeable, Data)
data Val    = Exists Symbol | Any {name::Symbol, no::Int} | Wild
              deriving (Eq, Ord, Typeable, Data)
type Subs   = Map Val Pred
type Func   = String
type Symbol = String

instance Show Val where
    show (Any a 0)  = a
    show (Any a n)  = a ++ show n
    show Wild       = "_"
    show (Exists b) = b

instance Show Pred where
    show (Val a) = show a
    show a@(App "cons" [x,y]) = "[" ++ (either id id $ showL a) ++ "]" 
      where
        showL :: Pred -> Either String String
        showL (App "cons" [x, Val (Exists "[]")]) = Right $ show x
        showL (App "cons" [x,y]) = let prf c a = concat [show x, c, a] 
                                   in  Right $ either (prf "|") (prf ",") $ showL y
        showL a                  = Left $ show a
    show (App sym args) = concat [sym, "(", intercalate "," (map show args), ")"]

instance Show Term where
    show (a :- []) = show a ++ "."
    show (a :- as) = concat [show a, " :- ", intercalate "," $ map show as, "."]

instance Monad (Either String) where
    return a = Right a

    (Right a) >>= f = f a
    (Left st) >>= _ = Left st
    fail str = Left str

rewriteBy :: forall a. (Data a) => a -> Subs ->  a
rewriteBy pred dic =
  let f v@(Val a@Any{}) = maybe v (`rewriteBy` dic) $ lookup a dic
      f a         = a
  in everywhere (mkT f) pred 
