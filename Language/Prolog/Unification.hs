{-# LANGUAGE PatternGuards #-}
module Language.Prolog.Unification (unify) where
import Control.Monad
import Control.Monad.State.Lazy
import Prelude hiding (map, lookup)
import Data.Map hiding (map)
import Data.Maybe

import Language.Prolog.DataTypes

type UnifMachine = StateT Subs

map :: Functor f => (a -> b) -> f a -> f b
map = fmap

unify :: (Monad m) => Pred -> Pred -> m Subs
unify pr1 pr2 = execStateT (unifyPred pr1 pr2) empty 

unifyPreds :: (Monad m) => [Pred] -> [Pred] -> UnifMachine m ()
unifyPreds ps1 ps2 
           | length ps1 == length ps2 = zipWithM_ unifyPred ps1 ps2
           | otherwise                = fail "arguments number didn't match"

unifyPred :: (Monad m) => Pred -> Pred -> UnifMachine m ()
unifyPred (Val a) (Val b) = unifyVal a b
unifyPred (Val a) f@(App _ _)
    | Wild  <- a = return ()
    | Any{} <- a = do
            f' <- lastSubs a
            case f' of
              (App _ _)                -> when (f' /= f) (fail "unification failed")
              (Val v) | Exists  e <- v -> when (f' /= (Val v)) (fail "unification failed")
                      | Integer i <- v -> when (f' /= (Val v)) (fail "unification failed")
                      | otherwise      -> return ()
            modify $ insert a f
    | otherwise = fail "didn't match"

unifyPred b a@(Val Any{}) = unifyPred a b

unifyPred (App f1 ps1) (App f2 ps2)
          | f1 == f2  = unifyPreds ps1 ps2
          | otherwise = fail "Functors didn't match"

unifyPred a b = fail ("not supported: " ++ unwords [show a,show b])

unifyVal :: (Monad m) => Val -> Val -> UnifMachine m ()
unifyVal Wild _ = return ()
unifyVal _ Wild = return ()
unifyVal a1@Any{} a2@Any{} = do
  ls <- lastSubs a1
  case ls of
    ap@(App _ _)   -> modify (insert a2 ap)
    an@(Val Any{}) -> modify (insert a1 (Val a2))
    an@(Val _)     -> modify (insert a2 an)
unifyVal an@Any{} ex = do
  sub <- lastSubs an
  case sub of
    (Val a2@Any{})    -> modify (insert a2 (Val ex))
    (Val ex2) -> when (ex/=ex2) (fail "Constant name didn't match.")
    (App _ _) -> fail ""
unifyVal e a@Any{} = unifyVal a e
unifyVal a b | a == b = return ()
             | otherwise = fail "Constant name didn't match."
