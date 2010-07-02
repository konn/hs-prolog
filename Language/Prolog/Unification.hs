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

lastSubs :: (Monad m) => Val -> UnifMachine m Pred
lastSubs val = do
  s <- gets (fromMaybe (Val $ val). lookup val)
  case s of
    v@(Val (Exists _)) -> return v
    (Val v@Any{})    -> if v == val then return (Val val) else lastSubs v 
    s                  -> return s

unify :: (Monad m) => Pred -> Pred -> m Subs
unify pr1 pr2 = execStateT (unifyPred pr1 pr2) empty 

unifyPreds :: (Monad m) => [Pred] -> [Pred] -> UnifMachine m ()
unifyPreds ps1 ps2 
           | length ps1 == length ps2 = zipWithM_ unifyPred ps1 ps2
           | otherwise                = fail "arguments number didn't match"

neverOccur :: (Monad m) => Val -> Pred -> UnifMachine m Bool
neverOccur (Any x n) (App _ xs) = 
    liftM (foldl (&&) True) $ mapM (neverOccur (Any x n)) xs
neverOccur Any{} _ = return True
neverOccur Wild _ = return False

unifyPred :: (Monad m) => Pred -> Pred -> UnifMachine m ()
unifyPred (Val a) (Val b) = unifyVal a b
unifyPred (Val a) f@(App _ _)
    | Wild  <- a = return ()
    | Any{} <- a = do
        cond <- a `neverOccur` f
        case cond of
          True -> do
            f' <- lastSubs a
            case f' of
              (App _ _)               -> when (f' /= f) (fail "unification failed")
              (Val v) | Exists e <- v -> when (f' /= (Val v)) (fail "unification failed")
                      | otherwise     -> return ()
            modify $ insert a f
          False -> fail "occurs twice"
    | otherwise = fail "didn't match"

unifyPred b a@(Val Any{}) = unifyPred a b

unifyPred (App f1 ps1) (App f2 ps2)
          | f1 == f2  = unifyPreds ps1 ps2
          | otherwise = fail "Functors didn't match"

unifyPred a b = fail ("not supported: " ++ unwords [show a,show b])

unifyVal :: (Monad m) => Val -> Val -> UnifMachine m ()
unifyVal Wild _ = return ()
unifyVal _ Wild = return ()
unifyVal an@Any{} ex@(Exists b) = do
  sub <- lastSubs an
  case sub of
    (Val a2@Any{})    -> modify (insert a2 (Val ex))
    (Val (Exists b2)) -> when (b/=b2) (fail "Constant name didn't match.")
    (App _ _) -> fail ""

unifyVal e@(Exists _) a@Any{} = unifyVal a e
unifyVal a b | a == b = return ()
             | (a1@Any{}, a2@Any{}) <- (a, b) = do
                  ls <- lastSubs a1
                  case ls of
                    ap@(App _ _)   -> modify (insert a2 ap)
                    an@(Val Any{}) -> modify (insert a1 (Val a2))
                    an@(Val _)     -> modify (insert a2 an)
             | otherwise = fail "Constant name didn't match."
