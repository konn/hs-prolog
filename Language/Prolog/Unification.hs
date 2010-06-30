{-# LANGUAGE PatternGuards #-}
module Language.Prolog.Unification (unify) where
import Control.Monad
import Control.Monad.State
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
    (Val v@(Any a))    -> if v == val then return (Val val) else lastSubs v 
    s                  -> return s
    

proven :: Pred -> [Term] -> Bool
proven (App f prds) axiom = undefined

unify :: (Monad m) => Pred -> Pred -> m Subs
unify pr1 pr2 = execStateT (unifyPred pr1 pr2) empty 

unifyPreds :: (Monad m) => [Pred] -> [Pred] -> UnifMachine m ()
unifyPreds ps1 ps2 
           | length ps1 == length ps2 = zipWithM_ unifyPred ps1 ps2
           | otherwise  = fail "arguments number didn't match"

neverOccur :: (Monad m) => Val -> Pred -> UnifMachine m Bool
neverOccur (Any x) (App _ xs) = 
    liftM (foldl (&&) True) $ mapM (neverOccur (Any x)) xs
neverOccur (Any x) (Val (Any y)) = return True
neverOccur (Any x) (Val (Exists _)) = return True

unifyPred :: (Monad m) => Pred -> Pred -> UnifMachine m ()
unifyPred (Val a) (Val b) = unifyVal a b
unifyPred (Val a) f@(App _ _) 
    | (Any _) <- a = do
        cond <- a `neverOccur` f
        case cond of
          True -> do
            modify $ insert a f
          False -> fail "occurs twice"
    | otherwise = fail "didn't match"

unifyPred b a@(Val (Any _)) = unifyPred a b

unifyPred (App f1 ps1) (App f2 ps2)
          | f1 == f2  = unifyPreds ps1 ps2
          | otherwise = fail "Functors didn't match"

unifyPred a b = fail ("not supported: " ++ unwords [show a,show b])

unifyVal :: (Monad m) => Val -> Val -> UnifMachine m ()
unifyVal an@(Any a) ex@(Exists b) = do
  sub <- lastSubs an
  case sub of
    (Val (Any a2))    -> modify (insert (Any a2) (Val ex))
    (Val (Exists b2)) -> when (b/=b2) (fail "Constant name didn't match.")
    (App _ _) -> fail ""

unifyVal e@(Exists _) a@(Any _) = unifyVal a e
unifyVal a b | a == b = return ()
             | (Any x, Any y) <- (a, b) = modify (insert (Any x) (Val $ Any y))
             | otherwise = fail "Constant name didn't match."
