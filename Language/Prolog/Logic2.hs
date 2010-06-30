{-# LANGUAGE PackageImports, PatternGuards #-}
module Logic where
import Prelude hiding (map, lookup)
import Data.Map hiding (map)
import Control.Monad.State
import "mtl" Control.Monad.Trans
import Control.Applicative hiding (empty)
import Data.Functor
import Data.List hiding (map, insert, lookup)
import Data.Monoid (mconcat, All(..))

map :: (Functor f) => (a -> b) -> f a -> f b
map = fmap

data Pred  = Apply String [Var] 
data Var   = Exists String | Any String deriving (Eq, Ord)

data Axiom = Pred :- [Pred]
 
type Program = [Axiom]
type UnifMachine s a = StateT s Maybe a
type PrologMachine a = UnifMachine ([String], Program) a 

type BindDic = Map Var Var


proven :: Program -> Pred -> Bool
proven axs pred = maybe False id $ evalStateT (unify [pred]) ([],axs)
  where
    unify :: [Pred] -> PrologMachine Bool
    unify [] = return True
    unify xs = do
      goal <- liftM concat $ mapM hoge xs
      unify goal
    hoge :: Pred -> PrologMachine [Pred]
    hoge (Apply pred args) = do
      (syms, axioms) <- get
      lift $ (\(_:-p)->p) 
          <$> find (\(Apply s y :- preds) -> 
                        s == pred && length y == length args && 
                        (maybe False id $ flip evalStateT empty$ foldl1 (>>) (zipWith (\a b ->match a b) y args))) axioms
    match :: Var -> Var -> UnifMachine BindDic Bool
    match a@(Any x) b@(Exists _) = do
      t <- lift =<< gets (mplus (return b).lookup a)
      if t == b then do
          modify $ insert a b
          return True
        else mzero
    match (Any x) (Any y) | x == y = return True
                          | otherwise = mzero
    match a@(Exists x) b | (Exists y) <- b
                         , x == y = return True
                         | y@(Any _) <- b = do
                                          t <- lift =<< gets (flip mplus (return b).lookup a)
                                          if t == b then do
                                                      modify $ insert a b
                                                      return True
                                            else mzero
                         | otherwise = mzero