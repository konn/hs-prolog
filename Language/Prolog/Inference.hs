{-# LANGUAGE PackageImports, RankNTypes, FlexibleContexts, NamedFieldPuns, PatternGuards, ScopedTypeVariables  #-}
module Language.Prolog.Inference where
import Language.Prolog.Unification (unify)
import Language.Prolog.DataTypes
import Language.Prolog.Arithmetic

import Control.Monad.RWS.Lazy hiding (Any, get, gets, modify, put, sequence)
import qualified Control.Monad.RWS.Lazy as RW
import Data.Map hiding (map, filter, null)
import Prelude hiding (map, lookup, sequence)
import Data.Maybe
import Data.Generics
import Data.List (sort, group)
import Control.Applicative hiding (empty)
import Data.Set hiding (map, null, union, filter, empty, fromList, member)
import qualified Data.Set as S
import Data.Traversable (sequence)

import Debug.Trace

map :: (Functor f) => (a -> b) -> f a -> f b
map = fmap

type InfMachine = RWST [Term] () InfState []
data InfState = IS{ subs :: Subs, used :: Set Val, level :: Int}
createInfState = IS{subs = empty, used = S.empty, level = 0}

newVar :: Val -> InfMachine Val
newVar an@Any{} = do
  is@IS{used} <- RW.get
  RW.put is{used=S.insert an used}
  return an
newVar b = return b

usedVals :: InfMachine (Set Val)
usedVals = RW.gets used

modify f = do
  is@IS{subs} <- RW.get
  RW.put is{subs= f subs}

put s = modify (const s)

get = RW.gets subs
gets :: (Subs -> a) -> InfMachine a 
gets f = f <$> get

debug :: (Show a) => String -> a -> InfMachine ()
debug header a = do
  c <- RW.gets level
  trace (concat[replicate c '\t', header, ": ", show a]) $ return ()

proven :: Pred -> [Term] -> [Subs]
proven pred axiom = map fst $ evalRWST st axiom createInfState{used=S.fromList $ vars pred}
  where
    vs = vars pred
    st = do
      prove pred
      dic <- get
      return $ map(`rewriteBy` dic) $ filterWithKey (\k v -> k`elem`vs) dic

incr :: InfMachine ()
incr = do
  is@IS{level} <- RW.get
  RW.put is{level= level+1}

decr :: InfMachine ()
decr = do
  is@IS{level} <- RW.get
  RW.put is{level=level-1}

merge :: (MonadState InfState m) => Subs -> m ()
merge = modify . flip union

prims = fromList [(("not", -1), notPrim), (("\\=",2), neqPrim), (("is", 2), isPrim)]
  where
    notPrim preds = do
      st <- RW.get
      axs  <- ask
      let ss = map fst $ execRWST (mapM_ (rewrite>=>prove) preds) axs st
      guard (null ss)
    neqPrim [x,y] = do
      [x', y'] <- mapM rewrite [x,y]
      guard (x' /= y')
    isPrim [x,y] = do
      y'  <- rewrite y
      ans <- evalArith y'
      subs <- lift ( (unify x) . Val . Integer $ ans )
      merge subs

prove :: Pred -> InfMachine ()
prove (Val (Exists sym)) = case sym of
                             "true"  -> return ()
                             "false" -> mzero
                             "fail"  -> mzero
                             _       -> fail "unko"
prove (Val Any{}) = mzero
prove pred | App a ps <- pred,
             (a, length ps) `member` prims = (prims ! (a, length ps)) ps
           | App a ps <- pred,
             (a, -1) `member` prims        = (prims ! (a, -1)) ps
           | otherwise = (arithPred pred>>=guard) `mplus` do
    axs <- ask
    koho <- forM axs $ \tr -> do
      tr'@(h:-_) <- rename tr
      return (tr', unify pred h)
    (h :- preds , Just dic) <- lift $ filter (isJust . snd) koho
    merge dic
    case null preds of
      True -> return ()
      False -> mapM_ (rewrite >=> prove) preds

rewrite :: forall a. (Data a) => a -> InfMachine a
rewrite s = rewriteBy s <$> get

vars :: forall a. (Data a) => a -> [Val]
vars = map head . group . sort . listify (\a->case a of{Any{} -> True; _ -> False})

rename :: forall a. (Data a,Show a) => a -> InfMachine a
rename from = do
  dups <- S.intersection (S.fromList $ vars from) <$> usedVals
  let transed = flip everywhere from $ mkT  $ \an ->
        case an of
          Any x n | (Any x n) `S.member` dups -> Any x (n+1)
                  | otherwise                 -> Any x n
          t                                   -> t
  case S.null dups of
    True  -> mapM newVar (vars from) >> return from
    False -> rename transed
