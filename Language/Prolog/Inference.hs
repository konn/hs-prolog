{-# LANGUAGE PackageImports, RankNTypes, FlexibleContexts #-}
module Language.Prolog.Inference where
import Control.Monad.RWS.Lazy hiding (Any, get, gets, modify, put, sequence)
import qualified Control.Monad.RWS.Lazy as RW
import Data.Map hiding (map, filter, null)
import Prelude hiding (map, lookup, sequence)
import Language.Prolog.Unification (unify)
import Language.Prolog.DataTypes
import Data.Maybe
import Data.Generics
import Data.List (sort, group)
import Control.Applicative hiding (empty)
import Data.Set hiding (map, null, union, filter, empty, fromList)
import qualified Data.Set as S
import Data.Traversable (sequence)

import Debug.Trace

map :: (Functor f) => (a -> b) -> f a -> f b
map = fmap

type InfMachine = RWST [Term] () InfState []
type InfState = (Subs,Set Val)

newVar :: Val -> InfMachine Val
newVar an@Any{} = do
  (a, ss) <- RW.get
  RW.put (a, S.insert an ss)
  return an
newVar b = return b

usedVals :: InfMachine (Set Val)
usedVals = RW.gets snd

modify f = do
  (a,b) <- RW.get
  RW.put (f a, b)

put s = modify (const s)

get = RW.gets fst
gets :: (Subs -> a) -> InfMachine a 
gets f = f <$> get

debug :: (Monad m) => String -> m ()
debug = flip trace $ return ()

proven :: Pred -> [Term] -> [Subs]
-- proven pred axiom = map (fst.fst) $ execRWST (prove pred) axiom (empty, S.fromList $ vars pred)
proven pred axiom = map fst $ evalRWST st axiom (empty, S.fromList vs)
  where
    vs = vars pred
    st = do
      prove pred
      dic <- get
      return $ map(`rewriteBy` dic) $ filterWithKey (\k v -> k`elem`vs) dic

merge :: (MonadState (Subs, a) m) => Subs -> m ()
merge = modify . flip union

prove :: Pred -> InfMachine ()
prove pred = do
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
