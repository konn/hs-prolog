{-# LANGUAGE PackageImports, RankNTypes #-}
module Language.Prolog.Inference where
import Control.Monad.RWS.Lazy hiding (Any)
import Data.Map hiding (map, filter, null)
import Prelude hiding (map, lookup)
import Language.Prolog.Unification (unify)
import Language.Prolog.DataTypes
import Data.Maybe
import Data.Generics
import Data.List (sort, group, intersect)
import Control.Applicative hiding (empty)

map :: (Functor f) => (a -> b) -> f a -> f b
map = fmap

type InfMachine = RWST [Term] () InfState []
type InfState = Subs

proven :: Pred -> [Term] -> [Subs]
proven pred axiom = map fst $ execRWST (prove pred) axiom empty

prove :: Pred -> InfMachine ()
prove pred = do
  axs <- ask
  koho <- forM axs $ \tr -> do
    tr'@(h:-_) <- rename tr pred
    return (tr', unify pred h)
  (h :- preds , Just dic) <- lift $ filter (isJust . snd) koho
  modify (union dic)
  case null preds of
    True -> return () -- modify (union $ fromJust $ unify pred h)
    False -> mapM_ (prove <=< rewrite) preds

rewrite :: forall a. (Data a) => a -> InfMachine a
rewrite s = rewriteBy s <$> get

vars :: forall a. (Data a) => a -> [Val]
vars = map head . group . sort . listify (\a->case a of{Any{} -> True; _ -> False})

rename :: Term -> Pred -> InfMachine Term
rename from env = do
  ks <- gets keys
  let dups = vars from `intersect` (vars env ++ ks)
      transed = flip everywhere from (mkT  $ \an ->
        case an of
          Any x n | (Any x n) `elem` dups -> Any x (n+1)
                  | otherwise             -> Any x n
          t                               -> t
                   )
  case dups of
    [] -> return from
    _  -> rename transed env