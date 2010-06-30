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
rewrite pred  = do
  dic <- get
  let f v@(Val a@(Any _)) = maybe (return v) rewrite $ lookup a dic
      f a         = return a
  everywhereM (mkM f) pred 

vars :: forall a. (Data a) => a -> [Val]
vars = map head . group . sort . listify (\a->case a of{Any _-> True; _ -> False})

rename :: Term -> Pred -> InfMachine Term
rename from env = do
  ks <- gets keys
  let dups = vars from `intersect` (vars env ++ ks)
      transed = flip everywhere from (mkT  $ \an ->
        case an of
          Any x | (Any x) `elem` dups -> Any (x++"1")
                | otherwise           -> Any x
          t                           -> t
                   )
  case dups of
    [] -> return from
    _  -> rename transed env