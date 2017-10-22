{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

module Stlc.Unify where

import           Stlc.Types

import           Control.Lens                     (noneOf)

-- TODO(jez) How to use patterns?
-- TODO(jez) Fix type errors
import           Data.Sequence                    (ViewL ((:<)), (|>))
import qualified Data.Sequence                    as Seq
import           Unbound.Generics.LocallyNameless

applyExplSubst :: (Subst a a) => ExplicitSubst a -> a -> a
applyExplSubst (ExplicitSubst (x, e)) = subst x e

overConstraint :: (Con -> Con) -> Constraint -> Constraint
overConstraint f (Constraint (t1, t2)) = Constraint (f t1, f t2)

notInFreeVars :: Cvar -> Con -> Bool
notInFreeVars t t1 = noneOf fv (== t) t1

unify :: Seq.Seq Constraint -> Maybe (Seq.Seq (ExplicitSubst Con))
unify (Seq.viewl -> Seq.EmptyL) = Just Seq.empty
unify (Seq.viewl -> Constraint (t1, t2) :< cs)
  | aeq t1 t2 = unify cs
unify (Seq.viewl -> Constraint (Cvar t, t2) :< cs)
  -- This uses lenses.
  -- fv is a "getter" combinator that gets the free variables of a structure,
  -- and noneOf lets us fold over that structure,
  -- checking that none of them are (== t)
  | notInFreeVars t t2 = do
    let explSubst = ExplicitSubst (t, t2)
    sol <- unify $ (overConstraint $ applyExplSubst explSubst) <$> cs
    Just (sol |> explSubst)
unify (Seq.viewl -> Constraint (t1, Cvar t) :< cs)
  | notInFreeVars t t1 = do
    let explSubst = ExplicitSubst (t, t1)
    sol <- unify $ (overConstraint $ applyExplSubst explSubst) <$> cs
    Just (sol |> explSubst)
unify (Seq.viewl -> Constraint (Carrow s1 s2, Carrow t1 t2) :< cs) =
  unify (cs |> Constraint (s1, t1) |> Constraint (s2, t2))
  -- TODO(jez) Be more explicit about fall through here
unify _ = Nothing
