{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

module Stlc.Unify
  ( unify
  ) where

import           Stlc.Types
import           Stlc.Subst

import           Control.Lens                     (noneOf)
import           Data.Sequence                    (ViewL ((:<)), (|>))
import qualified Data.Sequence                    as Seq
import           Unbound.Generics.LocallyNameless

-- This file uses lenses somewhat heavily. Lenses tend to cause a lot of
-- confusion, especially for Haskell beginners. Since this project is mostly
-- just a learning experience for myself, I figured I'd take the second to
-- learn them (to have one more tool in my pocket).
--
-- At a high level, lenses are really nice generalizations around getting,
-- setting, traversing, and folding. Since most of the code we write is just
-- massaging the data into the right shape, lenses help write concise code.
-- Concise code is also more likely to be correct!
--
-- I found watching a talk on lenses + implementing something with lenses (to
-- have to play around with the type error messages) a pretty good way to
-- learn.

-- This uses lenses because unbound-generic's fv supports lenses!
-- fv is a Getter combinator that gets the free variables of a structure, and
-- noneOf lets us fold over that structure, checking that none of them are (== t)
notInFreeVars :: Cvar -> Con -> Bool
notInFreeVars t t1 = noneOf fv (== t) t1

-- This function uses ViewPatterns, which let us run a function and pattern
-- match on the result: (viewFn -> pattern)
-- It's kind of like sticking a unix pipe between the argument and the pattern.
unify :: Seq.Seq Constraint -> Maybe (Seq.Seq (ExplSubst Con))
unify (Seq.viewl -> Seq.EmptyL) = Just Seq.empty
unify (Seq.viewl -> (Constraint t1 t2) :< cs)
  | aeq t1 t2 = unify cs
unify (Seq.viewl -> (Constraint (Cvar t) t2) :< cs)
  | notInFreeVars t t2 = do
    let explSubst = (ExplSubst t t2)
    sol <- unify $ substOverConstraints explSubst cs
    Just (sol |> explSubst)
unify (Seq.viewl -> (Constraint t1 (Cvar t)) :< cs)
  | notInFreeVars t t1 = do
    let explSubst = (ExplSubst t t1)
    sol <- unify $ substOverConstraints explSubst cs
    Just (sol |> explSubst)
unify (Seq.viewl -> (Constraint (Carrow s1 s2) (Carrow t1 t2)) :< cs) =
  unify (cs |> Constraint s1 t1 |> Constraint s2 t2)
  -- TODO(jez) Be more explicit about fall through here
  -- TODO(jez) Better error messages
unify _ = Nothing
