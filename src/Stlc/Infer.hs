module Stlc.Infer where

import           Stlc.Constrain
import           Stlc.Types
import           Stlc.Unify

import           Unbound.Generics.LocallyNameless

-- TODO(jez) I'm not sure if folding is okay here,
-- because it breaks the assumption that substitutions are simultaneous.
principalType :: Foldable t => t (ExplSubst Con) -> Con -> Con
principalType sol t0 = foldr folder t0 sol
  where folder :: (ExplSubst Con) -> Con -> Con
        folder (ExplSubst x t) = subst x t

inferTerm :: Term -> Maybe Con
inferTerm term = do
  let (t0, cs) = constraintsWithCon term
  sol <- unify cs
  Just $ principalType sol t0

