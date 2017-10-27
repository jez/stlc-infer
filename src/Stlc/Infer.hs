module Stlc.Infer
  ( principalType
  , inferTerm
  ) where

import           Stlc.Constrain
import           Stlc.Types
import           Stlc.Unify

import           Unbound.Generics.LocallyNameless

-- The top-level type we get back from constraint generation might have free
-- type variables. The point of unification is to solve for those variables
-- and return a substitution representing the solution.
--
-- To solve for the actual type of a term, we apply the solution substitution
-- to the constraint generations's type. We call this the "principal type"
-- because unification always gives us back a "principal solution" (this is
-- Theorem 22.4.5 in TAPL)
--
-- TODO(jez) I'm not sure if folding is okay here,
-- because it breaks the assumption that substitutions are simultaneous.
principalType :: Foldable t => t (ExplSubst Con) -> Con -> Con
principalType sol t0 = foldr folder t0 sol
  where folder :: (ExplSubst Con) -> Con -> Con
        folder (ExplSubst x t) = subst x t

-- Once we've separated our code into constraint generation and constraint
-- unification, inference is just a matter of composing them!
inferTerm :: Term -> Maybe Con
inferTerm term = do
  let (t0, cs) = constraints term
  sol <- unify cs
  Just $ principalType sol t0

