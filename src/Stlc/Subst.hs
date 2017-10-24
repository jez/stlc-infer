{-# LANGUAGE FlexibleContexts #-}

module Stlc.Subst where

import           Stlc.Types

import           Control.Lens                     (over)
import           Unbound.Generics.LocallyNameless (Subst, subst)

-- uses lenses to update both the left and right sides of a constraint.
substOverConstraint
  :: Subst a Con
  => ExplSubst a
  -> Constraint
  -> Constraint
substOverConstraint (ExplSubst x t) = over bothConstraint $ subst x t

-- traverses over outer container, then uses subst to set both
-- the left and right sides of each constraint.
substOverConstraints
  :: (Traversable t, Subst a Con)
  => ExplSubst a
  -> t Constraint
  -> t Constraint
substOverConstraints (ExplSubst x t) =
  over (traverse . bothConstraint) $ subst x t
