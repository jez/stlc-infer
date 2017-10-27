{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Stlc.Constrain
  ( constraints
  ) where

import           Stlc.Types

import           Control.Monad.Writer
import qualified Data.Sequence                    as Seq
import           Data.Map.Strict                  ((!))
import qualified Data.Map.Strict                  as Map
import           Unbound.Generics.LocallyNameless

type Ctx = Map.Map Tvar Con

-- The Writer monad is really just an abstraction around an accumulator argument
-- and 'tell' is how we append something in the accumulator.
constrain :: Con -> Con -> FreshMT (Writer (Seq.Seq Constraint)) ()
constrain t1 t2 = lift . tell . Seq.singleton $ Constraint t1 t2

-- Implements the judgement from TAPL Exercise 22.3.9:
--
--     Γ ⊢F e : τ |F' C
--
-- by threading F through the FreshMT monad, and
-- collecting the constraints C using a Writer monad.
--
-- By also returning the τ with the judgement, we can think of this as
-- "type-directed" constraint generation, because the constraint generation
-- piggy backs off the typing derivation, rather than just following the syntax.
constraintsWithCon :: Ctx -> Term -> FreshMT (Writer (Seq.Seq Constraint)) Con
constraintsWithCon ctx (Tvar x) = return $ ctx ! x
constraintsWithCon ctx (Tlam bnd) = do
  -- 'out' the ABT to get a fresh variable
  -- (x used to be "locally nameless", but now has a globally unique name)
  (x, e) <- unbind bnd
  -- Generate fresh type variable to put into the context
  t1 <- Cvar <$> fresh (s2n "t1_")
  let ctx' = Map.insert x t1 ctx
  t2 <- constraintsWithCon ctx' e
  return $ Carrow t1 t2
constraintsWithCon ctx (Tapp e1 e2) = do
  t1 <- constraintsWithCon ctx e1
  t2 <- constraintsWithCon ctx e2
  t <- Cvar <$> fresh (s2n "t_")
  constrain t1 (Carrow t2 t)
  return t
constraintsWithCon ctx (Tlet e1 bnd) = do
  (x, e2) <- unbind bnd
  t1 <- constraintsWithCon ctx e1
  let ctx' = Map.insert x t1 ctx
  t2 <- constraintsWithCon ctx' e2
  return t2
constraintsWithCon _ Tz = do
  return Cnat
constraintsWithCon ctx (Ts en) = do
  tn <- constraintsWithCon ctx en
  constrain tn Cnat
  return Cnat
constraintsWithCon ctx (Tifz en e0 bnd) = do
  tn <- constraintsWithCon ctx en
  constrain tn Cnat
  -- Check then
  t0 <- constraintsWithCon ctx e0
  -- Prep predecessor for else
  (xpred, e1) <- unbind bnd
  tpred <- Cvar <$> fresh (s2n "tn_")
  constrain tpred Cnat
  -- Check else
  let ctx' = Map.insert xpred tpred ctx
  t1 <- constraintsWithCon ctx' e1
  constrain t0 t1
  -- Return type of 'zero' branch, because 'zero' and 'else' must be the same
  return t0
constraintsWithCon _ (Tbool _) = do
  return Cbool
constraintsWithCon ctx (Tif ei et ee) = do
  ti <- constraintsWithCon ctx ei
  tt <- constraintsWithCon ctx et
  te <- constraintsWithCon ctx ee
  constrain ti Cbool
  constrain tt te
  -- Return type of 'then' branch, because 'then' and 'else' must be the same
  return tt

constraints :: Term -> (Con, Seq.Seq Constraint)
constraints e = runWriter . runFreshMT $ constraintsWithCon Map.empty e
