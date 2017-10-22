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
constrain t1 t2 = lift . tell . Seq.singleton $ Constraint (t1, t2)

-- Implements the judgement:
--
--     Γ ⊢F e : τ |F' C
--
-- by threading F through the FreshMT monad, and
-- collecting the constraints C using a Writer monad.
genConstraints :: Ctx -> Term -> FreshMT (Writer (Seq.Seq Constraint)) Con
genConstraints ctx (Tvar x) = return $ ctx ! x
genConstraints ctx (Tlam bnd) = do
  -- 'out' the ABT
  (x, e) <- unbind bnd
  -- Generate fresh type variable to put into the context
  t1 <- Cvar <$> fresh (s2n "t1_")
  let ctx' = Map.insert x t1 ctx
  t2 <- genConstraints ctx' e
  return $ Carrow t1 t2
genConstraints ctx (Tapp e1 e2) = do
  t1 <- genConstraints ctx e1
  t2 <- genConstraints ctx e2
  t <- Cvar <$> fresh (s2n "t_")
  constrain t1 (Carrow t2 t)
  return t
genConstraints ctx (Tlet e1 bnd) = do
  (x, e2) <- unbind bnd
  t1 <- genConstraints ctx e1
  let ctx' = Map.insert x t1 ctx
  t2 <- genConstraints ctx' e2
  return t2
genConstraints _ Tz = do
  return Cnat
genConstraints ctx (Ts en) = do
  tn <- genConstraints ctx en
  constrain tn Cnat
  return Cnat
genConstraints ctx (Tifz en e0 bnd) = do
  tn <- genConstraints ctx en
  constrain tn Cnat
  -- Check then
  t0 <- genConstraints ctx e0
  -- Prep predecessor for else
  (xpred, e1) <- unbind bnd
  tpred <- Cvar <$> fresh (s2n "tn_")
  constrain tpred Cnat
  -- Check else
  let ctx' = Map.insert xpred tpred ctx
  t1 <- genConstraints ctx' e1
  constrain t0 t1
  -- Return type of 'zero' branch, because 'zero' and 'else' must be the same
  return t0
genConstraints _ (Tbool _) = do
  return Cbool
genConstraints ctx (Tif ei et ee) = do
  ti <- genConstraints ctx ei
  tt <- genConstraints ctx et
  te <- genConstraints ctx ee
  constrain ti Cbool
  constrain tt te
  -- Return type of 'then' branch, because 'then' and 'else' must be the same
  return tt

constraints :: Term -> Seq.Seq Constraint
constraints e = execWriter . runFreshMT $ genConstraints Map.empty e
