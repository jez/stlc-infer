{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Stlc.Types
  ( Tvar
  , Cvar
  , Con(..)
  , Term(..)
  , Constraint(..)
  , bothConstraint
  , ExplSubst(..)
  ) where

import           Data.Typeable                    (Typeable)
import           GHC.Generics                     (Generic)
import           Text.Show                        (Show)
import           Unbound.Generics.LocallyNameless

-- | We don't have type variables yet, so we're going to let Var mean expression
--   variables
type Tvar = Name Term

type Cvar = Name Con

-- | Our ABT for simply-typed lambda calculus terms, with numbers and booleans.
--   Note that we don't need abbot for code generation!
--   All the code generation we need is built into the compiler.
--
--   We're going to use @Con@ instead of @Typ@ for "type constructors" even
--   though we don't have them in the language yet, because Term and Type start
--   with the same letter.
data Con
  = Cvar Cvar
  | Carrow Con
           Con
  | Cnat
  | Cbool
  deriving (Show, Generic, Typeable)

data Term
  = Tvar Tvar
  | Tlam (Bind Tvar Term)
  | Tapp Term
         Term
  | Tlet Term
         (Bind Tvar Term)
  | Tz
  | Ts Term
  -- | Tprec Term Term (Bind (Tvar, Tvar) Term)
  | Tifz Term
         Term
         (Bind Tvar Term)
  | Tbool Bool
  | Tif Term
        Term
        Term
  deriving (Show, Generic, Typeable)

-- Compiler automatically derives alpha equivalence!
instance Alpha Con

instance Alpha Term

-- | Tell which Exps are variables, so that the compiler can derive
--   capture-avoiding substitution for us!
--
--   By default, the inferred instance assumes that there are no variables in
--   the language. Since that's not the case for Term, we have to implement it.
--   Also: @Subst b a@ is for substituting a @b@ into an @a@.
--
--   Since we can't add type annotations to our terms yet, we only have two
--   subst instances.
instance Subst Con Con where
  isvar (Cvar x) = Just (SubstName x)
  isvar _        = Nothing

instance Subst Term Term where
  isvar (Tvar x) = Just (SubstName x)
  isvar _        = Nothing

-- A constraint is just a pair of types.
-- A constraints records the fact that two types must be equal.
data Constraint =
  Constraint Con
             Con
  deriving (Show)

-- A hand-written Traversal'. It just runs a function over both the Con's of a
-- constraint, and collects their result inside a functor.
-- Alternatively, we could have the compiler generate this with 'makeLenses'
bothConstraint :: Applicative f => (Con -> f Con) -> Constraint -> f Constraint
bothConstraint f (Constraint t1 t2) = Constraint <$> f t1 <*> f t2

-- Substitutions reified as a data structure, rather
-- than being implicit in the algorithm somewhere.
-- Maps variables of one sort to ABTs of that sort.
data ExplSubst a =
  ExplSubst (Name a)
                a
  deriving (Show)
