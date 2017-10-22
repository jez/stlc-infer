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
  , ExplicitSubst(..)
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

-- A constraint is just a pair type types.
-- A constraints records the fact that two types must be equal.
newtype Constraint =
  Constraint (Con, Con)
  deriving (Show)

-- Substitutions reified as a datastructure, rather
-- than being implicit in the algorithm somewhere.
-- Maps variables of one sort to ABTs of that sort.
newtype ExplicitSubst a =
  ExplicitSubst (Name a, a)
  deriving (Show)
