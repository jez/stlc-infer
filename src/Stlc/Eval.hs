{-# LANGUAGE LambdaCase #-}

module Stlc.Eval where

import           Stlc.Types

import           Unbound.Generics.LocallyNameless

-- TODO(jez) I have a feeling we can be a lot more concise here.
trystep :: Term -> FreshM (Maybe Term)
trystep (Tvar x)         = error $ "Unbound variable: " ++ show x
trystep (Tlam _)         = return Nothing
trystep (Tapp e1 e2)     = do
  trystep e1 >>= \case
    Just e1' -> return . Just $ Tapp e1' e2
    Nothing -> do
      trystep e2 >>= \case
        Just e2' -> return . Just $ Tapp e1 e2'
        Nothing -> do
          case e1 of
            Tlam bnd -> do
              (x, ebody) <- unbind bnd
              return . Just $ subst x e2 ebody
            _ -> error "Expected a function. This is a bug in the type checker."
trystep (Tlet e1 bnd)    =
  trystep e1 >>= \case
    Just e1' -> return . Just $ Tlet e1' bnd
    Nothing -> do
     (x, e2) <- unbind bnd
     return . Just $ subst x e1 e2
trystep Tz               = return Nothing
trystep (Ts e)           = trystep e >>= (return . fmap Ts)
trystep (Tifz ei et bnd) = do
  trystep ei >>= \case
    Just ei' -> return . Just $ Tifz ei' et bnd
    Nothing -> do
      case ei of
        Tz -> return $ Just et
        Ts en -> do
          (x, ee) <- unbind bnd
          return . Just $ subst x en ee
        _ -> error "Expected a nat. This is a bug in the type checker."
trystep (Tbool _)        = return Nothing
trystep (Tif ei et ee)   = do
  trystep ei >>= \case
    Just ei' -> return . Just $ Tif ei' et ee
    Nothing -> do
      case ei of
        Tbool True -> return $ Just et
        Tbool False -> return $ Just ee
        _ -> error "Expected a bool. This is a bug in the type checker."

eval :: Term -> FreshM Term
eval e =
  trystep e >>= \case
    Nothing -> return e
    Just e' -> eval e'

runEval :: Term -> Term
runEval = runFreshM . eval
