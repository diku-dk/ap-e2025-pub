module APL.Check (checkExp, Error) where

import APL.AST (Exp (..), VName)
import APL.Error (Error (..))
import Control.Monad (ap, liftM, unless)
import Data.List (union)

type Vars = [VName]

newtype CheckM a = CheckM {runCheckM :: Vars -> (a, [Error])}

instance Functor CheckM where
  fmap = liftM

instance Applicative CheckM where
  (<*>) = ap
  pure x = CheckM $ \_ -> (x, [])

instance Monad CheckM where
  CheckM x >>= f = CheckM $ \vars ->
    let (y, errs1) = x vars
        (z, errs2) = runCheckM (f y) vars
    in (z, union errs1 errs2)

askVars :: CheckM Vars
askVars = CheckM $ \vars -> (vars, [])

localVars :: (Vars -> Vars) -> CheckM a -> CheckM a
localVars f m = CheckM $ \vars ->
  runCheckM m (f vars)

failure :: Error -> CheckM ()
failure err = CheckM $ \_ -> ((), [err])

maskErrors :: CheckM a -> CheckM a
maskErrors m = CheckM $ \vars ->
  let (x, _) = runCheckM m vars in (x, [])

check :: Exp -> CheckM ()
check (CstInt _) = pure ()
check (CstBool _) = pure ()
check (Var v) = do
  vars <- askVars
  unless (v `elem` vars) $
    failure $
      UnknownVariable v
check (Add x y) = do
  failure NonInteger
  check x
  check y
check (Sub x y) = do
  failure NonInteger
  check x
  check y
check (Mul x y) = do
  failure NonInteger
  check x
  check y
check (Div x y) = do
  failure NonInteger
  failure DivisionByZero
  check x
  check y
check (Pow x y) = do
  failure NonInteger
  failure NegativeExponent
  check x
  check y
check (Eql x y) = do
  failure InvalidEqual
  check x
  check y
check (If x y z) = do
  failure NonBoolean
  check x
  check y
  check z
check (Let v e1 e2) = do
  check e1
  localVars (v :) $ check e2
check (Lambda v e) = do
  localVars (v :) $ check e
check (Apply x y) = do
  failure NonFunction
  check x
  check y
check (TryCatch x y) = do
  maskErrors $ check x
  check y

checkExp :: Exp -> [Error]
checkExp e = snd $ runCheckM (check e) []
