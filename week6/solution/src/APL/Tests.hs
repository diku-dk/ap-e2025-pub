module APL.Tests where

import APL.AST (Exp (..), VName)
import APL.Eval (eval, runEval)
import Test.QuickCheck
  ( Gen
  , Arbitrary (arbitrary, shrink)
  , elements
  , listOf
  , oneof
  , sized
  )

genVar :: Gen VName
genVar = do
    alpha <- elements ['a' .. 'z']
    alphaNums <- listOf $ elements $ ['a' .. 'z'] ++ ['0' .. '9']
    pure (alpha : alphaNums)

genExp :: Int -> Gen Exp
genExp size
  | size <= 1 = oneof [CstInt <$> arbitrary, CstBool <$> arbitrary]
  | otherwise =
    oneof
      [ CstInt <$> arbitrary
      , CstBool <$> arbitrary
      , Add <$> genExp halfSize <*> genExp halfSize
      , Sub <$> genExp halfSize <*> genExp halfSize
      , Mul <$> genExp halfSize <*> genExp halfSize
      , Div <$> genExp halfSize <*> genExp halfSize
      , Pow <$> genExp halfSize <*> genExp halfSize
      , Eql <$> genExp halfSize <*> genExp halfSize
      , If <$> genExp thirdSize <*> genExp thirdSize <*> genExp thirdSize
      , Var <$> genVar
      , Let <$> genVar <*> genExp halfSize <*> genExp halfSize
      , Lambda <$> genVar <*> genExp (size - 1)
      , Apply <$> genExp halfSize <*> genExp halfSize 
      , TryCatch <$> genExp halfSize <*> genExp halfSize
      ]
    where
      halfSize = size `div` 2
      thirdSize = size `div` 3

prop_integerAddAssoc :: Integer -> Integer -> Integer -> Bool
prop_integerAddAssoc a b c = (a + b) + c == a + (b + c)

prop_aplAddAssoc :: Exp -> Exp -> Exp -> Bool
prop_aplAddAssoc e1 e2 e3 = runEval (eval (Add (Add e1 e2) e3)) == runEval (eval (Add e1 (Add e2 e3)))

instance Arbitrary Exp where
  arbitrary = sized genExp

  shrink (CstBool _) = []
  shrink (CstInt k) = [CstInt k' | k' <- shrink k]
  shrink (Add e1 e2) =
    e1 : e2 : [Add e1' e2 | e1' <- shrink e1] ++ [Add e1 e2' | e2' <- shrink e2]
  shrink (Sub e1 e2) =
    e1 : e2 : [Sub e1' e2 | e1' <- shrink e1] ++ [Sub e1 e2' | e2' <- shrink e2]
  shrink (Mul e1 e2) =
    e1 : e2 : [Mul e1' e2 | e1' <- shrink e1] ++ [Mul e1 e2' | e2' <- shrink e2]
  shrink (Div e1 e2) =
    e1 : e2 : [Div e1' e2 | e1' <- shrink e1] ++ [Div e1 e2' | e2' <- shrink e2]
  shrink (Pow e1 e2) =
    e1 : e2 : [Pow e1' e2 | e1' <- shrink e1] ++ [Pow e1 e2' | e2' <- shrink e2]
  shrink (Eql e1 e2) =
    e1 : e2 : [Eql e1' e2 | e1' <- shrink e1] ++ [Eql e1 e2' | e2' <- shrink e2]
  shrink (If cond e1 e2) =
    e1 : e2 : [If cond' e1 e2 | cond' <- shrink cond] ++ [If cond e1' e2 | e1' <- shrink e1] ++ [If cond e1 e2' | e2' <- shrink e2]
  shrink (Var x) =
    [Var x' | x' <- shrink x, not (null x')]
  shrink (Let x e1 e2) =
    e1 : [Let x e1' e2 | e1' <- shrink e1] ++ [Let x e1 e2' | e2' <- shrink e2]
  shrink (Lambda x e) =
    e : [Lambda x e' | e' <- shrink e]
  shrink (Apply e1 e2) =
    e1 : e2 : [Apply e1' e2 | e1' <- shrink e1] ++ [Apply e1 e2' | e2' <- shrink e2]
  shrink (TryCatch e1 e2) =
    e1 : e2 : [TryCatch e1' e2 | e1' <- shrink e1] ++ [TryCatch e1 e2' | e2' <- shrink e2]
