module APL.AST
  ( VName
  , Exp (..)
  , printExp
  )
where

type VName = String

data Exp
  = CstInt Integer
  | CstBool Bool
  | Add Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp
  | Div Exp Exp
  | Pow Exp Exp
  | Eql Exp Exp
  | If Exp Exp Exp
  | Var VName
  | Let VName Exp Exp
  | Lambda VName Exp
  | Apply Exp Exp
  | TryCatch Exp Exp
  deriving (Eq, Show)

parens :: String -> String
parens x = "(" ++ x ++ ")"

printBinOp :: String -> Exp -> Exp -> String
printBinOp op x y = parens $ printExp x ++ " " ++ op ++ " " ++ printExp y

printExp :: Exp -> String
printExp (CstInt x) = show x
printExp (CstBool b) = if b then "true" else "false"
printExp (Add x y) = printBinOp "+" x y
printExp (Sub x y) = printBinOp "-" x y
printExp (Mul x y) = printBinOp "*" x y
printExp (Div x y) = printBinOp "/" x y
printExp (Pow x y) = printBinOp "**" x y
printExp (Eql x y) = printBinOp "==" x y
printExp (If x y z) =
  parens $
    "if "
      ++ printExp x
      ++ " then "
      ++ printExp y
      ++ " else "
      ++ printExp z
printExp (Var v) = v
printExp (Let v e1 e2) =
  parens $
    "let "
      ++ v
      ++ " = "
      ++ printExp e1
      ++ " in "
      ++ printExp e2
printExp (Lambda v body) =
  parens $ "\\" ++ v ++ " -> " ++ printExp body
printExp (Apply x y) =
  printExp x ++ " " ++ printExp y
printExp (TryCatch x y) =
  "try " ++ printExp x ++ " catch " ++ printExp y
