module APL.Tests where

import APL.AST (Exp (..), VName)
import Test.QuickCheck (Gen)

genVar :: Gen VName
genVar = undefined

genExp :: Gen Exp
genExp = undefined
