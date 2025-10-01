module APL.Error
  ( Error(..)
  , isVariableError
  , isDomainError
  , isTypeError
  )
where

import APL.AST (VName)

data Error
  = NonInteger
  | UnknownVariable VName
  | DivisionByZero
  | NegativeExponent
  | InvalidEqual
  | NonBoolean
  | NonFunction
  deriving (Show, Eq)

isVariableError :: Error -> Bool
isVariableError (UnknownVariable _) = True
isVariableError _ = False

isDomainError :: Error -> Bool
isDomainError DivisionByZero = True
isDomainError NegativeExponent = True
isDomainError _ = False

isTypeError :: Error -> Bool
isTypeError NonInteger = True
isTypeError InvalidEqual = True
isTypeError NonBoolean = True
isTypeError NonFunction = True
isTypeError _ = False
