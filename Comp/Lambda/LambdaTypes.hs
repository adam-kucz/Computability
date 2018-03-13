module LambdaTypes (
  Var,
  LambdaTerm(..),
  ($$)
) where

type Var = String
data LambdaTerm = V Var | Lambda Var LambdaTerm | App LambdaTerm LambdaTerm
  deriving (Show, Read, Eq)

infixl 1 $$
($$) :: LambdaTerm -> LambdaTerm -> LambdaTerm

f $$ x = App f x