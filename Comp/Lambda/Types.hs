module Comp.Lambda.Types (
  Var,
  LambdaTerm(..),
  ($$),
  (>\),
  ReduKind(..)
) where

type Var = String
data LambdaTerm = V Var | Lambda Var LambdaTerm | App LambdaTerm LambdaTerm
  deriving (Show, Read, Eq, Ord)

infixl 1 $$
($$) :: LambdaTerm -> LambdaTerm -> LambdaTerm
f $$ x = App f x

infixr 1 >\
(>\) :: Var -> LambdaTerm -> LambdaTerm
x >\ l = Lambda x l

data ReduKind = DoApp | ReduBody | ReduLeft | ReduRight
  deriving (Eq, Ord)

instance Show ReduKind where
  show DoApp = "app"
  show ReduBody = "\\_.()"
  show ReduLeft = "()_"
  show ReduRight = "_()"