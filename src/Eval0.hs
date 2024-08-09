module Eval0 where

import qualified Data.Map as Map
import Data.Maybe ( fromJust )
import Lib ( Vars, Value(..), Exp(..))

eval0 :: Vars -> Exp -> Value
eval0 env = \case
  Lit i -> IntVal i
  Var n -> fromJust (Map.lookup n env)
  Plus e1 e2 ->
    let IntVal i1 = eval0 env e1
        IntVal i2 = eval0 env e2
     in IntVal (i1 + i2)
  Abs n e -> FunVal env n e
  App e1 e2 ->
    let val1 = eval0 env e1
        val2 = eval0 env e2
     in case val1 of
          FunVal env' n body -> eval0 (Map.insert n val2 env') body


exampleExp0 :: Exp
exampleExp0 = Lit 12 `Plus` App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2)

execEval0 :: Value
execEval0 = eval0 Map.empty exampleExp0
