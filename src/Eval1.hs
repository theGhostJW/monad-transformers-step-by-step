module Eval1 where

import Control.Monad.Except
import Control.Monad.Identity
import qualified Data.Map as Map
import Data.Maybe
import Lib

-- converted to monadic style
type Eval1 a = Identity a

runEval1 :: Eval1 a -> a
runEval1 = runIdentity

-- `fail` replaced by `error` per https://downloads.haskell.org/~ghc/8.8.1/docs/html/users_guide/8.8.1-notes.html
-- eval1 :: Vars -> Exp -> Eval1 Value
eval1 :: (Monad m, MonadFail m) => Vars -> Exp -> m Value
eval1 vars = \case
  Lit i -> pure $ IntVal i
  Var n -> maybe (error $ "undefined variable: " <> n) pure $ Map.lookup n vars
  Plus e1 e2 -> do
    IntVal i1 <- eval1' e1
    IntVal i2 <- eval1' e2
    pure $ IntVal (i1 + i2)
  Abs n e -> pure $ FunVal vars n e
  App e1 e2 -> do
    val1 <- eval1' e1
    val2 <- eval1' e2
    case val1 of
      FunVal env' n body -> eval1 (Map.insert n val2 env') body
  where
    eval1' = eval1 vars

execEval1 :: Value
execEval1 = runEval1 $ eval1 Map.empty exampleExp
