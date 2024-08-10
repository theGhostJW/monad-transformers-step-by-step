module Eval3 where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import qualified Data.Map as Map
import Data.Maybe
import Lib

type Eval3 a = ReaderT Vars (ExceptT String Identity) a

runEval3 :: Vars -> Eval3 a -> Either String a
runEval3 vars evalWithReaderExcept = runIdentity (runExceptT (runReaderT evalWithReaderExcept vars))

eval3 :: Exp -> Eval3 Value
eval3 = \case
  Lit i -> pure $ IntVal i
  Var n -> do
    vars <- ask
    case Map.lookup n vars of
      Nothing -> throwError ("unbound variable: " ++ n)
      Just val -> pure val
  Plus e1 e2 -> do
    e1' <- eval3 e1
    e2' <- eval3 e2
    case (e1', e2') of
      (IntVal i1, IntVal i2) ->
        pure $ IntVal (i1 + i2)
      _ -> throwError "type error in addition"
  Abs n e -> do
    vars <- ask
    pure $ FunVal vars n e
  App e1 e2 -> do
    val1 <- eval3 e1
    val2 <- eval3 e2
    case val1 of
      FunVal vars' n body ->
        local (const (Map.insert n val2 vars')) (eval3 body)
      _ -> throwError "type error in application"

execEval3 :: Either String Value
execEval3 = runEval3 Map.empty $ eval3 exampleExp