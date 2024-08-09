

-- `fail` replaced by `error` per https://downloads.haskell.org/~ghc/8.8.1/docs/html/users_guide/8.8.1-notes.html

module Eval2 where

import Control.Monad.Except
  ( ExceptT,
    MonadError (throwError),
    runExceptT,
  )
import Control.Monad.Identity (Identity (runIdentity))
import qualified Data.Map as Map
import Lib (Exp (..), Value (..), Vars, exampleExp)

type Eval2 a = ExceptT String Identity a

runEval2 :: Eval2 a -> Either String a
runEval2 ev = runIdentity (runExceptT ev)

eval2a :: Vars -> Exp -> Eval2 Value
eval2a vars = \case
  Lit i -> pure $ IntVal i
  Var n -> maybe (error ("undefined variable: " ++ n)) pure $ Map.lookup n vars
  Plus e1 e2 -> do
    IntVal i1 <- eval2a vars e1
    IntVal i2 <- eval2a vars e2
    pure $ IntVal (i1 + i2)
  Abs n e -> pure $ FunVal vars n e
  App e1 e2 -> do
    val1 <- eval2a vars e1
    val2 <- eval2a vars e2
    case val1 of
      FunVal vars' n body ->
        eval2a (Map.insert n val2 vars') body

eval2b :: Vars -> Exp -> Eval2 Value
eval2b vars = \case
  Lit i -> pure $ IntVal i
  Var n -> maybe (error ("undefined variable: " ++ n)) pure $ Map.lookup n vars
  Plus e1 e2 -> do
    e1' <- eval2b vars e1
    e2' <- eval2b vars e2
    case (e1', e2') of
      (IntVal i1, IntVal i2) ->
        pure $ IntVal (i1 + i2)
      _ -> throwError "type error"
  Abs n e -> pure $ FunVal vars n e
  App e1 e2 -> do
    val1 <- eval2b vars e1
    val2 <- eval2b vars e2
    case val1 of
      FunVal vars' n body ->
        eval2b (Map.insert n val2 vars') body
      _ -> throwError "type error"

eval2c :: Vars -> Exp -> Eval2 Value
eval2c vars = \case
  Lit i -> pure $ IntVal i
  Var n -> maybe (error ("undefined variable: " ++ n)) pure $ Map.lookup n vars
  Plus e1 e2 -> do
    IntVal i1 <- eval2c vars e1
    IntVal i2 <- eval2c vars e2
    pure $ IntVal (i1 + i2)
  Abs n e -> pure $ FunVal vars n e
  App e1 e2 -> do
    FunVal vars' n body <- eval2c vars e1
    val2 <- eval2c vars e2
    eval2c (Map.insert n val2 vars') body

eval2 :: Vars -> Exp -> Eval2 Value
eval2 vars = \case
  Lit i -> pure $ IntVal i
  Var n -> case Map.lookup n vars of
    Nothing -> throwError ("unbound variable: " ++ n)
    Just val -> pure val
  Plus e1 e2 -> do
    e1' <- eval2 vars e1
    e2' <- eval2 vars e2
    case (e1', e2') of
      (IntVal i1, IntVal i2) ->
        pure $ IntVal (i1 + i2)
      _ -> throwError "type error in addition"
  Abs n e -> pure $ FunVal vars n e
  App e1 e2 -> do
    val1 <- eval2 vars e1
    val2 <- eval2 vars e2
    case val1 of
      FunVal vars' n body ->
        eval2 (Map.insert n val2 vars') body
      _ -> throwError "type error in application"

-- >>> execEval2a
-- Right (IntVal 18)
execEval2a :: Either String Value
execEval2a = runEval2 (eval2a Map.empty exampleExp)

-- >>> execEval2b
-- Right (IntVal 18)
execEval2b :: Either String Value
execEval2b = runEval2 (eval2b Map.empty exampleExp)

-- >>> execEval2c
-- Right (IntVal 18)
execEval2c :: Either String Value
execEval2c = runEval2 (eval2c Map.empty exampleExp)

-- >>> execEval2
-- Right (IntVal 18)
execEval2 :: Either String Value
execEval2 = runEval2 (eval2 Map.empty exampleExp)

-- >>> execEvalErr
-- Left "type error"
execEvalErr :: Either String Value
execEvalErr = runEval2 (eval2b Map.empty (Plus (Lit 1) (Abs "x" (Var "x"))))
-- >>> execEvalErr2
-- Left "type error"
execEvalErr2 :: Either String Value
execEvalErr2 = runEval2 (eval2c Map.empty (Plus (Lit 1) (Abs "x" (Var "x"))))
