

module Eval4 where

import Control.Monad.Except
  ( ExceptT,
    MonadError (throwError),
    runExceptT,
  )
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader
  ( MonadReader (ask, local),
    ReaderT (runReaderT),
  )
import Control.Monad.State
  ( MonadState (get, put),
    StateT (runStateT),
  )
import qualified Data.Map as Map
import Data.Maybe
import Lib (Exp (..), Value (..), Vars)

type Eval4 a = ReaderT Vars (ExceptT String (StateT Integer Identity)) a

runEval4 :: Vars -> Integer -> Eval4 a -> (Either String a, Integer)
runEval4 vars st ev = runIdentity (runStateT (runExceptT (runReaderT ev vars)) st)

tick :: (Num s, MonadState s m) => m ()
tick = do
  st <- get
  put (st + 1)

eval4 :: Exp -> Eval4 Value
eval4 = \case
  Lit i -> do
    tick
    pure $ IntVal i
  Var n -> do
    tick
    vars <- ask
    case Map.lookup n vars of
      Nothing -> throwError ("unbound variable: " ++ n)
      Just val -> pure val
  Plus e1 e2 -> do
    tick
    e1' <- eval4 e1
    e2' <- eval4 e2
    case (e1', e2') of
      (IntVal i1, IntVal i2) ->
        pure $ IntVal (i1 + i2)
      _ -> throwError "type error in addition"
  Abs n e -> do
    tick
    vars <- ask
    pure $ FunVal vars n e
  App e1 e2 -> do
    tick
    val1 <- eval4 e1
    val2 <- eval4 e2
    case val1 of
      FunVal vars' n body ->
        local (const (Map.insert n val2 vars')) (eval4 body)
      _ -> throwError "type error in application"
