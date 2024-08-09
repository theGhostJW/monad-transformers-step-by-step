module Eval5 where

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
import Control.Monad.Writer
  ( MonadWriter (tell),
    WriterT (runWriterT),
  )
import Data.Function ()
import qualified Data.Map as Map
import Data.Maybe ()
import Lib (Exp (..), Value (..), Vars, exampleExp)

type Eval5 a =
  ReaderT
    Vars
    ( ExceptT
        String
        (WriterT [String] (StateT Integer Identity))
    )
    a

-- MOVE TO LIB
tick :: (Num s, MonadState s m) => m ()
tick = do
  st <- get
  put (st + 1)

runEval5 :: Vars -> Integer -> Eval5 a -> ((Either String a, [String]), Integer)
runEval5 env st ev =
  runIdentity (runStateT (runWriterT (runExceptT (runReaderT ev env))) st)

eval5 :: Exp -> Eval5 Value
eval5 = \case
  Lit i -> do
    tick
    pure $ IntVal i
  Var n -> do
    tick
    tell [n]
    env <- ask
    case Map.lookup n env of
      Nothing -> throwError ("unbound variable: " ++ n)
      Just val -> pure val
  Plus e1 e2 -> do
    tick
    e1' <- eval5 e1
    e2' <- eval5 e2
    case (e1', e2') of
      (IntVal i1, IntVal i2) ->
        pure $ IntVal (i1 + i2)
      _ -> throwError "type error in addition"
  Abs n e -> do
    tick
    env <- ask
    pure $ FunVal env n e
  App e1 e2 -> do
    tick
    val1 <- eval5 e1
    val2 <- eval5 e2
    case val1 of
      FunVal env' n body ->
        local (const (Map.insert n val2 env')) (eval5 body)
      _ -> throwError "type error in application"


execEval5 :: ((Either String Value, [String]), Integer)
execEval5 = runEval5 Map.empty 0 $ eval5 exampleExp
