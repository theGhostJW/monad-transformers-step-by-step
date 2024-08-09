module Eval6 where

import Control.Monad.Except
    ( ExceptT, MonadError(throwError), runExceptT )
import Control.Monad.Identity ()
import Control.Monad.Reader
    ( MonadIO(liftIO), ReaderT(runReaderT), MonadReader(local, ask) )
import Control.Monad.State
    ( MonadIO(liftIO), StateT(runStateT), MonadState(put, get) )
import Control.Monad.Writer
    ( MonadIO(liftIO), WriterT(runWriterT), MonadWriter(tell) )
import Data.Function ()
import qualified Data.Map               as Map
import Data.Maybe ()

import Lib ( Value(..), Exp(..), Vars, exampleExp )

type Eval a = ReaderT Vars (ExceptT String
                          (WriterT [String] (StateT Integer IO))) a

runEval6 :: Vars -> Integer -> Eval a -> IO ((Either String a, [String]), Integer)
runEval6 env st ev =
    runStateT (runWriterT $ runExceptT $ runReaderT ev env) st

tick :: (Num s, MonadState s m) => m ()
tick = do st <- get
          put (st + 1)


eval6 :: Exp -> Eval Value
eval6 = \case
  Lit i -> do
    tick
    liftIO $ print i
    pure $ IntVal i
  Var n -> do
    tick
    tell [n]
    env <- ask
    case Map.lookup n env of
      Just n' -> pure n'
      Nothing -> throwError ("undefined variable: " ++ n)
  Plus a b -> do
    tick
    ia <- eval6 a
    ib <- eval6 b
    case (ia, ib) of
      (IntVal a', IntVal b') -> pure $ IntVal (a' + b')
      _                      -> throwError "type error in addition"
  Abs n e -> do
    tick
    env <- ask
    pure $ FunVal env n e
  App a b -> do
    tick
    a' <- eval6 a
    b' <- eval6 b
    case a' of
      FunVal env' n body -> local (const $ Map.insert n b' env') (eval6 body)
      _                  -> throwError "type error in application"

execEval6 :: IO ((Either String Value, [String]), Integer)
execEval6 = runEval6 Map.empty 0 $ eval6 exampleExp
