module MondayMorningHaskell where


import Control.Monad.Trans.Maybe
import Data.Char (isUpper, isLower)
import Control.Monad.IO.Class (MonadIO (liftIO))

-- https://mmhaskell.com/monads/transformers

readUserName' :: MaybeT IO String
readUserName' = MaybeT $ do
  putStrLn "Please enter your Username!"
  str <- getLine
  if length str > 5
    then return $ Just str
    else return Nothing

readEmail' :: MaybeT IO String
readEmail' = MaybeT $ do
  putStrLn "Please enter your Email!"
  str <- getLine
  pure $ if length str > 5
    then Just str
    else Nothing

readPassword' :: MaybeT IO String
readPassword' = MaybeT $ do
  putStrLn "Please enter your Password!"
  str <- getLine
  if length str < 8 || not (any isUpper str) || not (any isLower str)
    then return Nothing
    else return $ Just str

debug :: (MonadIO m) => String -> m ()
debug input = liftIO $ putStrLn ("Successfully produced input: " ++ input)

main3 :: IO ()
main3 = do
  maybeCreds <- runMaybeT $ do
    usr <- readUserName'
    debug usr
    email <- readEmail'
    debug email
    pass <- readPassword'
    debug pass
    return (usr, email, pass)
  case maybeCreds of
    Nothing -> print "Couldn't login!"
    Just (u, e, p) -> login u e p

login :: String -> String -> String -> IO ()
login = undefined


{-

instance MonadTrans MaybeT where
    lift = MaybeT . liftM Just
    {-# INLINE lift #-}

class MonadTrans t where
  lift :: (Monad m) => m a -> t m a

newtype MaybeTMy m a = MaybeTMy { runMaybeTMy :: m (Maybe a) }

instance (Monad m) => Monad (MaybeTMy m) where
  return = lift . return
  x >>= f = MaybeTMy $ do
    v <- runMaybeTMy x
    case v of
      Nothing -> return Nothing
      Just y  -> runMaybeTMy (f y)
-}