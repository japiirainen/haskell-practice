{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Tf where
        
type UserName = String

data DataResult = DataResult String
  deriving (Eq, Show)
        
class Monad m => Cache m where
  getFromCache :: String -> m (Maybe [DataResult])
  storeCache :: [DataResult] -> m ()

class Monad m => DataSource m where
  getFromDataSource :: String -> m [DataResult]

class Monad m => Logging m where
  logMsg :: String -> m ()

newtype NotInCache a = NotInCache { unNoCache :: IO a }
  deriving (Functor, Applicative, Monad)

instance Cache NotInCache where
  getFromCache _ = NotInCache $ return Nothing
  storeCache _   = NotInCache $ return ()

instance DataSource NotInCache where
  getFromDataSource user = return $ [DataResult $ " - source: " <> user]

instance Logging NotInCache where
  logMsg = NotInCache . putStrLn

newtype InCache a = InCache { unInCache :: IO a }
  deriving (Functor, Applicative, Monad)

instance Cache InCache where
  getFromCache user = InCache $ return $ Just [DataResult $ " - data: " <> user]
  storeCache _      = InCache $ return ()

instance DataSource InCache where
  getFromDataSource _ = InCache $ return []

instance Logging InCache where
  logMsg = InCache . putStrLn

requestData :: (Cache m, DataSource m, Logging m) => UserName -> m [DataResult]
requestData userName = do
  cache <- getFromCache userName
  result <- case cache of
    Just dataResult -> return dataResult
    Nothing         -> getFromDataSource userName
  storeCache result
  logMsg $ "Result data for user: " <> userName <> " - data: " <> show result
  return result


main :: IO ()
main = do
  (unNoCache $ requestData "joona") >>= (putStrLn . show)
  (unInCache $ requestData "joona") >>= (putStrLn . show)

