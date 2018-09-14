{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Monad.Reader

intFunc :: Int -> String
intFunc = do
  added <- (+5)
  times <- (*2)
  return $ show (added,times)

type FuncIO a b = a -> IO b

class Handleable m where
  toHandler :: m a -> ReaderT Int IO a 

--type family IOF a b where
  --IOF a b = a -> IO b
--type IOF a b = a -> IO b

instance Handleable (IOF Int) where
  toHandler x = ReaderT x

--instance Handleable a => Monad a


--instance MonadIO ((->) IO r) where
  --liftIO (IO a) = const a


--intReader :: FuncIO Int ()
--intReader = do
  --added <- return <$> (+5)
  --times <- return <$> (*2)
  --return (added,times)


--intReaderT :: ReaderT Int IO ()
--intReaderT = do
  --added <- (+5)
  --times <- (*2)
  --liftIO . putStrLn $ show (added,times)
  

main :: IO ()
--main = print $ intReader 1
main = print ""
