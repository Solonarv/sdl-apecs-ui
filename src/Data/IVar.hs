module Data.IVar where

import           Control.Concurrent.MVar
import           Control.Exception
import           Data.Maybe

import           Control.DeepSeq

-- | A simple write-once variable
data IVar a = IVar { lock :: MVar (), val :: MVar a }

-- | Create a new, empty IVar
new :: IO (IVar a)
new = IVar <$> newMVar () <*> newEmptyMVar

-- | Create a new, filled IVar. The IVar's value will be fully evaluated first.
newFull :: NFData a => a -> IO (IVar a)
newFull a_ = do
  a <- evaluate $ force a_
  IVar <$> newMVar () <*> newMVar a

-- Create a new, filled IVar without evaluating its value.
newFull_ :: a -> IO (IVar a)
newFull_ a = IVar <$> newMVar () <*> newMVar a

-- | Get the value stored in an IVar. If there is no value, blocks until it's filled.
get :: IVar a -> IO a
get = takeMVar . val

-- | Get the value stored in an IVar, or Nothing if it's empty.
getMaybe :: IVar a -> IO (Maybe a)
getMaybe = tryTakeMVar . val

-- | Check if the given IVar is empty
isEmpty :: IVar a -> IO Bool
isEmpty = fmap isNothing . getMaybe

-- | The type of exception that is raised when attempting to write to a full IVar
data FilledIVarException = FilledIVarException deriving (Eq, Show, Typeable)
instance Exception FilledIVarException

-- | Put a value into an IVar. The value will be fully evaluated first.
-- If the IVar already contains a value, throws a @'FilledIVarException'@.
fill :: NFData a => IVar a -> a -> IO ()
fill var a_ = bracket
  (takeMVar $ lock var)
  (\_ -> isEmptyMVar (val var) >>= \case
      True -> do
        a <- evaluate $ force a_
        putMVar (val var) a
      False -> throwIO FilledIVarException)
  (putMVar $ lock var)

-- | Fill an IVar without evaluating its value.
-- If the IVar already contains a value, throws a @'FilledIVarException'@.
fill_ :: IVar a -> a -> IO ()
fill_ var a = bracket
  (takeMVar $ lock var)
  (\_ -> isEmptyMVar (val var) >>= \case
    True -> putMVar (val var) a
    False -> throwIO FilledIVarException)
  (putMVar $ lock var)
