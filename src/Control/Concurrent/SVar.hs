-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.SVar
-- Copyright   :  (c) 2018 Daniel Taskoff
-- License     :  MIT
--
-- Maintainer  :  daniel.taskoff@gmail.com, godzbanebane@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module Control.Concurrent.SVar
  (
  -- * 'SVar'
    SVar
  -- * Creation of 'SVar's
  , newEmptySVar, newSVar
  -- * Modification of 'SVar's
  , putSVar, readSVar
  ) where

import Control.Concurrent.MVar
  ( MVar, newEmptyMVar, newMVar
  , putMVar, takeMVar
  )


-- | An 'SVar' (skip variable) is a variable which allows for non-blocking updates,
-- and blocking reads if the stored data has been read already, or if there is no data.
data SVar a = SVar (MVar (a, Maybe (MVar ()))) (MVar ())

-- | Create an empty 'SVar'.
newEmptySVar :: IO (SVar a)
newEmptySVar = do
  lock <- newEmptyMVar
  var <- newMVar (undefined, Just lock)
  -- ^ lock the 'undefined' value,
  -- and notify the writer to unlock the next value written
  pure (SVar var lock)

-- | Create an 'SVar' which contains the supplied value.
newSVar :: a -> IO (SVar a)
newSVar value = do
  lock <- newMVar ()
  var <- newMVar (value, Nothing)
  -- ^ keep the value unlocked for reading
  pure (SVar var lock)

-- | Put a value into an 'SVar'.
-- Never blocks, always overwrites the current value, if there is one.
putSVar :: SVar a -> a -> IO ()
putSVar (SVar var _) value = do
  (_, mlock) <- takeMVar var
  putMVar var (value, Nothing)

  mapM_ (\lock -> putMVar lock ()) mlock

-- | Read a value from an 'SVar'.
-- Blocks if there isn't a new value since the last read, or if the 'SVar' is empty.
readSVar :: SVar a -> IO a
readSVar (SVar var lock) = do
  takeMVar lock

  (value, _) <- takeMVar var
  putMVar var (value, Just lock)
  -- ^ keep the value unchanged,
  -- and notify the writer to unlock the next value written

  pure value
