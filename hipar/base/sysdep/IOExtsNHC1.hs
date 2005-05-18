-- This is a preliminary `IOExts' substitute for nhc provided by 
-- Malcolm Wallace <Malcolm.Wallace@cs.york.ac.uk>

module IOExtsNHC1
  ( IORef
  , newIORef
  , readIORef
  , writeIORef
  , fixIO
  ) where

import FFI

newtype IORef a = IORef (StablePtr a)

newIORef :: a -> IO (IORef a)
newIORef a = makeStablePtr a >>= return . IORef

readIORef :: IORef a -> IO a
readIORef (IORef r) = deRefStablePtr r

foreign import "stableCopy" 
  overwriteStablePtr :: StablePtr a -> StablePtr a -> IO ()

writeIORef :: IORef a -> a -> IO ()
writeIORef (IORef r) a = do
    s <- makeStablePtr a
    overwriteStablePtr r s
    freeStablePtr s

fixIO :: (a -> IO a) -> IO a
fixIO f = let x = unsafePerformIO (f x) in return x
