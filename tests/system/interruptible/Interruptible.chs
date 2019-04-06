{-# LANGUAGE InterruptibleFFI #-}
module Main where

import Control.Concurrent(killThread, forkIO)

main :: IO ()
main = do
  tid <- forkIO $ {#call interruptible run_forever#}
  killThread tid
  putStrLn "interrupted!"
