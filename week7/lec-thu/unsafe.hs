module Unsafe where

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

ref :: IORef [a]
ref = unsafePerformIO $ newIORef []

main :: IO ()
main = do
  writeIORef (ref :: IORef [Bool]) [True]
  x <- readIORef (ref :: IORef [String])
  putStrLn (head x)
