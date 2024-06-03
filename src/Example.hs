{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}

module Example where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.QSem
import Control.Monad (forever, replicateM_)
import Data.Foldable (for_)
import Pipes (MonadIO (liftIO), Pipe, await, each, runEffect, yield, (>->))
import Pipes.Concurrent (bounded, fromInput, performGC, spawn, toOutput)
import qualified Pipes.Prelude as P

main' = do
  qSem <- newQSem 3
  for_ [1 .. 9] \n -> do
    _ <- forkIO do
      threadDelay 200_000
      print n
      signalQSem qSem
    waitQSem qSem

main :: IO ()
main = do
  (output, input) <- spawn (bounded 10)

  runEffect do
    each [1 .. 9]
      >-> foo
      >-> P.stdoutLn

foo :: Pipe Int String IO ()
foo = forever do
  n <- await
  _ <- liftIO $ forkIO do
    threadDelay 2_000_000
    putStrLn ("from forked thread: " <> show n)
  yield (show n)

fooC :: Pipe Int String IO ()
fooC = forever do
  n <- await
  liftIO (threadDelay 2_000_000)
  yield (show n)

main'' :: IO ()
main'' = do
  (output, input) <- spawn (bounded 3)
  x <- async do
    runEffect do
      each [1 .. 9] >-> toOutput output
    performGC

  as <- async do
    runEffect do
      fromInput input
        >-> replicateM_ 3 fooC
        >-> P.stdoutLn

  wait x
  wait as
