module Worker where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Monad
import Pipes
import Pipes.Concurrent
import qualified Pipes.Prelude as P

main = do
  (output, input) <- spawn unbounded
  as <- forM [1 .. 3] $ \i ->
    async $ do
      runEffect $ fromInput input >-> worker i
      performGC
  a <- async $ do
    runEffect $ each [1 .. 10] >-> toOutput output
    performGC
  mapM_ wait (a : as)

worker :: (Show a) => Int -> Consumer a IO r
worker i = forever $ do
  a <- await
  lift $ threadDelay 1000000 -- 1 second
  lift $ putStrLn $ "Worker #" ++ show i ++ ": Processed " ++ show a