import Data.IORef

type Counter = Int -> IO Int

makeCounter :: IO Counter
makeCounter = do r <- newIORef 0
                 return (\i -> do modifyIORef r (+i) 
                                  readIORef r)

testCounter :: Counter -> IO ()
testCounter counter = do b <- counter 1
                         c <- counter 1
                         d <- counter 1
                         print [b,c,d]