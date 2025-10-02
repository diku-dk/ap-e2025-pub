-- Introduction to concurrency using Haskell 
import Control.Monad (forM_, void)

import Control.Concurrent
  ( Chan,       -- :: Type -> Type             FIFO message queues (channels)
    ThreadId,   -- :: Type                     thread ids
    forkIO,     -- :: IO () -> IO ThreadId     nonblocking fork of new thread
    newChan,    -- :: IO (Chan a)              generate new message queue with elements of type a
    readChan,   -- :: Chan a -> IO a           blocking read from message queue
    writeChan,  -- :: Chan a -> a -> IO ()     nonblocking write to message queue
    killThread, -- :: ThreadId -> IO ()        kill thread with given thread id
    threadDelay -- :: Int -> IO ()             dealy thread for given number of microseconds
  )

runThread :: IO ()
runThread = do
  t <- forkIO $ putStrLn "Hello there."
  print t


























-- Message logger
messagePrinter1Example :: IO ()
messagePrinter1Example = do
  c <- newChan                  -- create new channel
  forkIO $ do                   -- fork new thread:
    r <- readChan c             --      read string s from channel
    putStrLn $                  --      print "Received message: " ++ s
      "Received message: " <> r --      end of thread
  writeChan c "Hello there."    -- write "Hello there." to channel

messagePrinterExample :: IO ()
messagePrinterExample = do
  c <- newChan
  let readPrintLoop = do 
        r <- readChan c
        putStrLn $ "Received message: " <> r
        readPrintLoop
  forkIO readPrintLoop
  writeChan c "The first"
  writeChan c "The second"
  writeChan c "The third"























-- request-reply messages for a->b server functions
data MsgG a b = MsgG a (Chan b)
  
-- server loop listening on given request channel
loop :: (a -> b) -> Chan (MsgG a b) -> IO ()
loop f reqChan = do
   MsgG v replyChan <- readChan reqChan   
   writeChan replyChan $ f v    
   loop f reqChan

serve :: (a -> b) -> IO (a -> IO b)
serve f = do
  reqChan <- newChan
  forkIO $ loop f reqChan
  let sf v = do
        respChan <- newChan
        writeChan reqChan $ MsgG v respChan
        readChan respChan
  return sf

serveExample :: IO()
serveExample = do
  incFun <- serve (\x -> x + 1)
  incFun 5 >>= print
  incFun 8 >>= print

























-- Channel-based asynchronous evaluation

-- send value with private reply channel to server request channel and read its reply. 
requestReply :: Chan a -> (Chan b -> a) -> IO b
requestReply reqChan con = do
  replyChan <- newChan
  writeChan reqChan (con replyChan)
  readChan replyChan

-- messages to asynchronous compute server
data Msg a
  = MsgPut a         -- used by function evaluation thread
  | MsgGet (Chan a)  -- used by clients of the function result

-- channel for futures (asynchronous values)
data Async a = Async (Chan (Msg a))  

-- get/wait for the result (blocking read of output)
get :: Async a -> IO a
get (Async a) = requestReply a MsgGet

-- Spawn a new thread and return the request channel it listens to
spawn :: (Chan a -> IO ()) -> IO (Chan a)
spawn serverLoop = do
  reqChan <- newChan
  forkIO $ serverLoop reqChan
  return reqChan

-- server loop state 2: Function has previously returned result v
valueLoop :: Chan (Msg a) -> a -> IO ()
valueLoop reqChan v = do
  msg <- readChan reqChan
  case msg of
    MsgPut _ ->               
      valueLoop reqChan v      
    MsgGet replyChan -> do
      writeChan replyChan v
      valueLoop reqChan v
      
-- server loop state 1: Function has not yet returned its result
noValueLoop :: Chan (Msg a) -> [Chan a] -> IO ()
noValueLoop reqChan waiters = do
  msg <- readChan reqChan
  case msg of
    MsgPut v -> do -- send value to all waiting calls
      forM_ waiters $ \replyChan ->
        writeChan replyChan v
      valueLoop reqChan v
    MsgGet replyChan ->
      noValueLoop reqChan (replyChan : waiters)

-- asynchronous call to f
async :: (a -> b) -> a -> IO (Async b)
async f x = do
  reqChan <- spawn $ \chan -> do
    forkIO $ do
      writeChan chan $ MsgPut (f x)
    noValueLoop chan []
  return $ Async reqChan






































-- Example function: Fibonacchi numbers, very slowly
fib :: Int -> Maybe Int
fib n | n < 0 = Nothing
fib n = Just (fib' n)
  where -- fib': Nat -> Int
      fib' 0 = 1
      fib' 1 = 1
      fib' n = fib' (n-1) + fib' (n-2)

fibExample :: IO ()
fibExample = do
  async fib 30 >>= get >>= print
  async fib 20 >>= get >>= print
  async fib  8 >>= get >>= print
  async fib 14 >>= get >>= print
  async fib  3 >>= get >>= print
  return ()


fibExample2 :: IO ()
fibExample2 = do
  forkIO $ async fib 30 >>= get >>= print
  forkIO $ async fib 20 >>= get >>= print
  forkIO $ async fib  8 >>= get >>= print
  forkIO $ async fib 14 >>= get >>= print
  forkIO $ async fib  3 >>= get >>= print
  return ()
