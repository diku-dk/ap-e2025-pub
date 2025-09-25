{-# LANGUAGE RankNTypes #-}
-- Preliminary, 2025-09-25.  Minor changes may be added.

import Control.Monad (ap, join)
import Control.Arrow ((***))
import Data.IORef


-- Free monad over functor f
data Free f a
  = Pure a
  | Free (f (Free f a))

-- Functor instance
instance Functor f => Functor (Free f) where
  fmap f (Pure x)  = Pure (f x)
  fmap f (Free fx) = Free (fmap (fmap f) fx)

-- Applicative instance
instance Functor f => Applicative (Free f) where
  pure = Pure
  Pure f  <*> fx = fmap f fx
  Free ff <*> fx = Free (fmap (<*> fx) ff)

-- Monad instance
instance Functor f => Monad (Free f) where
  Pure x  >>= f = f x
  Free g >>= f = Free (fmap (>>= f) g)

-- State monads over states of type s
class (Monad m) => StateMonad m s where
  put :: s -> m ()
  get :: m s


-- Purely functional state monad
newtype FState s a = FState {runFState :: (s -> (a, s))}

instance Functor (FState s) where
  fmap f (FState sa) = FState $ (f *** id) . sa
  
instance Applicative (FState s) where
  pure a = FState $ \s -> (a, s) 
  (FState sf) <*> (FState sa) = FState $ \s ->
    let (f, s')  = sf s
        (a, s'') = sa s'
    in (f a, s'')
    
instance Monad (FState s) where
  return = pure  -- default, can be omitted
  (FState sa) >>= f = FState (uncurry ($) . (runFState . f *** id)  .  sa)

instance StateMonad (FState s) s where
  put s = FState (const ((), s))
  get = FState dup where dup x = (x, x)

-- State-like functor
data StateF s a 
  = Put s a         -- iso with (s, () -> a)
  | Get (s -> a)    -- iso with ((), s -> a)

instance Functor (StateF s) where
  fmap f (Put s a) = Put s (f a)
  fmap f (Get g)   = Get $ f . g

-- Free monad over State functor
type FreeState s a = Free (StateF s) a

instance StateMonad (Free (StateF s)) s where
  put s = Free (Put s (Pure ()))
  get = Free (Get Pure)

-- Evaluating monad into target monad (generic)
eval :: (StateMonad m s) => FreeState s b -> m b
eval (Pure a) = return a
eval (Free g) = join $ fmap eval (interp g)
     where interp :: (StateMonad m s) => StateF s a -> m a
           interp (Put s a) = put s >> return a
           interp (Get f)   = get >>= return . f


runFreeState :: FreeState s a -> s -> (a, s)
runFreeState = runFState . eval


-- Examples

modify :: (StateMonad m s, StateMonad m t) => (s -> t) -> m ()
modify f = get >>= put . f

tickM :: (StateMonad m Int) => m Int
tickM = do
  n <- get
  put (n + 1)
  return n

tickF :: FState Int Int
tickF = tickM

tickFree :: FreeState Int Int
tickFree = tickM


type Stack = [Int]

push :: (StateMonad m [a]) => a -> m ()
push x = modify (x:)

pop :: (StateMonad m [a]) => m (Maybe a)
pop = do
  st <- get
  case st of
    [] -> return Nothing
    (x : xs) -> put xs >> return (Just x)

stackExampleM :: (StateMonad m [Integer]) => m (Maybe Integer)
stackExampleM = do
  push 3
  push 5
  a <- pop
  b <- pop
  return (plus a b)
  where plus :: Maybe Integer -> Maybe Integer -> Maybe Integer
        plus mx my = do
          x <- mx
          y <- my
          return (x + y)

stackExampleM2 :: (StateMonad m [Integer]) => m (Maybe Integer)
stackExampleM2 = stackExampleM >> pop

stackExampleF :: FState [Integer] (Maybe Integer)
stackExampleF = stackExampleM

stackExampleF2 :: FState [Integer] (Maybe Integer)
stackExampleF2 = stackExampleM2

stackExampleFree :: FreeState [Integer] (Maybe Integer)
stackExampleFree = stackExampleM

stackExampleFree2 :: FreeState [Integer] (Maybe Integer)
stackExampleFree2 = stackExampleM2

-- Imperative state implementation

newtype IState s a = IState (IORef s -> IO a)

runIState :: IState s a -> s -> IO a
runIState (IState g) s  =
  do ref <- newIORef s
     g ref

instance Functor (IState s) where
  fmap f (IState sa) = IState $ fmap f . sa

instance Applicative (IState s) where
  pure = IState . const . return
  (IState sf) <*> (IState sa) = IState $ \ref ->
     do f <- sf ref
        a <- sa ref
        return (f a)

instance Monad (IState s) where
  return = pure 
  (IState sa) >>= f = IState $ \ref ->
     do a <- sa ref
        let IState g = f a
        g ref
        
instance StateMonad (IState s) s where
  put s = IState $ \ref -> writeIORef ref s
  get = IState readIORef

tickI :: IState Int Int
tickI = tickM 

stackExampleI :: IState [Integer] (Maybe Integer)
stackExampleI = stackExampleM

stackExampleI2 :: IState [Integer] (Maybe Integer)
stackExampleI2 = stackExampleM2

-- Optimized evaluation of free state monad
runFreeStateOpt :: FreeState s a -> s -> a
runFreeStateOpt (Pure x) _         = x 
runFreeStateOpt (Free (Get c)) s   = runFreeStateOpt (c s) s
runFreeStateOpt (Free (Put s c)) _ = runFreeStateOpt c s


-- Monadic Fibonacci numbers
fibM :: (Monad m) => Int -> m Int
fibM 0 = return 1
fibM 1 = return 1
fibM n = do x <- fibM (n - 1)
            y <- fibM (n - 2)
            return (x + y)

data FibOp a
  = FibLog String a
  | FibMemo Int (FibM Int) (Int -> a)

instance Functor FibOp where
  fmap f (FibLog s x) =
    FibLog s $ f x
  fmap f (FibMemo n fibn c) =
    FibMemo n fibn $ \x -> f (c x)

type FibM a = Free FibOp a

fibLog :: String -> FibM ()
fibLog s = Free $ FibLog s $ Pure ()

memo :: Int -> FibM Int -> FibM Int
memo n fibn =
  Free $ FibMemo n fibn $ \x -> Pure x

fib :: Int -> FibM Int
fib 0 = pure 1
fib 1 = pure 1
fib n = memo n $ do
  fibLog $ "f(" ++ show n ++ ")"
  x <- fib (n - 1)
  y <- fib (n - 2)
  pure $ x + y

pureRunFibM :: FibM a -> a
pureRunFibM (Pure x) = x
pureRunFibM (Free (FibLog _ c)) =
  pureRunFibM c
pureRunFibM (Free (FibMemo _ fibn c)) =
  pureRunFibM $ c $ pureRunFibM fibn

ioRunFibM :: FibM a -> IO a
ioRunFibM (Pure x) = pure x
ioRunFibM (Free (FibLog s c)) = do
  putStrLn s
  ioRunFibM c
ioRunFibM _ = undefined

listRunFibM :: FibM a -> ([String], a)
listRunFibM (Pure x) = ([], x)
listRunFibM (Free (FibLog s c)) =
  let (l, x) = listRunFibM c
   in (s : l, x)
listRunFibM _ = undefined

memoRunFibM :: FibM a -> IO a
memoRunFibM m = fst <$> run [] m
  where
    run ::
      [(Int, Int)] ->
      FibM a ->
      IO (a, [(Int, Int)])
    run cache (Pure x) = pure (x, cache)
    run cache (Free (FibLog s c)) = do
      putStrLn s
      run cache c
    run cache (Free (FibMemo n fibn c)) =
      case lookup n cache of
        Just x -> run cache $ c x
        Nothing -> do
          (x, cache') <- run cache fibn
          run ((n, x) : cache') $ c x

