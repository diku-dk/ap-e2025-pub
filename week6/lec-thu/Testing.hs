import Test.QuickCheck
import Data.IORef
-- import Control.Monad
-- import Control.Monad.State
-- import Data.Int

-- Testing by reference implementation

-- Model of state monad (reference implementation)
-- NB: This the standard functional state monad without a data type constructor

type FState s a = s -> (a, s)

fmapF :: (a -> b) -> FState s a -> FState s b
fmapF f sa s =
  let (a', s') = sa s
  in (f a', s')

returnF :: a -> FState s a
returnF a s = (a, s)

bindF :: FState s a -> (a -> FState s b) -> FState s b
bindF sa f s =
  let (a', s') = sa s
  in  f a' s'

-- standard definition of join in terms of bind
joinF :: FState s (FState s a) -> FState s a
joinF c = c `bindF` id

getF :: FState s s
getF s = (s, s)

putF :: s -> FState s ()
putF new current = ((), new)

runStateF :: FState s a -> s -> (a, s)
runStateF = id

-- Put/get properties of state monad

prop_putget :: Eq s => s -> s -> Bool
prop_putget s initial =
  (putF s `bindF` const getF) initial == 
  (putF s `bindF` const (returnF s)) initial

prop_putput :: Eq s => s -> s -> s -> Bool
prop_putput s t initial =
  (putF s `bindF` const (putF t)) initial ==
  (putF t) initial

prop_getget :: Eq s => s -> Bool
prop_getget initial =
  (getF `bindF` const getF) initial ==
  getF initial

-- Testing of get/put properties by testing on a large proxy type with equality, using Int
prop_state = conjoin
           [property (prop_putget :: Int -> Int -> Bool),
            property (prop_putput :: Int -> Int -> Int -> Bool),
            property (prop_getget :: Int -> Bool)]

-- quickCheck with n tests
quickCheckN :: Testable a => Int -> a -> IO ()
quickCheckN n = quickCheckWith (stdArgs { maxSuccess = n })

test1 = quickCheck (prop_putget :: Int -> Int -> Bool)
test2 = quickCheck (prop_putput :: Int -> Int -> Int -> Bool)
test3 = quickCheck (prop_getget :: Int -> Bool)

testState = quickCheck prop_state

-- Monad properties of state monad

prop_joinreturn :: (Eq a, Eq s) => FState s a -> s -> Bool
prop_joinreturn sa s = joinF (returnF sa) s == sa s

prop_joinfmapreturn :: (Eq a, Eq s) => FState s a -> s -> Bool
prop_joinfmapreturn sa s = joinF (fmapF returnF sa) s == sa s

prop_joinjoin :: (Eq a, Eq s) => FState s (FState s (FState s a)) -> s -> Bool
prop_joinjoin sa s = joinF (joinF sa) s == joinF (fmapF joinF sa) s

prop_monad = conjoin 
  [property (prop_joinreturn :: FState Int Int -> Int -> Bool),
   property (prop_joinfmapreturn :: FState Int Int -> Int -> Bool),
   property (prop_joinjoin :: FState Int (FState Int (FState Int Int)) -> Int -> Bool)]

instance Show (FState Int Int) where
  show f = "Some FState Int Int element"

instance Show (FState Int (FState Int (FState Int Int))) where
  show f = "Some FState Int (FState Int (FState Int Int)) element"

test4 = quickCheck (prop_joinreturn :: FState Int Int -> Int -> Bool)
test5 = quickCheck (prop_joinfmapreturn :: FState Int Int -> Int -> Bool)
test6 = quickCheck (prop_joinjoin :: FState Int (FState Int (FState Int Int)) -> Int -> Bool)

testMonad = quickCheck prop_monad

-- Imperative state implementation

type IState s a = IORef s -> IO a

fmapI :: (a -> b) -> IState s a -> IState s b
fmapI f sa s = fmap f (sa s)

returnI :: a -> IState s a 
returnI a ref = return a

bindI :: IState s a -> (a -> IState s b) -> IState s b
bindI isa f ref = do
  a <- isa ref
  f a ref

joinI :: IState s (IState s a) -> IState s a
joinI c = c `bindI` id

getI :: IState s s
getI = readIORef

putI :: s -> IState s ()
putI s ref = writeIORef ref s

runIState :: IState s a -> s -> IO (a, s)
runIState isa initialState = do
  ref <- newIORef initialState
  a' <- isa ref
  s' <- readIORef ref
  return (a', s')

-- Test state  properties (put/get properties) on functional states

-- Using ioProperty: Checks only observable output, not IO side effects
prop_putgetI :: Eq s => s -> s -> Property
prop_putgetI s initial = ioProperty $ do
  out1 <- runIState (putI s `bindI` const getI) initial
  out2 <- runIState (putI s `bindI` const (returnI s)) initial
  return (out1 == out2)

prop_putputI :: Eq s => s -> s -> s -> Property
prop_putputI s t initial = ioProperty $ do
  out1 <- runIState (putI s `bindI` const (putI t)) initial
  out2 <- runIState (putI t) initial
  return (out1 == out2)

prop_getgetI :: Eq s => s -> Property
prop_getgetI initial = ioProperty $ do
  out1 <- runIState (getI `bindI` const getI) initial
  out2 <- runIState getI initial
  return (out1 == out2)

prop_stateI = conjoin
           [property (prop_putgetI :: Int -> Int -> Property),
            property (prop_putputI :: Int -> Int -> Int -> Property),
            property (prop_getgetI :: Int -> Property)]


test1I = quickCheck (prop_putgetI :: Int -> Int -> Property)
test2I = quickCheck (prop_putputI :: Int -> Int -> Int -> Property)
test3I = quickCheck (prop_getgetI :: Int -> Property)

testStateI = quickCheck prop_stateI

-- Monad properties of imperative state monad (without checking side effects)

istate :: (s -> a) -> IState s a
istate f ref = do
  s <- readIORef ref
  return (f s)
  
prop_joinreturnI :: (Eq a, Eq s) => (s -> a) -> s -> Property
prop_joinreturnI f s = ioProperty $ do
  out1 <- runIState (joinI (returnI (istate f))) s
  out2 <- runIState (istate f) s
  return (out1 == out2)

prop_joinfmapreturnI :: (Eq a, Eq s) => (s -> a) -> s -> Property
prop_joinfmapreturnI f s = ioProperty $ do
  out1 <- runIState (joinI (fmapI returnI (istate f))) s
  out2 <- runIState (istate f) s
  return (out1 == out2)

istate3 :: (s -> s -> s -> a) -> IState s (IState s (IState s a))
istate3 f ref = do
  s <- readIORef ref
  return $ \ref' -> do s' <- readIORef ref'
                       return $ \ref'' -> do s'' <- readIORef ref''
                                             return (f s s' s'')

prop_joinjoinI :: (Eq a, Eq s) => (s -> s -> s -> a) -> s -> Property
prop_joinjoinI f s = ioProperty $ do
  out1 <- runIState (joinI (joinI (istate3 f))) s
  out2 <- runIState (joinI (fmapI joinI (istate3 f))) s
  return (out1 == out2)

prop_monadI = conjoin 
  [property (prop_joinreturnI :: (Int -> Int) -> Int -> Property),
   property (prop_joinfmapreturnI :: (Int -> Int) -> Int -> Property),
   property (prop_joinjoinI :: (Int -> Int -> Int -> Int) -> Int -> Property)]

instance Show (Int -> Int) where
  show f = "Some Int -> Int function"

instance Show (Int -> Int -> Int -> Int) where
  show f = "Some Int -> Int -> Int -> Int function"

test4I = quickCheck (prop_joinreturnI :: (Int -> Int) -> Int -> Property)
test5I = quickCheck (prop_joinfmapreturnI :: (Int -> Int) -> Int -> Property)
test6I = quickCheck (prop_joinjoinI :: (Int -> Int -> Int -> Int) -> Int -> Property)

testMonadI = quickCheck prop_monad

-- Testing with respect to model/reference implementation
-- Software under test (SUT): Imperative state monad
-- Model (reference implementation): Functional state 

-- Conversion between SUT type and model type, e.g. from/to imperative state to functional state
to :: FState s a -> IState s a
to sa ref = do
  s <- readIORef ref
  let (a, t) = sa s
  writeIORef ref t
  return a

from :: IState s a -> (s -> IO (a, s))
from isa s = do
  ref <- newIORef s
  a <- isa ref
  t <- readIORef ref
  return (a, t)

-- to/from property: from . to ~ id (modulo side effects)
prop_tofrom :: (Eq s, Eq a) => FState s a -> s -> Property
prop_tofrom sa s = ioProperty $ do
  out1 <- (from (to sa)) s
  let out2 = sa s
  return (out1 == out2)

testToFrom = quickCheck (prop_tofrom :: FState Int Int -> Int -> Property)

-- Correctness of getI wrt. getF: from getI ~ getF
prop_get :: Eq a => a -> Property
prop_get initialState = ioProperty $ do
  out1 <- from getI initialState
  let out2 = getF initialState
  return (out1 == out2)

-- Correctness of putI wrt. putF: from (putI s) ~ putF
prop_put :: Eq s => s -> s -> Property
prop_put s initialState = ioProperty $ do
  out1 <- from (putI s) initialState
  let out2 = putF s initialState
  return (out1 == out2)

-- Correctness of fmapI wrt. fmapF: from (fmapI f (to sa)) ~ fmapF f sa
prop_fmap :: (Eq s, Eq b) => (a -> b) -> FState s a -> s -> Property
prop_fmap f sa initialState = ioProperty $ do
  out1 <- from (fmapI f (to sa)) initialState
  let out2 = fmapF f sa initialState
  return (out1 == out2)

-- Correctness of returnI wrt. returnF: from (returnI a) ~ returnF a
prop_return :: (Eq s, Eq a) => a -> s -> Property
prop_return a initialState = ioProperty $ do
  out1 <- from (returnI a) initialState
  let out2 = returnF a initialState
  return (out1 == out2)

-- Correctness of bindI wrt. bindF: to sa `bindI` (from . f) ~ sa `bindF` f
-- Sufficient to prove for f = id, that is for joinI: join
prop_bind :: (Eq s, Eq a) => FState s a -> (a -> FState s a) -> s -> Property
prop_bind sa f initialState = ioProperty $ do
  out1 <- from (to sa `bindI` (to . f)) initialState
  let out2 = (sa `bindF` f) initialState
  return (out1 == out2)
  

instance Show (Int -> FState Int Int) where
  show f = "Some Int -> FState Int Int function"
  
testGet = quickCheck (prop_get :: Int -> Property)
testPut = quickCheck (prop_put :: Int -> Int -> Property)
testFmap = quickCheck (prop_fmap :: (Int -> Int) -> FState Int Int -> Int -> Property)
testReturn = quickCheck (prop_return :: Int -> Int -> Property)
testBind = quickCheck (prop_bind :: FState Int Int -> (Int -> FState Int Int) -> Int -> Property)

prop_stateIwrtF = conjoin
  [property (prop_get :: Int -> Property),
   property (prop_put :: Int -> Int -> Property),
   property (prop_fmap :: (Int -> Int) -> FState Int Int -> Int -> Property),
   property (prop_return :: Int -> Int -> Property),
   property (prop_bind :: FState Int Int -> (Int -> FState Int Int) -> Int -> Property)]

testStateIwrtF = quickCheckN 10000 prop_stateIwrtF

-- Conversion special case: SUT and model type are the same, e.g. Fibonacchi implementation
-- Reference implementation: Naive recursive definition
-- precondition: input is nonnegative
fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- SUT: Tupled implementation
fibT :: Integer -> Integer
fibT n = fibT' (1, 1) n
  where
  fibT' :: (Integer, Integer) -> Integer -> Integer
  fibT' (r1, r2) 0 = r1
  fibT' (r1, r2) n = fibT' (r2, r1+r2) (n-1)

-- Correctness of fibT wrt. fib: fib n = fibT n for all n >= 0; limit input sizes by k
prop_fib :: Integer -> Integer -> Bool
prop_fib k n = if 0 <= n && n <= k then fibT n == fib n else True

testFib = quickCheck (prop_fib 30)


