module BlockAutomaton.RPS (Colour (..), rps) where

import BlockAutomaton.Rules
import System.Random (StdGen, mkStdGen, uniformR)

data Colour = RGB Int Int Int deriving (Show, Eq)

data RPS = Rock | Paper | Scissors
  deriving (Eq, Show)

defeats :: RPS -> RPS -> Bool
defeats Paper Rock = True
defeats Scissors Paper = True
defeats Rock Scissors = True
defeats _ _ = False

data Decider = Decider (StdGen -> (RPS, StdGen, Decider))

decide :: Decider -> StdGen -> (RPS, StdGen, Decider)
decide (Decider f) r = f r

data RPSCell = RPSCell
  { cellDecider :: Decider,
    cellColour :: Colour,
    cellRNG :: StdGen
  }

cycleRPS :: [RPS] -> Decider
cycleRPS all_rps = go all_rps
  where
    go [] = cycleRPS all_rps
    go (d : ds) = Decider $ \r -> (d, r, go ds)

alwaysRock :: Decider
alwaysRock = cycleRPS [Rock]

alwaysPaper :: Decider
alwaysPaper = cycleRPS [Paper]

alwaysScissors :: Decider
alwaysScissors = cycleRPS [Scissors]

randomlyFrom :: [RPS] -> Decider
randomlyFrom xs = Decider $ \r ->
  let (l, r') = uniformR (0, length xs - 1) r
   in (xs !! l, r', randomlyFrom xs)

strategies :: [(Decider, Colour)]
strategies =
  [ (alwaysRock, RGB 255 0 0),
    (alwaysPaper, RGB 0 255 0),
    (alwaysScissors, RGB 0 0 255),
    (cycleRPS [Rock, Paper, Scissors], RGB 255 0 255),
    (cycleRPS [Scissors, Rock, Paper], RGB 0 255 255),
    (cycleRPS [Paper, Rock, Scissors], RGB 100 255 255),
    (randomlyFrom [Rock, Paper, Scissors], RGB 255 255 255)
  ]

rpsInitial :: Int -> (Int, Int) -> RPSCell
rpsInitial s (i, j) =
  let r = mkStdGen (s + i * 10000 + j)
      n = length strategies
      (l, r') = uniformR (0, n - 1) r
      (d, c) = strategies !! l
   in RPSCell
        { cellDecider = d,
          cellColour = c,
          cellRNG = r'
        }

rpsInteract :: (RPSCell, RPSCell) -> (RPSCell, RPSCell)
rpsInteract (a, b) =
  let (decision_a, r_a, next_a) = decide (cellDecider a) (cellRNG a)
      (decision_b, r_b, next_b) = decide (cellDecider b) (cellRNG b)
   in if decision_a `defeats` decision_b
        then
          ( a {cellDecider = next_a, cellRNG = r_a},
            b {cellDecider = next_a, cellRNG = r_b, cellColour = cellColour a}
          )
        else
          if decision_b `defeats` decision_a
            then
              ( a {cellDecider = next_b, cellRNG = r_a, cellColour = cellColour b},
                b {cellDecider = next_b, cellRNG = r_b}
              )
            else
              ( a {cellDecider = next_a, cellRNG = r_a},
                b {cellDecider = next_b, cellRNG = r_b}
              )

rpsObserve :: RPSCell -> Colour
rpsObserve = cellColour

rps :: Int -> Rules RPSCell Colour
rps s =
  Rules
    { rulesInitial = rpsInitial s,
      rulesInteract = rpsInteract,
      rulesObserve = rpsObserve
    }
