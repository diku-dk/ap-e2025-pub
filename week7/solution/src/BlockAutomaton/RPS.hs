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

data Strategy = Strategy (StdGen -> (RPS, StdGen, Strategy))

decide :: Strategy -> StdGen -> (RPS, StdGen, Strategy)
decide (Strategy f) r = f r

data RPSCell = RPSCell
  { cellStrategy :: Strategy,
    cellColour :: Colour,
    cellRNG :: StdGen
  }

cycleRPS :: [RPS] -> Strategy
cycleRPS all_rps = go all_rps
  where
    go [] = cycleRPS all_rps
    go (d : ds) = Strategy $ \r -> (d, r, go ds)

alwaysRock :: Strategy
alwaysRock = cycleRPS [Rock]

alwaysPaper :: Strategy
alwaysPaper = cycleRPS [Paper]

alwaysScissors :: Strategy
alwaysScissors = cycleRPS [Scissors]

randomlyFrom :: [RPS] -> Strategy
randomlyFrom xs = Strategy $ \r ->
  let (l, r') = uniformR (0, length xs - 1) r
   in (xs !! l, r', randomlyFrom xs)

strategies :: [(Strategy, Colour)]
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
        { cellStrategy = d,
          cellColour = c,
          cellRNG = r'
        }

rpsInteract :: (RPSCell, RPSCell) -> (RPSCell, RPSCell)
rpsInteract (a, b) =
  let (decision_a, r_a, next_a) = decide (cellStrategy a) (cellRNG a)
      (decision_b, r_b, next_b) = decide (cellStrategy b) (cellRNG b)
   in if decision_a `defeats` decision_b
        then
          ( a {cellStrategy = next_a, cellRNG = r_a},
            b {cellStrategy = next_a, cellRNG = r_b, cellColour = cellColour a}
          )
        else
          if decision_b `defeats` decision_a
            then
              ( a {cellStrategy = next_b, cellRNG = r_a, cellColour = cellColour b},
                b {cellStrategy = next_b, cellRNG = r_b}
              )
            else
              ( a {cellStrategy = next_a, cellRNG = r_a},
                b {cellStrategy = next_b, cellRNG = r_b}
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
