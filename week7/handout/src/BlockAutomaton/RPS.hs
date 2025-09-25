module BlockAutomaton.RPS (Colour (..)) where

data Colour = RGB Int Int Int deriving (Show, Eq)

data RPS = Rock | Paper | Scissors
  deriving (Eq, Show)

defeats :: RPS -> RPS -> Bool
defeats Paper Rock = True
defeats Scissors Paper = True
defeats Rock Scissors = True
defeats _ _ = False
