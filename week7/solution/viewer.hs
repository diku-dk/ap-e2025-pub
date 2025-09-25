module Main (main) where

import BlockAutomaton.RPS
import BlockAutomaton.Simulation
import Control.Exception (bracket)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)

enterAlt, exitAlt, hideCursor, showCursor :: String
enterAlt = "\ESC[?1049h"
exitAlt = "\ESC[?1049l"
hideCursor = "\ESC[?25l"
showCursor = "\ESC[?25h"

-- ANSI escapes for 24-bit background/foreground
bgCode :: Colour -> String
bgCode (RGB r g b) =
  "\ESC[48;2;" <> show r <> ";" <> show g <> ";" <> show b <> "m"

fgCode :: Colour -> String
fgCode (RGB r g b) =
  "\ESC[38;2;" <> show r <> ";" <> show g <> ";" <> show b <> "m"

-- Reset attributes
reset :: String
reset = "\ESC[0m"

-- Move cursor back to upper-left corner of a grid of size (h * w)
-- h = number of color rows, w = number of color columns
moveGridHome :: Int -> Int -> String
moveGridHome h w =
  "\ESC["
    <> show h
    <> "A"
    <> "\ESC["
    <> show w
    <> "D"

-- Render a single "pixel cell" using ▀
renderPixelCell :: Colour -> Colour -> String
renderPixelCell top bottom =
  fgCode top <> bgCode bottom <> "▀" <> reset

-- Render a row of cells from two color rows
renderRow :: [Colour] -> [Colour] -> String
renderRow tops bottoms = concat (zipWith renderPixelCell tops bottoms) <> "\n"

-- Render the whole grid (height must be even)
renderGrid :: [[Colour]] -> String
renderGrid [] = ""
renderGrid (a : b : rest) = renderRow a b <> renderGrid rest
renderGrid [_] = error "Grid height must be an even number."

prepareTerminal :: IO ()
prepareTerminal = do
  putStr (enterAlt <> hideCursor)
  hFlush stdout

restoreTerminal :: IO ()
restoreTerminal = do
  putStr (reset <> showCursor <> exitAlt)
  hFlush stdout

data Config = Config
  { cfgHeight :: Int,
    cfgWidth :: Int
  }

run :: Config -> IO ()
run cfg = do
  let h = cfgHeight cfg
      w = cfgWidth cfg
      rules = rps (h + w)

  let loop grid = do
        display grid
        loop $ stepGrid rules grid
      display grid = do
        putStr $ renderGrid $ observeGrid rules grid
        hFlush stdout
        putStr $ moveGridHome ((h `div` 2)) w

  loop $ initialGrid rules h w

main :: IO ()
main = do
  args <- getArgs
  let cfg =
        case args of
          [h, w] -> Config (read h) (read w)
          _ -> Config 40 40
  bracket prepareTerminal (const restoreTerminal) (const (run cfg))
