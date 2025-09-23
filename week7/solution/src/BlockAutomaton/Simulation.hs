module BlockAutomaton.Simulation (initialGrid, stepGrid, observeGrid) where

import BlockAutomaton.Rules

type Grid a = [[a]]

gridIndex :: (Int, Int) -> Grid a -> a
gridIndex (i, j) g =
  let row = g !! (i `mod` length g)
   in row !! (j `mod` length row)

gridUpdate :: (Int, Int) -> a -> Grid a -> Grid a
gridUpdate (i, j) x g =
  case splitAt (i `mod` length g) g of
    (rows_bef, row : rows_aft) ->
      case splitAt (j `mod` length row) row of
        (elems_bef, _ : elems_aft) ->
          rows_bef ++ [elems_bef ++ x : elems_aft] ++ rows_aft
        _ -> error "column out of bounds"
    _ -> error "row out of bounds"

positions :: Int -> Int -> [[(Int, Int)]]
positions h w = map (\i -> map (\j -> (i, j)) [0 .. w - 1]) [0 .. h - 1]

initialGrid :: Rules state obs -> Int -> Int -> Grid (MargolusPos, state)
initialGrid rules h w =
  map (map (\(i, j) -> (margolusInitial (i, j), rulesInitial rules (i, j)))) (positions h w)

stepOne ::
  Rules state obs ->
  (Int, Int) ->
  Grid (MargolusPos, state) ->
  Grid (MargolusPos, state)
stepOne rules (i, j) g =
  let (pos, ul) = gridIndex (i, j) g
   in case pos of
        UL ->
          let (_, ur) = gridIndex (i, j + 1) g
              (_, ll) = gridIndex (i + 1, j) g
              (_, lr) = gridIndex (i + 1, j + 1) g
              (ul', ur') = rulesInteract rules (ul, ur)
              (ll', lr') = rulesInteract rules (ll, lr)
              (ul'', ll'') = rulesInteract rules (ul', ll')
              (ur'', lr'') = rulesInteract rules (ur', lr')
           in gridUpdate (i, j) (UL, ul'') $
                gridUpdate (i, j + 1) (UR, ur'') $
                  gridUpdate (i + 1, j) (LL, ll'') $
                    gridUpdate (i + 1, j + 1) (LR, lr'') g
        _ -> g

stepGrid ::
  Rules state obs ->
  Grid (MargolusPos, state) ->
  Grid (MargolusPos, state)
stepGrid rules g = map (map shiftCell) $ foldr (stepOne rules) g (concat $ positions h w)
  where
    shiftCell (pos, s) = (margolusShift pos, s)
    h = length g
    w = length $ g !! 0

observeGrid :: Rules state obs -> Grid (MargolusPos, state) -> Grid obs
observeGrid rules = map (map (rulesObserve rules . snd))
