# Week 7 - Block Automata

## Suggested Reading

None.

### Going Beyond

TBD

## Exercises

In these exercises you will develop a set of functions for evaluating *block
cellular automata* that are parameterised over the evaluation rules, as well as
a set of evaluation rules that involve nontrivial state management within each
cell. **You will make use of these techniques at the exam.**

### Cellular Automata

A two-dimensional *cellular automaton* is a model of computation where the
computation is structured as a grid of *cells*, each of which contain some
state. Computation occurs by each cell interacting with its neighbours through
some rule. Conway's Game of Life is an example of a cellular automaton that you
may be familiar with. For our purposes, each cell interacts only with its
immediate (non-diagonal) neighbours.

A [*block cellular
automaton*](https://en.wikipedia.org/wiki/Block_cellular_automaton) is a variant
of cellular automatons where the grid is partitioned into non-overlapping blocks
and interactions take place only for the cells inside each block. By regularly
shifting the partition of the grid into blocks, a cell still gets to eventually
interact with all of its neighbours. Block cellular automatons are useful when
the interactions are complicated. For these exercises we use the [*Margolus
neighbourhood*](https://en.wikipedia.org/wiki/Block_cellular_automaton#Neighborhoods)
partitioning scheme, which is demonstrated below. Note that we assume a world
shaped like a torus (or donut), where the neighbours of a cell at the edges are
found by wrapping around to the opposite edge.

![Margolus neighbourhoods](margolus.png)

The numbers serve only to identify which cells belong to the same
neighbourhood---this is significant only in the odd case, where the
neighbourhoods wrap around the edges of the grid.

For each iteration and partioning of the grid into Margolus neighbourhoods, each
neighbourhood is updated as follows:

1. The upper left cell interacts with the upper right cell, and the lower left
  cell interacts with the lower right cell.
2. The upper left cell interacts with the lower left cell, and the upper right
  cell interacts with the lower right cell.

This is shown in the image below.

![Interactions](interactions.png)

At the end of each iteration, the partitioning shifts by one. Note that this
allows some concurrency within each Margolous neighbourhood, and more
importantly, it also allows each neighbourhood to be updated independently.

As always, you will be working from a [code handout](handout/), and there are
[solutions](solutions/) available. The handout is completely skeletal, with
nothing to explain.

### Basic infrastructure

For the following you will be adding code to the `BlockAutomaton.Rules` module.

A grid cell can be identified with a pair *(i,j)* of a row and column coordinate
respectively (note that this differs from normal *x/y* convention). However, we
also need a datatype for representing the position of a cell within its Margolus
neighbourhood (upper-left, upper-right, lower-left, lower-right):

```Haskell
data MargolusPos = UL | UR | LL | LR
  deriving (Eq, Show)
```

Now define a function

```Haskell
margolusInitial :: (Int, Int) -> MargolusPos
```

that given an initial position *(i,j)* of a cell determines its Margolus
position (assume an even iteration, i.e., the red grid above).

<details> <summary>Open this to see the answer</summary>

```Haskell
margolusInitial :: (Int, Int) -> MargolusPos
margolusInitial (i, j) =
  case (i `mod` 2, j `mod` 2) of
    (0, 0) -> UL
    (0, 1) -> UR
    (1, 0) -> LL
    _ -> LR
```

</details>

Note that as the Margolus neighbourhood shifts, each cells' Margolus position is
mirrored across the centre. For example, a cell in position `UL` in one
iteration will be in position `LR` in the next iteration, and then flip back to
`UL` in the iteration after that. Write a function

```Haskell
margolusShift :: MargolusPos -> MargolusPos
```

that performs this transformation.

<details> <summary>Open this to see the answer</summary>

```Haskell
margolusShift :: MargolusPos -> MargolusPos
margolusShift UL = LR
margolusShift LR = UL
margolusShift UR = LL
margolusShift LL = UR
```

</details>

### Interaction rules

The evolution of a cellular automaton is defined by the interaction function
that determines what happens when two cells interact. For convenience, we will
also require two other functions: one for observing the external state of the
automaton (e.g. for visualisation), and one for initialising the cells of the
grid when the automaton starts. We encapsulate this in a type `Rules state obs`,
which we also define in `BlockAutomaton.Rules`:

```Haskell
data Rules state obs = Rules
  { rulesInitial :: (Int, Int) -> state,
    rulesInteract :: (state, state) -> (state, state),
    rulesObserve :: state -> obs
  }
```

A `Rules state obs` describes a cellular automaton where cells contain values of
type `state`, and the observable result of evaluation is `obs`.

* `rulesInitial` is passed the initial coordinate of a cell, and returns the
  initial state.

* `rulesInteract` is passed two cell states and returns two new states (in the
  same order as the arguments).

* `rulesObserve` is passed a cell state and returns some observable data. In
  some cases this may be the same as the entire cell state, but often this is
  some kind of representative value of the cell state (e.g., in our most fancy
  ruleset, this will be an RGB colour we can use to visualise the grid).

Now define an interaction ruleset where cell states are `Doubles` and the
observables are `Int`

```Haskell
smoothen :: Rules Double Int
```

such that

* Each cell *(i,j)* initially has the value *i+j*.

* When two cells interact, they each end up with a state that is half of their
  summed states (i.e., take the average of the two states).

* Observation is by rounding the `Double` to an `Int`.


<details> <summary>Open this to see the answer</summary>

```Haskell
smoothen :: Rules Double Int
smoothen =
  Rules
    { rulesInitial = \(i, j) -> fromIntegral $ i + j,
      rulesInteract = \(x, y) -> ((x + y) / 2, (x + y) / 2),
      rulesObserve = round
    }
```

</details>

Finally, make sure the following things are exported from `BlockAutomaton.Rules`:

```
  ( Rules (..),
    MargolusPos (..),
    margolusInitial,
    margolusShift,
  )
```

### Simulating block cellular automatons

<details> <summary>Open this to see the answer</summary>

```Haskell

```

</details>

### The Rock-Paper-Scissors Multi-Actor Duel

<details> <summary>Open this to see the answer</summary>

```Haskell
data Decider = Decider RPS Decider

decide :: Decider -> StdGen -> (RPS, Decider)
decide (Decider decision next) r = (decision, next)

alwaysRock :: CellDecider
alwaysRock = CellDecider Rock alwaysRock

alwaysPaper :: CellDecider
alwaysPaper = CellDecider Paper alwaysPaper

alwaysScissors :: CellDecider
alwaysScissors = CellDecider Scissors alwaysScissor
```

### Visualisation

Copy [solution/viewer.hs](solution/viewer.hs) into your solution (overriding the
existing dummy `viewer.hs`) and do

```
$ cabal run -- viewer
```

to observe a bloody struggle between your strategies. Use

```
$ cabal run -- viewer H W
```

to run on a grid of size *H* by *W*.
