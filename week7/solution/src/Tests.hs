module Tests (tests) where

import BlockAutomaton.RPS
import BlockAutomaton.Rules
import BlockAutomaton.Simulation
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "All tests"
    [ testGroup
        "smoothen"
        [ testCase "initialGrid" $
            initialGrid smoothen 4 4
              @?= [ [(UL, 0.0), (UR, 1.0), (UL, 2.0), (UR, 3.0)],
                    [(LL, 1.0), (LR, 2.0), (LL, 3.0), (LR, 4.0)],
                    [(UL, 2.0), (UR, 3.0), (UL, 4.0), (UR, 5.0)],
                    [(LL, 3.0), (LR, 4.0), (LL, 5.0), (LR, 6.0)]
                  ],
          testCase "observeGrid" $
            observeGrid smoothen (initialGrid smoothen 4 4)
              @?= [ [0, 1, 2, 3],
                    [1, 2, 3, 4],
                    [2, 3, 4, 5],
                    [3, 4, 5, 6]
                  ],
          testCase "stepGrid" $
            observeGrid smoothen (stepGrid smoothen $ initialGrid smoothen 4 4)
              @?= [ [1, 1, 3, 3],
                    [1, 1, 3, 3],
                    [3, 3, 5, 5],
                    [3, 3, 5, 5]
                  ]
        ],
      testGroup "rps" $
        let rules = rps 1337
         in [ testCase "observeGrid" $
                observeGrid rules (initialGrid rules 4 4)
                  @?= [ [RGB 255 0 255, RGB 255 255 255, RGB 0 255 255, RGB 0 0 255],
                        [RGB 100 255 255, RGB 0 0 255, RGB 100 255 255, RGB 100 255 255],
                        [RGB 100 255 255, RGB 255 0 255, RGB 255 255 255, RGB 0 0 255],
                        [RGB 255 0 255, RGB 0 255 255, RGB 255 255 255, RGB 0 255 255]
                      ],
              testCase "stepGrid" $
                observeGrid rules (stepGrid rules (initialGrid rules 4 4))
                  @?= [ [RGB 0 0 255, RGB 0 0 255, RGB 0 255 255, RGB 100 255 255],
                        [RGB 0 0 255, RGB 0 0 255, RGB 100 255 255, RGB 100 255 255],
                        [RGB 255 0 255, RGB 255 0 255, RGB 255 255 255, RGB 0 255 255],
                        [RGB 255 0 255, RGB 255 0 255, RGB 255 255 255, RGB 0 255 255]
                      ]
            ]
    ]
