import Test.Tasty (defaultMain, localOption, mkTimeout)
import Tests (tests)

main :: IO ()
main = defaultMain $ localOption (mkTimeout 3000000) tests
