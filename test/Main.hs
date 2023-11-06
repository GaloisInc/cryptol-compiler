import Test.Tasty (defaultMain, testGroup)

import Golden qualified as Golden

main :: IO ()
main =
  do  goldenTests <- Golden.tests
      defaultMain $ testGroup "tests"
        [goldenTests]
