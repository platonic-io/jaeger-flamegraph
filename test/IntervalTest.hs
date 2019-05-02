module IntervalTest where

import           Data.List        (nub, permutations)
import           Jaeger.Interval
import           Test.Tasty.Hspec

-- https://hspec.github.io/writing-specs.html
-- https://hackage.haskell.org/package/tasty-discover

spec_interval :: Spec
spec_interval = do
  describe "measure" $ do
    it "should support one interval" $ do
      (measure [interval 10 12]) `shouldBe` 2

    it "should support disjoint intervals" $ do
      (measure [(interval 10 12), (interval 20 25)]) `shouldBe` 7

    it "should support a more complex interval" $ do
      (measure [ (interval 10 12)
               , (interval 20 25)
               , (interval 10 25)
               , (interval 21 22)
               ]) `shouldBe` 15

prop_lte :: [Interval] -> Bool
prop_lte is = (measure is) <= (sum $ width <$> is)

-- permutations is too slow, so trim the list
prop_permutations :: [Interval] -> Bool
prop_permutations is =
  1 == (length $ nub $ measure <$> (permutations . take 7 $ is))
