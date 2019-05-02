{-# LANGUAGE NamedFieldPuns #-}

module Jaeger.Interval
  ( Interval
  , interval
  , intervalStart
  , intervalEnd
  , measure
  , width)
where

import           Data.List                 (sort)
import           Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)

-- nothing on http://hackage.haskell.org/packages/search?terms=interval
data Interval = Interval
  { intervalStart :: Integer -- ^ assume start <= end
  , intervalEnd   :: Integer
  } deriving (Eq, Ord, Show)

interval :: Integer -> Integer -> Interval
interval a b | a <= b    = Interval a b
             | otherwise = Interval b a

width :: Interval -> Integer
width (Interval from to) = to - from

-- idea by etorreborre
measure :: [Interval] -> Integer
measure unsorted =
  count $ sieve sorted
  where
    sorted = sort unsorted
    sieve (car@(Interval a b) : cadr@(Interval c d) : rest) =
      if a == c || c <= b
      then sieve $ Interval a (max b d) : rest
      else car : sieve (cadr : rest)
    sieve done = done
    count is = sum $ width <$> is

instance Arbitrary Interval where
  arbitrary = do a <- arbitrary
                 b <- arbitrary
                 pure $ interval a b
