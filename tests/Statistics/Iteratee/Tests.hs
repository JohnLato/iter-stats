module Statistics.Iteratee.Tests

where

import Data.Iteratee as I
import Statistics.Iteratee as Si
import Statistics.Sample as St
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Control.Monad.Identity
import qualified Data.Vector.Unboxed as V

tests =
  [ testGroup "Sample" $ map mkUnProp uns
  ]

unsProp :: (Eq a)
        => (V.Vector Double -> a)
        -> (Iteratee [Double] Identity a)
        -> [Double]
        -> Bool
unsProp vec iter xs = if null xs then True
    else vec (V.fromList xs) == (runIdentity $ run =<< enumPure1Chunk xs iter)
    -- we're using Eq for doubles, which is always a bad idea...

    -- also not checking empty vectors, because in some cases (range,
    -- harmonicMean) we get NaN's or other funky values.

mkUnProp (lbl, st, si) = testProperty lbl $ unsProp st si

-- unary properties
uns =
  [ ("mean", St.mean, Si.mean)
  , ("range", St.range, Si.range)
  , ("harmonic_mean", St.harmonicMean, Si.harmonicMean)
  , ("variance", St.fastVariance, Si.variance)
  , ("std_dev", St.fastStdDev,   Si.stdDev)
  ]
