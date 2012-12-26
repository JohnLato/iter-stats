{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS -Wall #-}
-- | some functions for working with uniformly-sampled data
module Statistics.Iteratee.Uniform (
  someRollingFunction
, movingAverage
) where

import Statistics.Iteratee.Compat
import Statistics.Iteratee.Sample

import Control.Monad.Identity
import Data.Iteratee as I
import Data.ListLike (ListLike)

#if MIN_VERSION_iteratee(0,9,0)
#else
import qualified Data.ListLike as LL
#endif

#if MIN_VERSION_iteratee(0,9,0)
roll'
    :: (Monad m, ListLike s el)
    => Int  -- ^ length of chunk (t)
    -> Int  -- ^ amount to consume (d)
    -> Iteratee s m [s]
roll' = roll
#else
roll'
    :: (Monad m, Nullable s, ListLike s el)
    => Int  -- ^ length of chunk (t)
    -> Int  -- ^ amount to consume (d)
    -> Iteratee s m [s]
roll' t d
  | t > d  = liftI (go LL.empty)
  | otherwise = error "Iteratee.roll: (t <= d).  Reverse the args?"
    where
        go prev (Chunk vec) =
                let withPrev = prev `LL.append` vec
                in if LL.length withPrev > t
                    then idone [LL.take t withPrev] (Chunk $ LL.drop d withPrev)
                    else liftI (go withPrev)
        go prev e = idone [prev] e
#endif

someRollingFunction
    :: (Monad m, ListLikey s el)
    => Int
    -> (s -> summary)
    -> Enumeratee s [summary] m a
someRollingFunction count mkSummary =
    convStream (roll' count (count-1))
    ><> mapStream mkSummary
{-# INLINABLE someRollingFunction #-}

movingAverage
    :: (Fractional el, Monad m, ListLikey s el)
    => Int
    -> Enumeratee s [el] m a
movingAverage n = someRollingFunction n chunkMean
  where
    chunkMean = runIdentity . (run <=< flip enumPure1Chunk mean)
{-# INLINABLE movingAverage #-}
