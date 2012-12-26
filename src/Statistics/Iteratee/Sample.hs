{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}

{-# OPTIONS -Wall #-}
module Statistics.Iteratee.Sample (
  minMaxNBy
, range
, mean
, harmonicMean
, variance
, stdDev
) where

import Statistics.Iteratee.Compat
import Control.Arrow
import Control.Monad
import Data.Iteratee as I

import Data.Heap as Heap

--  | /O(n)/ minMaxNBy. Calculate the 'n' highest and lowest elements of the
--  stream, according to the given priority function.
--  Returns /([minimum],[maximum]/ with the /(minimum,maximum)/
--  elements listed first.
minMaxNBy
  :: forall m prio s el. (Monad m, Ord prio, ListLikey s el)
  => Int
  -> (el -> prio)
  -> Iteratee s m ([(prio,el)],[(prio,el)])
minMaxNBy ns prio = finalize `liftM` I.foldl' step (Heap.empty,Heap.empty)
  where
    finalize :: (MaxPrioHeap prio el, MinPrioHeap prio el)
             -> ([(prio,el)],[(prio,el)])
    finalize = Heap.toDescList *** Heap.toDescList
    addHeap val = Heap.insert (prio val, val)
    step :: (MaxPrioHeap prio el, MinPrioHeap prio el) -> el
            -> (MaxPrioHeap prio el, MinPrioHeap prio el)
    step (!mins,!maxes) val = let sz = Heap.size mins
                                  adj hp = if sz >= ns
                                             then Heap.drop 1 hp
                                             else hp
                              in (adj $ addHeap val mins
                                 ,adj $ addHeap val maxes)
{-# INLINE minMaxNBy #-}

-- | /O(n)/ Range. The difference between the largest and smallest elements of
-- a stream.
range :: (Monad m, ListLikey s el, Num el, Ord el)
      => Iteratee s m el
range = finalize `liftM` minMaxNBy 1 id
  where
    finalize ([mins],[maxes]) = snd maxes - snd mins
    finalize _ = 0
{-# INLINE range #-}

-- | /O(n)/ Arithmetic mean.  Uses Welford's algorithm.
mean :: forall s m el. (Fractional el, Monad m, ListLikey s el)
     => Iteratee s m el
mean = fst `liftM` I.foldl' step (0,0)
  where
    step :: (el,Integer) -> el -> (el,Integer)
    step (!m,!n) x = let m' = m + (x-m) / fromIntegral n'
                         n' = n + 1
                     in  (m',n')
{-# INLINE mean #-}

-- | /O(n)/ Harmonic mean.
harmonicMean :: (Fractional el, Monad m, ListLikey s el) => Iteratee s m el
harmonicMean = finalize `liftM` I.foldl' step (0,0 :: Integer)
  where
    finalize (m,n) = fromIntegral n / m
    step (!m,!n) val = (m+(1/val),n+1)
{-# INLINE harmonicMean #-}

-- | /O(n)/ variance, using Knuth's algorithm.
var :: (Fractional el, Integral t, Monad m, ListLikey s el)
    => Iteratee s m (t, el, el)
var = I.foldl' step (0,0,0)
  where
    step (!n,!m,!s) x = let n' = n+1
                            m' = m+d/fromIntegral n'
                            s' = s+d* (x-m')
                            d  = x-m
                        in (n',m',s')
{-# INLINE var #-}

-- | /O(n)/ Maximum likelihood estimate of a sample's variance, using Knuth's
--   algorithm.
variance :: (Fractional b, Monad m, ListLikey s b) => Iteratee s m b
variance = finalize `liftM` var
  where
    finalize (n,_,s)
      | n > 1 = s / fromInteger n
      | otherwise = 0
{-# INLINE variance #-}

-- | /O(n) Standard deviation, using Knuth's algorithm.
stdDev :: (Floating b, Monad m, Functor m, ListLikey s b) => Iteratee s m b
stdDev = sqrt `liftM` variance
{-# INLINE stdDev #-}
