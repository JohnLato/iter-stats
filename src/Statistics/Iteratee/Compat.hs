{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}

module Statistics.Iteratee.Compat (
  ListLikey
)

where

import Data.Iteratee as I
import Data.ListLike (ListLike)

#if MIN_VERSION_iteratee(0,9,0)
type ListLikey s el = (ListLike s el)
#else
type ListLikey s el = (ListLike s el, Nullable s)
#endif
