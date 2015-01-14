{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving, MultiParamTypeClasses,
    FunctionalDependencies #-}


{-| Utility functions for statistical accumulation. -}

{-

Copyright (C) 2014 Google Inc.
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

1. Redistributions of source code must retain the above copyright notice,
this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-}

module Ganeti.Utils.Statistics
  ( SumStatistics(..)
  , StdDevStatistics(..)
  , getSumStatistics
  , getStdDevStatistics
  , Statistics(..)
  ) where

import Data.List (foldl')
import Data.Monoid

square :: Double -> Double
square x = x * x

-- | Abstract class of statistical accumulations. They behave as if the given
-- statistics were computed on the list of values, but they allow a potentially
-- more efficient update of a given value.
--
-- The statistic is a single floating point value. It aggregates over the type
-- b.
class Monoid a => Statistics b a | a -> b where
  getStatisticValue :: a -> Double
  (<->) :: a -> a -> a -- ^ "substraction", such that a <> b <-> b = a
                       -- and  a <> b <-> a = b
  singleton :: b -> a

newtype SumStatistics = SumStatistics Double
  deriving (Show, Num, Eq)

data StdDevStatistics = StdDevStatistics Double Double Double
                  -- count, sum, and not the sum of squares---instead the
                  -- computed variance for better precission.
  deriving (Show, Eq)

instance Monoid SumStatistics where
  mempty = SumStatistics 0
  mappend = (+)

instance Statistics Double SumStatistics where
  getStatisticValue (SumStatistics a) = a
  (<->) = (-)
  singleton = SumStatistics

instance Monoid StdDevStatistics where
  mempty = StdDevStatistics 0 0 0
  mappend !left !right =
    let StdDevStatistics n1 sum1 sumsq1 = left
        StdDevStatistics n2 sum2 sumsq2 = right
        nom = square (sum1 * n2 - sum2 * n1)
        sumsq = if n1 == 0 || n2 == 0
                then sumsq1 + sumsq2
                else sumsq1 + sumsq2 + nom/((n1 + n2) * n1 * n2)
    in StdDevStatistics (n1+n2) (sum1+sum2) sumsq

instance Statistics Double StdDevStatistics where
  getStatisticValue (StdDevStatistics n _ sumsq) = sqrt (sumsq / n)
  left <-> right =
    let StdDevStatistics nab sumab sumsqab = left
        StdDevStatistics nb sumb sumsqb = right
        na = nab - nb
        suma = sumab - sumb
        sumsqa = sumsqab - sumsqb - (square $ suma*nb - sumb * na)/(na*nb*(na+nb))
        cut0 x
          | x < 0 = 0
          | otherwise = x
    in StdDevStatistics (cut0 na) (cut0 suma) (cut0 sumsqa)
  singleton f = StdDevStatistics 1 f 0

-- | Get a statistics that sums up the values.
getSumStatistics :: [Double] -> SumStatistics
getSumStatistics = SumStatistics . sum

-- | Get a statistics for the standard deviation.
getStdDevStatistics :: [Double] -> StdDevStatistics
getStdDevStatistics xs =
  let (nt, st) = foldl' (\(n, s) x ->
                            let !n' = n + 1
                                !s' = s + x
                            in (n', s'))
                 (0, 0) xs
      mean = st / nt
      nvar = foldl' (\v x -> let d = x - mean in v + d * d) 0 xs
  in StdDevStatistics nt st (nvar / nt)
