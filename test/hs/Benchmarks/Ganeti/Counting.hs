{-| Benchmark to figure out what way to count in performance critical code.

-}

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

module Benchmarks.Ganeti.Counting (benchCounting) where

import Criterion.Main
import Data.List (foldl')

count1 :: [Char] -> Char -> Int
count1 l c = sum $ map (\x -> if x==c then 1 else 0) l

count2 :: [Char] -> Char -> Int
count2 l c = foldl' (\a x -> if x==c then a+1 else a) 0 l

count3 :: [Char] -> Char -> Int
count3 l c = length $ filter (== c) l

examples :: [(String, Char, String)]
examples = [ ("small", 'a', "acaksdfklckajdkfacoijadsf")
           , ("1000", 'a', take 1000 $ repeat 'a')
           , ("10000", 'a', take 10000 $ cycle ['a'..'z'])
           ]

mkBenchmark :: ([Char] -> Char -> Int) -> (String, Char, String) -> Benchmark
mkBenchmark f (name, char, string) = bench name $ nf (uncurry f) (string, char)

benchCounting :: [Benchmark]
benchCounting = [
  bgroup "Counting"
    [ bgroup "count1" $ map (mkBenchmark count1) examples
    , bgroup "count2" $ map (mkBenchmark count2) examples
    , bgroup "count3" $ map (mkBenchmark count3) examples
    ]]
