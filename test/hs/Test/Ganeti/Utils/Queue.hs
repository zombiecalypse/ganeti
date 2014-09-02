{-# LANGUAGE TemplateHaskell, CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Ganeti.Utils.Queue(testUtils_Queue) where

import Data.Set (Set(..), fromList)
import Data.Foldable (toList)
import Data.List
import Data.Functor
import Data.Maybe (fromMaybe)

import Test.QuickCheck
import Test.HUnit (assert)

import Test.Ganeti.TestHelper
import Test.Ganeti.TestCommon

import Ganeti.Utils.Queue

uniq = toList . fromList

arbitraryNames :: Gen [String]
arbitraryNames = uniq <$> arbitrary

arbitraryQueue :: Gen PriorityQueue
arbitraryQueue = do
  names <- arbitraryNames
  prios <- vector (length names)
  return . sort $ zip prios names

prop_mkQueue_allDue :: Property
prop_mkQueue_allDue = forAll arbitraryNames $ \names ->
  let queue = mkQueue names
  in  due queue ==? names

prop_decrement_orderConstant :: PriorityQueue -> Integer -> Property
prop_decrement_orderConstant queue i =
  let names = map snd
  in  names queue ==? names (decrement queue i)

prop_updateQueue_noNewQueueMembers :: Property
prop_updateQueue_noNewQueueMembers = forAll arbitraryQueue $ \queue ->
  not (null queue) ==>
    forAll (elements queue) $ \(_, item) ->
    forAll arbitrary $ \prio ->
      let queue' = updateQueue queue [(prio, item)]
      in  fromList (map snd queue') ==? fromList (map snd queue)

prop_updateQueue_sorted :: Property
prop_updateQueue_sorted = forAll arbitraryQueue $ \queue ->
  not (null queue) ==>
    forAll (elements queue) $ \(prio, item) ->
      map snd (updateQueue queue [(prio, item)]) ==? map snd (sort queue)

prop_due_negative :: Property
prop_due_negative = forAll arbitraryQueue $ \queue ->
  (not . null $ due queue) ==>
    forAll (elements (due queue)) $ \name ->
      case find ((==) name . snd) queue of
        Nothing -> failTest $ "Not element: " ++ name
        Just (x, _) ->
          if x <= 0
            then passTest
            else failTest $ "Positive priority " ++ show x ++ " is due"

prop_decrement_moreDue :: Positive Integer -> Property
prop_decrement_moreDue (Positive i) = forAll arbitraryQueue $ \queue ->
  let queue' = decrement queue i
  in  if length (due queue') >= length (due queue)
        then passTest
        else failTest "Not at least as many due."

testSuite "Utils/Queue"
            [ 'prop_mkQueue_allDue
            , 'prop_decrement_orderConstant
            , 'prop_updateQueue_noNewQueueMembers
            , 'prop_updateQueue_sorted
            , 'prop_due_negative
            , 'prop_decrement_moreDue
            ]
