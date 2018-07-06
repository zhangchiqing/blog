module Lib where

import Data.Vector
import Numeric.Search
import Prelude hiding (length)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- > f 10 1
-- True
-- > f 10 4
-- False
-- > f 10 <$> [1..10]
-- [True,True,True,False,False,False,False,False,False,False]
f :: Int -> Int -> Bool
f n x = x * x <= n

-- > traceInput (f 10) 1
-- 1
-- True
traceInput :: (Int -> Bool) -> Int -> IO Bool
traceInput fn x = do
  print x
  return (fn x)

-- > findSqrt 25
-- Just 5
-- > findSqrt 3000000
-- Just 1732
findSqrt :: Int -> Maybe Int
findSqrt t = largest True $ search positiveExponential divForever (f t)

-- > traceFindSqrt 25
-- 1
-- 2
-- 4
-- 8
-- 5
-- 7
-- 6
-- Just 5
traceFindSqrt :: Int -> IO (Maybe Int)
traceFindSqrt t =
  largest True <$> searchM positiveExponential divForever (traceInput (f t))

arr :: Vector Int
arr = fromList [1, 4, 5, 7, 10, 21, 33, 45]

arr' :: Vector Int
arr' = fromList [45, 33, 21, 10, 7, 5, 4, 1]

arr2 :: Vector Int
arr2 = fromList [1, 3, 4, 6, 7, 8, 10, 13, 14, 18, 19, 21, 24, 37, 40, 45, 71]

-- > nthElm arr 4 0
-- True
-- > nthElm arr 4 2
-- False
-- > nthElm arr 4 <$> [0..7]
-- [True,True,False,False,False,False,False,False]
nthElm :: Vector Int -> Int -> Int -> Bool
nthElm v t x = v ! x <= t

-- > bsFindIndex arr 7
-- Just 3
-- > bsFindIndex arr 33
-- *** Exception
bsFindIndex :: Vector Int -> Int -> Maybe Int
bsFindIndex v n =
  case found of
    Nothing -> Nothing
    Just i ->
      if n == v ! i
        then Just i
        else Nothing
  where
    found :: Maybe Int
    found = largest True $ search nonNegativeExponential divForever (nthElm v n)

-- > bsFindIndexFromTo arr 7
-- Just 4
-- > bsFindIndexFromTo arr <$> [1..10]
-- [Just 0,Nothing,Nothing,Just 1,Just 2,Nothing,Just 3,Nothing,Nothing,Just 4]
bsFindIndexFromTo :: Vector Int -> Int -> Maybe Int
bsFindIndexFromTo v n =
  case found of
    Nothing -> Nothing
    Just i ->
      if n == v ! i
        then Just i
        else Nothing
  where
    lastIndex = length v - 1
    found :: Maybe Int
    found = largest True $ search (fromTo 0 lastIndex) divForever (nthElm v n)

-- > bsFindIndexAsc arr' 7
-- Just 4
-- > bsFindIndexAsc arr' <$> [1..10]
-- [Just 7,Nothing,Nothing,Just 6,Just 5,Nothing,Just 4,Nothing,Nothing,Just 3]
bsFindIndexAsc :: Vector Int -> Int -> Maybe Int
bsFindIndexAsc v n =
  case found of
    Nothing -> Nothing
    Just i ->
      if n == v ! i
        then Just i
        else Nothing
  where
    lastIndex = length v - 1
    found :: Maybe Int
    found = smallest True $ search (fromTo 0 lastIndex) divForever (nthElm v n)

-- > traceFindIndex arr 7
-- 0
-- 1
-- 2
-- 4
-- 2
-- 3
-- Just 3
traceFindIndex :: Vector Int -> Int -> IO (Maybe Int)
traceFindIndex v n = do
  found <-
    largest True <$>
    searchM nonNegativeExponential divForever (traceInput (nthElm v n))
  case found of
    Nothing -> return Nothing
    Just i ->
      if n == v ! i
        then return $ Just i
        else return Nothing

traceBSFindIndex :: Vector Int -> Int -> IO (Maybe Int)
traceBSFindIndex v n = do
  found <-
    largest True <$>
    searchM (fromTo 0 lastIndex) divForever (traceInput (nthElm v n))
  case found of
    Nothing -> return Nothing
    Just i ->
      if n == v ! i
        then return $ Just i
        else return Nothing
  where
    lastIndex = length v - 1
