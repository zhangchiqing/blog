---
title: Writing a binary search solution in one-liner in Haskell
author: Leo Zhang
tags: functional programming, algorithms, binary search, haskell
---

## Motivation
While I was doing the 2018 Google Code Jam Code challenge, I noticed the binary search problem appeared in the first two rounds. [The 4th problem "Cubic UFO" in Qualification round](https://codejam.withgoogle.com/2018/challenges/00000000000000cb/dashboard/00000000000079cc) and [the 2nd problem "Bit Party" in Round 1A](https://codejam.withgoogle.com/2018/challenges/0000000000007883/dashboard/000000000002fff6) are both binary search problems.

Even though the ideas to solve them are similar, they are still slightly different in terms of the exit condition, midpoint calculation and data types, etc. I ended up implementing [two](https://github.com/zhangchiqing/google-code-jam-2018/blob/master/src/GCJ2018QDD.hs#L87-L98) binary search [algorithms](https://github.com/zhangchiqing/google-code-jam-2018/blob/master/src/GCJ2018R1B.hs#L62-L77), each of them took me nearly 1 hour to get all the details right.

After the challenge, I was wondering: since the idea of the solutions are similar, is there a generic approach to the binary search, so that most of the iteration logic can be shared and could be less stressful to implement a solution?

After some research, I discovered [a great library in Haskell called "binary-search"](https://hackage.haskell.org/package/binary-search), which can provides reusable functions to do binary search and exponential search. The author "Ross Paterson" and "Takayuki Muranushi" have considered a lot of variations and the API is so concise and inspiring that I decided to share what I’ve learned.

## Binary search and Exponential search problem

A classic binary search problem is finding the index of an element in a sorted array. For example, finding the index of `7` in `[1,3,4,6,7,8,10,13,14,18,19,21,24,37,40,45,71]`. The binary search algorithm will check the value at position `0`, `16`, `8`, `3`, `5`, `4`. And found the index of `7` is `4`.

Each iteration of the algorithm cuts the search space into half, therefore the algorithm is `O(logN)`, `N` being the length of the array.

However, it's also possible that the item you are looking for doesn't exist in the sorted array. The algothrim should consider such case as well.

Another variation, for example, is to find the closest square root of a given number. To find the closest square root of 26, it tries `1`, `2`, `4`, `8`, `6`, and `5`, since `6` has been tried before, there is no better guess, then `5` is the closest square root of `26`. Since the search process starts with expanding the search space exponentially, and then shrink, it's also called "Exponential search"

## The key of finding a binary search solution
To find the closest square root of a given number, with the binary-search library, the solution could be as simple as this one-liner:
```
> largest  True  $ search positiveExponential divForever (\x -> x * x <= 26)
Just 5
```

How does it work? I will explain it in a bit.

But I will first talk about the last function `(\x -> x * x <= 26)` which is the key to make the binary search solution.

If we name this function as `f`, then this function has such signature

```haskell
f :: Int -> Bool
f x = x * x <= 26
```

And we call this `f` function "predicate function". A predicate function is a function that takes some input and returns a Boolean value.

## What's special with the predicate function?

If we map this `f` function over a list of possible `x`, then we get a new list that shows if its square is less than 26.

```
> f <$> [1..15]
[True,True,True,True,True,False,False,False,False,False,False,False,False,False,False]
```

Observe the result, it’s all `True` from the beginning, and all `False` from a certain position onward. This pattern is very important, if we can find a predict function that returns a list of Boolean, such that for all possible `x`, it's all `True`, and then all `False` from a certain position, then our answer is normally around the position where the result turned from `True` to `False`, or the other way around.

Let's go back to our example.

Here, our predicate function is `f`, and the position of the last `True` is `5`, so the answer of the closest square root integer of `26` is `5`.

With the `f` function in mind, let's see how the binary search solution works:

## How the binary search solution works
Here is the one-liner solution to find the closest square root of `26`:

```
> largest True $ search positiveExponential divForever f
Just 5
```

The `search` function is a generic binary search function, it takes a search range, a splitter, a predicate function, and returns a range of search result.

```haskell
search :: Eq b => SearchRange a -> Splitter a -> (a -> b) -> [Range b a]
```

### Search Range
The SearchRange value we used is `positiveExponential`. It means the search range will start from `1`, which is the smallest positive int, and grows exponentially `2`, `4`, `8` ....

You can choose other `SearchRange` function, for instance, `fromTo`, which specifies the search boundaries with two numbers
```
> largest  True $ search (fromTo 1 100) divForever f
Just 5
```

However, if the search boundary doesn't include the answer, you will get a `Nothing`
```
> largest True $ search (fromTo 20 100) divForever f
Nothing
```

### The Spliter
The SearchRange value is for growing the search space exponentially, once the predicate function returns the first `False`, then the `search` function needs the `Splitter` function to know how to shrink the search space.

The Splitter value we used is `divForever`, which means when the predicate function found a first `False`, the search space will be divided into half forever until the mid-point value has been seen before.

### Choose the answer
In the end, the returned search range gives you an option to choose which value to return.

Recall the list of result that we applied `f` with all possible `x` in increasing order
```
> f <$> [1..15]
[True,True,True,True,True,False,False,False,False,False,False,False,False,False,False]
```

`largest True` will return the largest `x` value which will let the predicate function to return `True`.

```
> largest True $ search positiveExponential divForever f
Just 5
```

And we can also try `smallest False`, then it will return `6`, because `f x == False`
```
> smallest False $ search positiveExponential divForever f
Just 6
```

## Generalize into finding the square root of any number
We were experimenting the API to find the square root of `26`, we can easily generalize it by providing a different function that takes a given number and returns predicate function

```haskell
f :: Int -> Int -> Bool
f n x = x * x <= n
```

Finding the square root of `26` would be:
```
> largest True $ search positiveExponential divForever (f 26)
Just 5
```

And a generalized function to find the square root of any number:

```haskell
findSqrt :: Int -> Maybe Int
findSqrt n = largest True $ search positiveExponential divForever (f n)
```

```
> findSqrt 30000000
Just 1732
```

## How to check it's really doing binary search?
If we would like to see which number was used in each iteration, we can print the number out in the predicate function. Since the predicate introduces side effect, we need to use `searchM` instead which can take predicate functions that returns an `IO Boolean` value.

```haskell
searchM :: forall a m b. (Functor m, Monad m, Eq b) => SearchRange a -> Splitter a -> (a -> m b) -> m [Range b a]
```

In our case of finding the square root of an Int, the `searchM`'s type signature is
```haskell
searchM :: SearchRange Int -> Splitter Int -> (Int -> IO Bool) -> IO [Range Int Int]
```

With the following `traceInput`, we can print the number in each iteration.
```haskell
traceInput :: (Int -> Bool) -> Int -> IO Bool
traceInput fn x = do
  print x
  return (fn x)

traceFindSqrt :: Int -> IO (Maybe Int)
traceFindSqrt t =
  largest True <$> searchM positiveExponential divForever (traceInput (f t))
```

```
> traceFindSqrt 30000000
1
2
4
8
16
32
64
128
256
512
1024
2048
1025
1537
1793
1665
1729
1761
1745
1737
1733
1731
1732
Just 1732
```

The iteration expanded the search range exponentially until `2048`, then started shrinking the search range all the way until get `1732`.

## Exercise
As an exercise, you can try to implement a binary search solution for finding the index of an item in a sorted array.

Hint 1: The List structure in Haskell takes `O(N)` to access an item by index. Use `Data.Vector` instead as array implement in Haskell, which takes `O(1)` to access by index.

Hint 2: You should use a different SearchRange other than `positiveExponential` because accessing an array with an index that is out of boundary will throw exception.

## Conclusion
Binary/Exponential search takes O(logN) time, which is very fast.  Binary search problems have lots of variations. Implementing them right is tricky as a lot of details require attention. Since the ideas of Binary search and exponential search are similar, the binary-search library in Haskell provides reusable functions to help you build your own binary/exponential search solutions. And the key is to find a specific predicate function. With that, a binary search solution can be implemented as simple as an one-liner.
