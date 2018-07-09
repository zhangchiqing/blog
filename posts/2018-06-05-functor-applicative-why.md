---
title: Functor, Applicative and Why
author: Leo Zhang
tags: functional programming, functor, applicative, haskell
---

## Introduction
This blog post will explain two core concepts in Haskell –– Functor and Applicative, which also exist in many other functional languages. Functor and Applicative are great abstractions that allow us to reuse lots of code.

However, when I was learning these concepts, it was difficult for me. Not just because they're abstract, but because most of the articles and posts I read about Functor and Applicative start by introducing their definition and then give examples.

I feel it should be the other way around. It's easier to learn abstract things by seeing concrete instances or use cases of them.

So in this post, I'm starting with concrete examples then tying them back to the definitions of Functor and Applicative.

## Input validation
Let's say we have a `greet` funcition in Javascript that takes a user object and returns a string:
```js
var greet = function(user) {
  return "Hello " + user.name;
};
```

If we try it in the node REPL, it should work.
```
> greet({ name: "Alice" })
'Hello Alice'
```

However, if we enter an `undefined` value, it will throw an exception
```
> greet(undefined)
TypeError: Cannot read property 'name' of undefined
...
```

Why would the `user` be `undefined`?

Well, Javascript is an untyped language, so any variable could be `undefined`. This function assumes the input is not `undefined`. But it's common for developers to forget about this assumption.

In order to handle the `undefined` input, we could wrap this function with a function that validates the input:

```js
var checkAndGreet = function(user) {
  if (!user) {
    return undefined;
  }
  return greet(user);
};
```

So if the user is `undefined`, we wouldn't pass it to `greet`, but short circuit to return `undefined`.

## `Maybe` type in Haskell
Let's see how this case is handled in Haskell.

First, let's define the `User` type and the `greet` function.

```haskell
data User = User String deriving (Show)

greet :: User -> String
greet (User name) = "Hello " ++ name
```

Test it in GHCi:
```
> User "Alice"
User "Alice"

> greet (User "Alice")
"Hello Alice"
```

In Haskell, this case is handled by a data type called `Maybe`. This is the type declaration of `Maybe`, which says the generic `Maybe` type is either a `Just` value that contains other values, or a constant `Nothing` value.

```haskell
data Maybe a = Nothing | Just a deriving (Eq, Ord)
```

So `Maybe User` is a type that can present two cases, either `Nothing` or a `Just User`.

If the input is a `Maybe User`, then we can make a `checkAndGreet` to take that and return a `Maybe String`.

```haskell
checkAndGreet :: Maybe User -> Maybe String
checkAndGreet Nothing = Nothing
checkAndGreet (Just user) = Just (greet user)
```

The first line defines the `Maybe`

Let's try it in GHCi
```
> checkAndGreet Nothing
Nothing

> user = User "Alice"

> checkAndGreet (Just user)
Just "Hello Alice"
```

## How does it prevent mistakes?

Where would we get a `Maybe User`?

Well, let's say we don't want the user name to be empty, so we can create a `validateAndMakeUser` function to check if the name is empty and returns a `Maybe User`.

```haskell
validateAndMakeUser :: String -> Maybe User
validateAndMakeUser "" = Nothing
validateAndMakeUser name = Just (User name)
```

Let's look at the mistake we had before.

If we have a user name of `String` and we forget to validate the name and pass it directly to `greet`, the Haskell compiler won't allow it. The compiler will say `greet` takes a `User`, which is not a `String`.

```
> greet "Alice"
error:
    • Couldn't match expected type ‘User’ with actual type ‘[Char]’
    • In the first argument of ‘greet’, namely ‘"Alice"’
      In the expression: greet "Alice"
      In an equation for ‘it’: it = greet "Alice"
```

If we remember to validate the name, and now we have a `Maybe User` value, but still we passed it to `greet` instead of `checkAndGreet`, then the code won't compile either. Because `Maybe User` and `User` are different types.

```
> greet (Just (User "Alice"))

<interactive>:20:8: error:
    • Couldn't match expected type ‘User’ with actual type ‘Maybe User’
    • In the first argument of ‘greet’, namely ‘(Just (User "Alice"))’
      In the expression: greet (Just (User "Alice"))
      In an equation for ‘it’: it = greet (Just (User "Alice"))
```

## Extract the input validation part into a function
Alright. Let's say we have other functions that tak `User` and returns a different `String`, for instance, a `bye` function:

```haskell
bye :: User -> String
bye (User name) = "Goodbye " ++ name
```

However, the input value we have is a `Maybe User` value, not a `User`. How can I take the `User` from the `Maybe User` and pass it to the `bye` function?

Then we can wrap it the same way as `checkAndGreet` to make a `checkAndBye`.

```haskell
checkAndBye :: Maybe User -> Maybe String
checkAndBye Nothing = Nothing
checkAndBye (Just user) = Just (bye user)
```

As you probably noticed, the `checkAndGreet` and `checkAndBye` are very similar. We're kind of repeating the logic here.

We could extract the common part into a function, and use that to make `checkAndGreet` and `checkAndBye`. Let's name this common function `mapUser`.

```haskell
mapUser :: (User -> String) -> Maybe User -> Maybe String
mapUser f Nothing = Nothing
mapUser f (Just user) = Just (f user)

checkAndGreet :: Maybe User -> Maybe String
checkAndGreet maybeUser = mapUser greet maybeUser

checkAndBye :: Maybe User -> Maybe String
checkAndBye maybeUser = mapUser bye maybeUser
```

We can further refactor them into point-free style:
```haskell
checkAndGreet :: Maybe User -> Maybe String
checkAndGreet = mapUser greet

checkAndBye :: Maybe User -> Maybe String
checkAndBye = mapUser bye
```

## Generalize the `mapUser` function
Let's take a look at the `mapUser` function again. Even though this function is called `mapUser`, it didn't actually use anything special about `User`, nor about `String`

```haskell
mapUser :: (User -> String) -> Maybe User -> Maybe String
mapUser f Nothing = Nothing
mapUser f (Just user) = Just (f user)
```

If we replace `User` with a generic type `a` and replace `String` with a generic type `b`, then `mapUser` is equivalent to the `mapMaybe` function as below:

```haskell
mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f Nothing = Nothing
mapMaybe f (Just a) = Just (f a)
```

Then we can refactor the `checkAndGreet` and `checkAndBye` with the `mapMaybe` function, and it will still type-check and work.
```haskell
checkAndGreet :: Maybe User -> Maybe String
checkAndGreet = mapMaybe greet

checkAndBye :: Maybe User -> Maybe String
checkAndBye = mapMaybe bye
```

Now we have a generic `mapMaybe` function that can deal with a case where the input is empty (`Nothing`).

With Haskell's strong type system, we can just write functions that process on concrete types, and if we need those functions to process the Maybe values we get them from DB calls or HTTP request, we can now just wrap the function with `mapMaybe` to get a new function that is able to process over `Maybe` values.

The `mapMaybe` function can be implemented in Javascript too. Given there is no `Maybe` type, we would just call it `mapNullable`:

```javascript
var mapNullable = function(f) {
  return function(v) {
    if (v === null || v === undefined) {
      return null;
    }
    return f(v);
  };
};

var greet = function(user) {
  return "Hello " + user.name;
};

var bye = function(user) {
  return "Bye " + user.name;
};

var checkAndGreet = mapNullable(greet);
var checkAndBye = mapNullable(bye);

console.log(checkAndGreet({ name: 'Alice' })); // "Hello Alice"
console.log(checkAndBye(undefined)); // undefined
```

## Map over 2 `Maybe` values
Let's back to our Haskell version. We have a generic `mapMaybe` to map a function over a `Maybe` value. But it seems to only work for functions that take just one argument.

```haskell
mapMaybe :: (a -> b) -> Maybe a -> Maybe b
```

What if my function takes more than one argument? Is it possible to have a function that maps over multiple `Maybe` values? For example, `map2Maybes` and `map3Maybes` with the following type signatures:

```haskell
map2Maybes :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
map3Maybes :: (a -> b -> c -> d) -> Maybe a -> Maybe b -> Maybe c -> Maybe d
...
```

And let's call a function that takes `N` arguments an "N-arity" function. So `mapMaybe` is 1-arity function, and `map2Maybes` is 2-arity function, `map3Maybes` is 3-arity function.

If we have the `map2Maybes` function, then we can use it to wrap a function that takes two `Maybe User` values as input.

```haskell
showParents :: User -> User -> String
showParents (User fatherName) (User motherName) = "Father is " ++ fatherName ++ " and mother is " ++ motherName

checkAndShowParents :: Maybe User -> Maybe User -> Maybe String
checkAndShowParents maybeFather maybeMother = map2Maybes showParents maybeFather maybeMother
```

And the `map2Maybes` is not hard to implement:
```haskell
map2Maybes :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
map2Maybes _ Nothing _ = Nothing
map2Maybes _ _ Nothing = Nothing
map2Maybes f (Just a) (Just b) = Just (f a b)
```

Similarly, you can implement `map3Maybes`, `map4Maybes` ... We can make as many as we want. However, we still need to write them manually each time for wrapping an N-arity function.

Is it possible to have a generic `mapNMaybes` function that works for functions that take any number of arguments?

Let’s find out.

## Map over N Maybe values

In Haskell, every function is curried. We can pass a value to a 2-arity function to get a new 1-arity function.

```
> :t showParents (User “Bob”)
showParents (User “Bob”) :: User -> String
```

Therefore, `showParents`'s type signature could also be written as:

```haskell
showParents :: User -> (User -> String)
```

If we treat the 1-arity function `(User -> String)` as a value, then `showParents` can be passed to `mapMaybe` and it will return a `Maybe (User -> String)` type

```
> :t mapMaybe showParents (Just (User “Bob”))
mapMaybe showParents (Just (User “Bob”)) :: Maybe (User -> String)
```

But wait a second, can a `Maybe` type contain a function?

Yes, why not? `Maybe a` is a generic type, and it can take any concrete type to make a new type. Since a function is also a concrete type, it can be wrapped in a `Maybe` value too. And it doesn't matter how many arguments it takes.

For instance, we can just pass any function to one of the Maybe type constructor `Just`:

```
> :t Just greet
Just greet :: Maybe (User -> String)

> :t Just showParents
Just showParents :: Maybe (User -> User -> String)
```

But what can we do with a `Maybe (User -> String)` value?

Well, let's take a look at what we need. We’d like to implement a `checkAndShowParents` function.

```haskell
checkAndShowParents :: (User -> User -> String) -> Maybe User -> Maybe User -> Maybe String
```

Since we've got
```haskell
mapMaybe :: (User -> (User -> String)) -> Maybe User -> Maybe (User -> String)
```
Or
```haskell
mapMaybe :: (User -> User -> String) -> Maybe User -> Maybe (User -> String)
```

If we have another function with the following type signature, let's call it `applyShowParents` for now.
```haskell
applyShowParents :: Maybe (User -> String) -> Maybe User -> Maybe String
```

Then we can composite it with the `mapMaybe` to make `checkAndShowParents`:
```haskell
checkAndShowParents :: Maybe User -> Maybe User -> Maybe String
checkAndShowParents maybeFather maybeMother = applyShowParents (mapMaybe showParents maybeFather) maybeMother

applyShowParents :: Maybe (User -> String) -> Maybe User -> Maybe String
applyShowParents Nothing _ = Nothing
applyShowParents _ Nothing = Nothing
applyShowParents (Just f) (Just user) = Just (f user)
```

## Generalize to `applyMaybe`
Again, we can generalize the `applyShowParents` as well into a generic `applyMaybe` function, because the function body doesn't need anything special from either `User` or `String`.

```haskell
applyMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b
applyMaybe Nothing _ = Nothing
applyMaybe _ Nothing = Nothing
applyMaybe (Just f) (Just a) = Just (f a)
```

And the `checkAndShowParents` can be implemented by compositing `mapMaybe`, `applyMaybe` and `showParents`

```haskell
checkAndShowParents :: Maybe User -> Maybe User -> Maybe String
checkAndShowParents maybeFather maybeMother = applyMaybe (mapMaybe showParents maybeFather) maybeMother
```

## Generalize into N-arity function
Now that we can map over 2-arity functions, can it apply to N-arity functions?

Yes. Observe the `applyMaybe`'s type signature and how we can use it for a 3-arity functions or N-arity too:

```haskell
applyMaybe :: Maybe (a -> b -> c) -> Maybe a -> Maybe (b -> c)
applyMaybe :: Maybe (a -> b -> c -> d) -> Maybe a -> Maybe (b -> c -> d)
applyMaybe :: Maybe (a -> b -> c -> d -> e) -> Maybe a -> Maybe (b -> c -> d -> e)
...
```

And with the following functions we can reduce a `Maybe` N-arity function value into a `Maybe` (N-1)-arity function value, which can be further reduced all the way to a `Maybe` value.

```
> add3 a b c = a + b + c + (1 :: Int)

> :t add3
add3 :: Int -> Int -> Int -> Int

> :t applyMaybe (Just add3) (Just 1)
applyMaybe (Just add3) (Just 1) :: Maybe Int -> Maybe (Int -> Int -> Int)

> :t applyMaybe (applyMaybe (Just add3) (Just 1)) (Just 2)
:t applyMaybe (Just add3) (Just 1) :: Maybe Int -> Maybe (Int -> Int)

> :t applyMaybe (applyMaybe (applyMaybe (Just add3) (Just 1)) (Just 2)) (Just3)
:t applyMaybe (applyMaybe (applyMaybe (Just add3) (Just 1)) (Just 2)) (Just3) :: Maybe Int
```

The above expression looks a bit messy. Let's rewrite it as infix operator.

```
> Just add3 `applyMaybe` (Just 1) `applyMaybe` (Just 2) `applyMaybe` (Just 3)
Just 7

> Just add3 `applyMaybe` Nothing `applyMaybe` (Just 2) `applyMaybe` (Just 3)
Nothing
```

## Pattern of writing input validation for functions
We've made a generic `mapMaybe` function and an `applyMaybe` function that can be used to wrap any N-arity functions. That way, they're able to take values from N `Maybe` and shortcircuit to return `Nothing` if any of the N Maybe values are `Nothing`.

```haskell
greet :: User -> String
greet (User name) = "Hello " ++ name

checkAndGreet :: Maybe User -> Maybe String
checkAndGreet = mapMaybe

showParents :: User -> User -> String
showParents (User fatherName) (User motherName) = "Father is " ++ fatherName ++ " and mother is " ++ motherName

checkAndShowParents :: Maybe User -> Maybe User -> Maybe String
checkAndShowParents maybeFather maybeMother = showParents `mapMaybe` maybeFather `applyMaybe` maybeMother

add3 :: Int -> Int -> Int -> Int
add3 a b c = a + b + c + 1

checkAndAdd3 :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int
checkAndAdd3 ma mb mc = add3 `mapMaybe` ma `applyMaybe` mb `applyMaybe` mc
```

You might notice a pattern here: if we want to wrap an `N-arity` function with empty input checks and the function has only 1 argument, we can just use `mayMaybe`. If there is more than 1 argument, we just append (`applyMaybe` argN) in the end.

## Functor and Applicative
OK, with the above examples and practices in mind, now it's time to introduce the term `Functor` and `Applicative`.

What is `Functor`?  `Functor` is a typeclass that essentially defines a list of functions to implement.

`Functor` typeclass defines just one function `fmap` with the following type signature.

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

Recall the `mapMaybe` function we created earlier. It's the `fmap` function for `Maybe` to be a `Functor`. In other words, `Maybe` is a `Functor` because `Maybe` implements `fmap`:

```haskell
mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f Nothing = Nothing
mapMaybe f (Just a) = Just (f a)

instance Functor Maybe where
  fmap = mapMaybe
```

`fmap` is also defined as the inflx operator `<$>`. Therefore, the following two operations are identical:

```haskell
infixl 4 <$>
(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap

fmap greet (Just (User "Alice"))
greet <$> Just (User "Alice")
```

## What is `Applicative`?
`Applicative` is also a typeclass. To be an `Applicative`, the type has also to be a `Functor`.

`Applicative` defines two functions, `pure` and `<*>`, with the following type signature:

```haskell
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```

`Maybe` is an instance of `Applicative`. Refer to the type signature of `Just` and `applyMaybe`: these two functions are `Maybe` type's implementation for being `Applicative`.

```haskell
Just :: a -> Maybe a

applyMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b

class Applicative Maybe where
  pure = Just
  (<*>) = applyMaybe
```

The abstraction of `Functor` and `Applicative` allows more generic functions to be built and reused. For example, the `liftA2` and `liftA3` are generic versions of the `map2Maybes` and `map3Maybes` for `Applicative`.

```haskell
liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
```

Both `liftA2` and `liftA3` can be implemented with just `pure` and `<*>`. You can try to implement them yourself.

### More Functor example
So far we've seen an instance of `Functor` and `Applicative`, which is `Maybe`. Actually, there are a lot more of them defined in the [base module](https://www.stackage.org/haddock/lts-11.12/base-4.10.1.0/Prelude.html#control.i:Functor), and other modules.

The most useful Applicatives (which also means they are `Functor`s) are `Maybe`, `Either`, `IO` and `List`, which means you can use the same functions `fmap`, `(<*>)` on all those types.

For instance, `IO` is also a `Functor` and an `Applicative`. Here is an example of how to read and parse environment variables with the functions provided by `Functor` and `Applicative`.

To read the environment variables by name, there is a `getEnv` function under the namespace `System.Environment` that takes `String` as the env var name and returns the env var as `IO String`.

```haskell
> import System.Environment (getEnv)
> :t getEnv
getEnv :: String -> IO String
```

`IO String` is a computation that might run into an exception. For instance,

```
> getEnv "NAME"
*** Exception: NAME: getEnv: does not exist (no environment variable)

> ("NAME: " ++) <$> getEnv "NAME"
*** Exception: NAME: getEnv: does not exist (no environment variable)

> ("PORT: " ++) <$> getEnv "PORT"
"4567"
```

```haskell
data Config = Config
  { cfgHost :: String
  , cfgPort :: Int
  , cfgDebug :: Bool
  , cfgLogLevel :: Int
  } deriving (Show)

readConfig :: IO Config
readConfig = Config <$> getHost <*> getPort <*> getDebug <*> getLogLevel

getHost :: IO String
getHost = getEnv "HOST"

getPort :: IO Int
getPort = read <$> getEnv "PORT"

getDebug :: IO Bool
getDebug = read <$> getEnv "DEBUG"

getLogLevel :: IO Int
getLogLevel = return 1
```

Notice that the `read` function is a polymorphic parse function that can be either `String -> Int` or `String -> Bool` depending on where it's used.

So if the environment doesn't have `HOST`, then the `IO` will throw an exception:
```
> readConfig
*** Exception: HOST: getEnv: does not exist (no environment variable)
```

If all the environment variables are present, we'll get a `Config` value with all the parsed value in it.
```
> readConfig
Config {cfgHost = "localhost", cfgPort = 4567, cfgDebug = True, cfgLogLevel = 1}
```

## Why?

To summarize my points with a few QnAs.

### So what is `Functor` and `Applicative`?

They are typeclasses (like interfaces in other languages) that define the functions that their type instances have to implement.

### Why do I need `Functor`?

Because we want to reuse code.

`Functor` generalizes how to map a function from one value to another. We used the `Maybe` type as an example to show why and how to use a generic `mapMaybe` function without having to deal with the empty case. And the `mapMaybe` is the implementation of `Functor` for `Maybe` type.

### Why do I need `Applicative`?

Because `Functor` can only map a function which takes one argument. If we have a function that takes multiple arguments, we need `Applicative`.

`Applicative` provides abstraction for how to apply a function that takes multiple arguments over multiple values. We used `applyMaybe`, which is the implementation of `Applicative` for `Maybe` type, as an example to show how to map a function over multiple `Maybe` values without dealing with the case of any value being `Nothing`.

### Why do I need `Functor` and `Applicative` instead of just `mapMaybe` and `applyMaybe`?

There are more instances of `Functor` and `Applicative`. For example, `Maybe`, `Either`, `IO`, `List` are all `Functor` and `Applicative`. You can reuse the same `fmap` and `<*>` function to map functions over those values without having to write `mapMaybe`, `mapEither`, `mapList`, etc.

And functions like [`liftA2`](https://hackage.haskell.org/package/base-4.11.1.0/docs/Control-Applicative.html#v:liftA2), which is defined on top of `Functor` and `Applicative`, can also be reused for free without having to write `may2Maybes`, `map2IOs`, `map2Eithers` etc.

### Conclusion

`Functor` and `Applicative` are two key concepts in functional programming. We used the `Maybe` type and its use cases as examples to introduce the need for abstraction. `Functor` and `Applicative` are abstractions; they define what functions have to be implemented for a type to be an instance of them.

Functional programming has plenty [more abstractions and type classes](https://wiki.haskell.org/File:Typeclassopedia-diagram.png) built on top of `Functor` and `Applicative`. This makes a great amount of code and logic reusable, and Haskell's strong type system ensures that the use of those generic functions are correct and safe.

Thanks for reading this blog post. Hopefully, I made `Functor` and `Applicative` a bit easier to understand.
