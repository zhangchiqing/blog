---
title: Functor, Applicative and Why
author: Leo Zhang
tags: functional programming, functor, applicative, haskell
---

## Introduction
This blog post is trying to explain two core concepts in Haskell - `Functor` and `Applicative`, which also exist in many other functional languages. `Functor` and `Applicative` are great abstractions that allows us to reuse lots of code.

However, when I was learning these concepts, it was difficult for me. Not just they are abstract, but because most of the articles and posts I read starting from introducing their definition, then giving examples.

I feel it should be the other round. It's easier to learn abstract things by seeing enough concrete instances or use cases of them.

So in this post, I'm trying to start with some concrete examples and use cases then relate them to the definition of `Functor` and `Applicative`.

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

However, if we pass in an `undefined` value, it will throw an exception
```
> greet(undefined)
TypeError: Cannot read property 'name' of undefined
...
```

Why the `user` would be `undefined`?

Well, Javascript is a untyped language, any variable could be `undefined`. This function assumes the input is not `undefined`. But it's common that we forgot about this assumption.

In order to handle the `undefined` input, we could wrap this function with a function that validates the input:

```js
var checkAndGreet = function(user) {
  if (!user) {
    return undefined;
  }
  return greet(user);
};
```

So if the user is `undefined`, we wouldn't pass it to `greet`, but shortcircuit to return `undefined`.

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

In Haskell, this case is handled by a data type called `Maybe`.

`Maybe User` is a a type can presents two cases that can be "Nothing" or a user.

If the input is a `Maybe User`, then we can make a `checkAndGreet` to take that and returns a `Maybe String`.

```haskell
data Maybe a = Nothing | Just a

checkAndGreet :: Maybe User -> Maybe String
checkAndGreet Nothing = Nothing
checkAndGreet (Just user) = Just (greet user)
```

Let's try it in GHCi
```
> checkAndGreet Nothing
Nothing

> user = User "Alice"

> checkAndGreet (Just user)
"Hello Alice"
```

## How does it prevent our mistakes

Where would we get a `Maybe User`?

Well, let's say we don't want the user name to be empty, so we can create a `validateAndMakeUser` function to check if the name is empty and returns a `Maybe User`.

```haskell
validateAndMakeUser :: String -> Maybe User
validateAndMakeUser "" = Nothing
validateAndMakeUser name = Just (User name)
```

Let's look at our mistake we had before.

If we have a user name as `String`, and we forget to validate the name and pass it directly to `greet`, the Haskell compiler won't allow it. The compiler will say `greet` takes a `User`, which is not a `String`.

```
> greet "Alice"
error:
    • Couldn't match expected type ‘User’ with actual type ‘[Char]’
    • In the first argument of ‘greet’, namely ‘"Alice"’
      In the expression: greet "Alice"
      In an equation for ‘it’: it = greet "Alice"
```

If we remember to validate the name, and now we have a `Maybe User` value, but still we passed it `greet` instead of `checkAndGreet`, then the code won't compile either. Because `Maybe User` and `User` are different types.

```
> greet (Just (User "Alice"))

<interactive>:20:8: error:
    • Couldn't match expected type ‘User’ with actual type ‘Maybe User’
    • In the first argument of ‘greet’, namely ‘(Just (User "Alice"))’
      In the expression: greet (Just (User "Alice"))
      In an equation for ‘it’: it = greet (Just (User "Alice"))
```

## Extract the input validation part into a function
Alright. Let's say we have other functions that takes `User` and returns a different `String`, for instance, a `bye` function:

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

As you notice, the `checkAndGreet` and `checkAndBye` are very similar. We kind of repeating the logic here.

We could extract the common part into a function, and use that to make `checkAndGreet` and `checkAndBye`. And let's name this common function as `mapUser`.

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
Let's take a look at the `mapUser` again. Even though this function called `mapUser`, but this function didn't actually use anything special about `User`, nor about `String`

```haskell
mapUser :: (User -> String) -> Maybe User -> Maybe String
mapUser f Nothing = Nothing
mapUser f (Just user) = Just (f user)
```

If we replace `User` with a generic type `a` and replace `String` with a generic type `b`, then `mapUser` is equivlent to the `mapMaybe` function as below:

```haskell
mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f Nothing = Nothing
mapMaybe f (Just a) = Just (f a)
```

And we can refactor the `checkAndGreet` and `checkAndBye` with `mapMaybe`, and it will still type-check and work.
```haskell
checkAndGreet :: Maybe User -> Maybe String
checkAndGreet = mapMaybe greet

checkAndBye :: Maybe User -> Maybe String
checkAndBye = mapMaybe bye
```

Now we have a generic `mapMaybe` function that can take a function and returns a new function that can deal with the case where the input could be empty (`Nothing`).

With Haskell's strong type system, we can just write functions that process on concrete types, and if we need those functions to be able to process those Maybe value, where we get them from DB calls or HTTP request, we can now just wrap the function with `mapMaybe` to get a new function that is able to process over `Maybe` value.

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
Let's back to our Haskell version. We have a generic `mapMaybe` to map a function over a `Maybe` value. But it seems only work for functions that take just 1 argument.

```haskell
mapMaybe :: (a -> b) -> Maybe a -> Maybe b
```

What if my function takes more than 1 argument? Is it possible to have a function that maps over multiple `Maybe` values? For example, `map2Maybes` and `map3Maybes` with the following type signatures:

```haskell
map2Maybes :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
map3Maybes :: (a -> b -> c -> d) -> Maybe a -> Maybe b -> Maybe c -> Maybe d
...
```

And let's call a function that takes `N` arguments as "N-arity" function. So `mapMaybe` is 1-arity function, and `map2Maybes` is 2-arity function, `map3Maybes` is 3-arity function.

If we have the `map2Maybes` function, then we can use it to wrap a function that takes 2 `Maybe User` values as input.

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

Similarly, you can implement `map3Maybes`, `map4Maybes` ... We can make as many as we want, however, we still need to write them manually each time to wrapping a N-arity function.

Is it possible to have a generic `mapNMaybes` function that works for functions that take any number of arguments?

Let’s find out.

## Map over N Maybe values

In Haskell, every function is curried. We can pass a value to a 2-arity function to get a new 1-arity function.

> :t showParents (User “Bob”)
User -> String

Therefore, `showParents`'s type signature could also be written as:

```haskell
showParents :: User -> (User -> String)
```

if we treat the 1-arity function `(User -> String)` as a value, then `showParents` can be passed to `mapMaybe`, and it will return a `Maybe (User -> String)` type

```
> :t mapMaybe showParents (Just (User “Bob”))
Maybe (User -> String)
```

But wait for a second, `Maybe` type can contain a function?

Yes, why not? `Maybe a` is a generic type, and it can take any concrete type to make a new type. Since a function is also a concrete type, it can be wrapped in a `Maybe` value too. And I don't matter how many arguments it takes.

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

And the `checkAndShowParents` can be implemented by composing `mapMaybe`, `applyMaybe` and `showParents`

```haskell
checkAndShowParents :: Maybe User -> Maybe User -> Maybe String
checkAndShowParents maybeFather maybeMother = applyMaybe (mapMaybe showParents maybeFather) maybeMother
```

## Generalize into N-arity function
Now can we map over 2-arity function, can it apply to N-arity functions?

Yes. Observe the `applyMaybe`'s type signature, we can use it for a 3-arity functions or N-arity too:

```haskell
applyMaybe :: Maybe (a -> b -> c) -> Maybe a -> Maybe (b -> c)
applyMaybe :: Maybe (a -> b -> c -> d) -> Maybe a -> Maybe (b -> c -> d)
applyMaybe :: Maybe (a -> b -> c -> d -> e) -> Maybe a -> Maybe (b -> c -> d -> e)
...
```

And with the following functions, then we can reduce a `Maybe` N-arity function value into a `Maybe` (N-1)-arity function value, which can be further reduced all the way to a `Maybe` value.

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
We've made a generic `mapMaybe` function and a `applyMaybe` function that can be used to wrap any N-arity functions to be able to take values from N `Maybe` values, and shortcircuits to return `Nothing` if any of the N Maybe value is `Nothing`.

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

You might notice a pattern here, if we want to wrap a `N-arity` function with empty input checks, if the function has only 1 argument, we can just use `mayMaybe`. If there is more than 1 argument, we just append (`applyMaybe` argN) in the end.

## Functor and Applicative
OK, with the above examples and practices in mind, now it's the time to introduce the term `Functor` and `Applicative`. Let's see what are they.

What is `Functor`?  `Functor` is a typeclass, which essentially defines a list of functions to implement.

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

In `Prelude`, `fmap` is also defined as the inflx operator `<$>`. Therefore, the following two operations are identical:

```haskell
fmap greet (Just (User "Alice"))
greet <$> Just (User "Alice")
```

## What is `Applicative`?
`Applicative` is also a typeclass. To be an `Applicative`, the type has also to be a `Functor`.

`Applicative` defines two functions `pure` and `<*>` with the following type signature.

```haskell
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```

And `Maybe` is an instance of `Applicative`, refer to the type signature of `Just` and `applyMaybe`, these two functions are `Maybe` type's implementation for being `Applicative`.

```haskell
Just :: a -> Maybe a

applyMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b

class Applicative Maybe where
  pure = Just
  (<*>) = applyMaybe
```

### More Functor example
With the abstraction of `Functor` and `Applicative`, more generic functions can be built and reused.

So far we've only seen one instance of `Functor` and `Applicative`, which is `Maybe`. Actually, there are a lot more of them defined in the [base module](https://www.stackage.org/haddock/lts-11.12/base-4.10.1.0/Prelude.html#control.i:Functor).

The most useful `Applicative`s (which also means they are `Functor`s) are `Maybe`, `Either`, `IO` and `List`, which means you can use the same functions `fmap`, `(<*>)` on those types.

For instance, `IO` is also a `Functor` and an `Applicative`. And here is an example of how to read and parse environment variables with the functions provided by `Functor` and `Applicative`.

To read the env var by name, there is a `getEnv` function under the namespace `System.Environment` that takes `String` as env var name, and return the env var as `IO String`.

```haskell
> import System.Environment (getEnv)
> :t getEnv
getEnv :: String -> IO String
```

`IO String` is a computation that might potentially run into an exception. For instance,

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

Notice that the `read` function is a polymorphic parse function that can be either `String -> Int` or `String -> Bool` depending on where they are used.

So if the environment doesn't have `HOST`, then the `IO` will throw an exception:
```
> readConfig
*** Exception: HOST: getEnv: does not exist (no environment variable)
```

If all the environment variables present, we will get a `Config` value with all the parsed value in it.
```
> readConfig
Config {cfgHost = "localhost", cfgPort = 4567, cfgDebug = True, cfgLogLevel = 1}
```

## Summary
Thanks for reading this blog post through. It's a really long one. Hopefully, I made the functional programming terms `Functor` and `Applicative` a bit earlier to understand.
