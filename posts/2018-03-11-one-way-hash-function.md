---
title: One way hash function in Haskell - SHA
author: Leo Zhang
tags: haskell, cryptography, hash
---

## What is one-way hash function
A one-way hash function takes a message in any length and returns a hash value in fixed length.

It has the following properties:

1. One way hash function is a pure function, meaning computing hash for the same message will always produce the same hash value.
2. If a message is changed somehow, even 1 bit only, then the returned hash value will be very different.
3. No matter how big the message is, e.g 10 bits, 10 MB, 100 GB, the length of its hash value is always the same.
4. One way means a hash value can't be converted back to its original message.

With the above properties, one can check if a certain message or file was intercepted or not very quickly.

For example, Alice is editing a big file, let's say it's 1 GB. Once she's done with the editing, she uploaded the file to the server and went home. The next day, she wants to continue the work from yesterday, so she downloaded the 1 GB file from the server. But before she starts opening that file, she has the question: "Did my file ever get hacked by someone?".

If she still has the original file on the disk, she could compare it with the file downloaded from the server bit by bit. But it's gonna be slow because the file is 1GB big.

With one-way hash function, Alice can compute the hash value of the file before uploading it to the server. And next day, she can compute the hash value of the file downloaded from the server and then check if the two hash values are identical to know if the file was intercepted.

If they are the same, it means the file was not intercepted, because if any bit of the file was changed, the hash will be a different one. And if the two hash values are different, then the file must be intercepted by someone! That's because the hash value for a message never changes.

A hash value is also called message digest or fingerprint.

## Hash collision and Secure Hash Algorithm
A hash collision means that two different messages have the same hash value.

Wait, didn't you just say if a message is changed, its hash value will be changed as well? Well, yes, but there is still a very small chance a message is changed, the hash value does not change. With a good hash function, the possibility of such case will be reduced to very small such that we can ignore.

However, since this possibility still exists, to describe it, we call it hash collision.

Secure Hash Algorithm (SHA) provides a family of hash functions including: `SHA-1`, `SHA-2` and `SHA-3`.

[Since 2005 `SHA-1` has not been considered secure](https://en.wikipedia.org/wiki/SHA-1). In 2017, [Google announced the first SHA-1 hash collision](https://security.googleblog.com/2017/02/announcing-first-sha1-collision.html).

`SHA-2` and `SHA-3` are the recommended hash function since 2010. Bitcoin uses `SHA256` from `SHA-2` family for verifying transactions and calculating proof of work. Ethereum uses `Keccak-256` and `Keccak-512`, which is a variant of `SHA3-256` and `SHA3-512`, for verifying transactions.

## Haskell implementation for SHA256
The Haskell crypto library `cryptonite` provides [a number of well-known hash algorithms](https://hackage.haskell.org/package/cryptonite-0.25/docs/Crypto-Hash-Algorithms.html) under the namespace `Crypto.Hash.Algorithms`.

I'm going to use SHA256 from SHA-2 family as an example to go through how hash function works.

Since a hash value is also called message digest. `cryptonite` provides a polymorphic function to turn a `ByteString` into a `Digest` under a certain hash algorithm.

For example, we can make a hash function computes message digest using SHA256 algorithm like this:

```haskell
import Crypto.Hash (Digest, hash, SHA256)

messageHashSHA256 :: ByteString -> Digest SHA256
messageHashSHA256 msg = hash msg
```

If you remember the point-free style I mentioned in the last blog post, the above function can be refactored into this:

```haskell
messageHashSHA256 :: ByteString -> Digest SHA256
messageHashSHA256 = hash
```

Let's try it out in GHCi:

```
stack ghci src/OneWayHashFunction.hs

ghci> :set -XOverloadedStrings

ghci> messageHashSHA256 "hello"
2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824

ghci> messageHashSHA256 "hello world"
b94d27b9934d3e08a52e52d7da7dabfac484efe37a5380ee9088f7ace2efcde9

ghci> messageHashSHA256 "hello world"
b94d27b9934d3e08a52e52d7da7dabfac484efe37a5380ee9088f7ace2efcde9

ghci> messageHashSHA256 "hello world. this is a very very very very very very very very very very very long message"
b30e31dbb4c9c8478888890be8a053226d7c3f297fd3ff01ea9566c683f34690

ghci> messageHashSHA256 "hello worlD"
519fd0f684972f4eaa0946cc880198619d11c1f2f026d8f84ab080ad54f72c1a
```

From the above experiments, we can see:
1. The hash values for "hello" and "hello world" is different, but both of them have the same length (64 characters).
2. Running `messageHashSHA256 "hello world"` twice return the same hash value
3. No mater how long the message is, the hash value is always 64 characters long.
4. Even 1 character change from `hello world` to `hello worlD` will produce two completely different hash value.

## Polymopyic `hash` function
We have an implementation for computing SHA256 digest for a message. How to implement a function to compute `Keccak-256` hash?

It's actually quite similar to the SHA256 hash function implementation:

```haskell
import Crypto.Hash (Digest, hash, SHA256)

messageHashSHA256 :: ByteString -> Digest SHA256
messageHashSHA256 = hash

messageHashKeccak256 :: ByteString -> Digest Keccak_256
messageHashKeccak256 = hash
```

The implementations are the same, just the `hash` function. Only the type signature is different.

Let's try it out in GHCi:

```
stack ghci src/OneWayHashFunction.hs

ghci> :set -XOverloadedStrings

ghci> messageHashSHA256 "hello world"
b94d27b9934d3e08a52e52d7da7dabfac484efe37a5380ee9088f7ace2efcde9

ghci> messageHashKeccak256 "hello world"
47173285a8d7341e5e972fc677286384f802f8ef42a5ec5f03bbfa254cb01fad
```

Both `messageHashSHA256` and `messageHashKeccak256` produced hash values, but they are different.

You might have the question. Why the function definition for `messageHashSHA256` and `messageHashKeccak256` are the same, which is just the `hash` function, but the implementations are different?

Let's take a look at [the type signature of the `hash` function](https://hackage.haskell.org/package/cryptonite-0.25/docs/Crypto-Hash.html#v:hash).

```haskell
-- Hash a strict bytestring into a digest.
hash :: (ByteArrayAccess ba, HashAlgorithm a) => ba -> Digest a
```

`hash` takes one parameter `ba` and returns a `Digest a` value. What's `ba`? It's a generic type, and the left-hand side of `=>` defines what is `ba`.

On the left-hand side of `=>`, there is `(ByteArrayAccess ba, HashAlgorithm a)`, which says `ba` is an instance of typeclass `ByteArrayAccess` and `a` is an instance of typeclass `HashAlgorithm`. As we introduced in the last blog post, tyepclasses are basically interfaces that define a number of functions that a type has to implement.

So the whole type signature `hash :: (ByteArrayAccess ba, HashAlgorithm a) => ba -> Digest a` reads as:

If `ba` is any type that implements all the functions that are defined by typeclass `ByteArrayAccess`, and if `a` is any type that implements all the functions that are defined by typeclass `HashAlgorithm`, then `hash` is a function that takes a value of the type `ba` and returns a value of type `Digest a`.

So when we define the SHA256 hash function like this:

```haskell
messageHashSHA256 :: ByteString -> Digest SHA256
messageHashSHA256 = hash
```

Since `ByteString` is an instance of `ByteArrayAccess` typeclass and `SHA256` is an instance of typeclass `HashAlgorithm`, then the generic `hash` function is able to use their implementations of `ByteArrayAccess` and `HashAlgorithm` typeclasses (interfaces) to compute the hash value (digest).

This is how polymorphism works in Haskell.

## Static type check

Haskell allows polymorphism through typeclass. We've seen the example that the generic `hash` function can take different types value and compute its hash value.

One might argue that the similar polymorphism can be also implemented in languages like Javascript or Python. What sets Haskell apart is that its advanced type system is able to run the static type check and find the type error at compile time.

Let's take the above `messageHashSHA256` function as an example. So if let it take `Int` as parameter instead of `ByteString`,

```haskell
messageHashSHA256 :: Int -> Digest SHA256
messageHashSHA256 = hash
```

Let's see what would happen:

```
stack ghci src/OneWayHashFunction.hs

/Users/leo/zhangchiqing/blog/posts/code/cryptography/src/OneWayHashFunction.hs:9:21: error:
    • No instance for (memory-0.14.11:Data.ByteArray.Types.ByteArrayAccess
                         Int)
        arising from a use of ‘hash’
    • In the expression: hash
      In an equation for ‘messageHashSHA256’: messageHashSHA256 = hash
  |
9 | messageHashSHA256 = hash
  |                     ^^^^
```

The error says `Int` is not an instance of typeclass `ByteArrayAccess`, which is required by the `hash` function. Because of such error, the problem won't compile.

## Summary
In this blog post, we introduced what is one-way hash function, and how it works. We used the Haskell library cryptonite to implement a function that computes a SHA256 digest of a given message, and also introduced how polymorphism works in Haskell by comparing the implementation of two different hash functions.

In the next blog post, I will be talking about Asymmetric key encryption, and how to use it in Haskell.
