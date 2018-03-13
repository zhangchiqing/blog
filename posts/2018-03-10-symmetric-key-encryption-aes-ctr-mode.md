---
title: Symmetric-key encryption in Haskell, AES and CTR mode
author: Leo Zhang
tags: haskell, cryptography, aes
---

## Introduction
In the last blog post, we introduced symmetric key encryption and how to use one of the implementations - AES256 in Haskell. We introduced the block cipher mode - ECB mode. In this blog post, I'm going to talk about a more practical and secure mode - CTR mode (Counter mode).

And also I will be talking about how to use `IO` in Haskell, and more useful commands for GHCi.

Let's get started.

## CTR Mode
For ECB mode, we can observe that the ciphertext is only determined by the secret key and plaintext.

<picture for ECB mode>

So ECB would be vulnerable for [replay attack](https://en.wikipedia.org/wiki/Replay_attack).

For example, Alice and Bob both have the same secret key, if Alice wants to send a message Bob, saying "send $100 from Alice's account to Bob's account". Alice can encrypt the message with the shared secret key, and sent the ciphertext to Bob. But if a hacker Mallory intercepted the ciphertext, even though she doesn't know what's the plaintext, she can still mass up by sending the same ciphertext to Bob twice (replay). Then, since Bob receives two exactly same messages, and they all can be decrypted correctly with his secret key, then he would think Alice sent $200 to him in total.

CTR mode changes that. CTR mode turns a block cipher into a stream cipher. In addition to a secret key, CTR mode also asks for an initialization vector (IV) which is basically a random nonce to encrypt plaintext.

<picture for CTR mode>

Let's see how the initialization vector helps to prevent the replay attack. Let's use the example of Alice, Bob, and Mallory again:

Alice and Bob both have the same secret key, if Alice wants to send a message Bob, saying "send $100 from Alice's account to Bob's account". Alice first creates a new random IV, then encrypt the message with the secret key and the IV. Then Alice sends the ciphertext along with the IV to Bob. With the IV and the secret key, Bob can decrypt the ciphertext back to the plaintext.

Now if Mallory somehow receives both the ciphertext and the IV, since she doesn't have the secret key, she can't decrypt the message. And if she sends the ciphertext again to Bob to do replay attack. Bob will reject the second message because the IV is the same, Alice and Bob have the agreement that everything they send each other a message, the IV has to be different.

If Mallory sends Bob the ciphertext with a different IV, since the IV is not the same IV that ciphertext was encrypted with, then the cipher can't be decrypted, therefore Bob can just treat the ciphertext as a malformated message.

## Creating initialization vector (IV)
Let's see how to use the AES CTR mode in Haskell.

First, we need to create an IV. `cryptonite` provides three functions for making IV: `makeIV`, `ivAdd` and `nullIV`.

```haskell
makeIV :: (ByteArrayAccess b, BlockCipher c) => b -> Maybe (IV c)
nullIV :: BlockCipher c => IV c
ivAdd :: BlockCipher c => IV c -> Int -> IV c
```

Let's start with `nullIV` and `ivAdd`. `nullIV` creates an IV that is representing `0`. And `ivAdd` adds an int to a given IV and returns a new IV. So we can combine this two functions and create a new function that creates an IV from an int.

Let's call this function `makeIVFromInt`, add it to a new file `src/SymmetricCTR.hs` and import the necessary modules.

```haskell
{-# LANGUAGE OverloadedStrings #-}

module SymmetricCTR where

import Crypto.Cipher.AES (AES256(..))
import Crypto.Cipher.Types (IV, ivAdd, nullIV, ctrCombine)

makeIVFromInt :: Int -> IV AES256
makeIVFromInt = ivAdd nullIV
```

OK. Let's try it out in GHCi

```
stack ghci src/SymmetricCTR.hs

ghci> makeIVFromInt 100

<interactive>:2:1: error:
    • No instance for (Show (IV AES256)) arising from a use of ‘print’
    • In a stmt of an interactive GHCi command: print it
```

Oops, looks like `IV` doesn't have a `Show` instance, so GHCi doesn't know how to turn it into String in order to print it.

## Print IV, typeclass
We don't have to print the `IV`, because as long as the encryption can read it properly, that's OK. But here, I'm just curious to see if it's possible.

Yes, it's possible, and actually quite easy. But to understand how it works, we need to understand some Haskell language feature.

The type `IV` is an instance of typeclass `ByteArrayAccess`:

```haskell
instance BlockCipher c => ByteArrayAccess (IV c)
```

What is typeclass? typeclass is essentially an interface that defines some functions that a type has to implement.

For example, for type `IV c`, if `c` is a `BlockCipher`, then `IV c` is an instance of `ByteArrayAccess`. Since ByteArrayAccess defines the following three methods, `IV c` has implemented these functions.

```haskell
length :: IV c -> Int
withByteArray :: IV c -> (Ptr p -> IO a) -> IO a
copyByteArrayToPtr :: IV c -> Ptr p -> IO ()
```

Since `IV c` is an instance of `ByteArrayAccess`, then any operation that takes `ByteArrayAccess` can be used on `IV c`.

`Data.ByteArray` from a module `memory` defines the `DataArrayAccess` typeclass and provides a list of functions to interact with it.

For example, the `unpack` function from `Data.ByteArray` takes any type that implements `ByteArrayAccess` and returns a list of `[Word8]`.

```haskell
unpack :: ByteArrayAccess a => a -> [Word8]
```

What is `Word8`? We will take about later. For now, just to know `[Word8]` is a type that can be printed. Let's try it in GHCi and print an IV.

```haskell
stack ghci src/SymmetricCTR.hs

ghci> import Data.ByteArray (unpack)
ghci> import GHC.Word (Word8)

ghci> unpack $ makeIVFromInt 100
[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,100]

ghci> unpack $ makeIVFromInt 256
[0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0]

ghci> unpack $ makeIVFromInt 30000000000000
[0,0,0,0,0,0,0,0,0,0,27,72,235,87,224,0]
```

Cool. We created an IV from an int `100`, and passed it to `Data.ByteArray.unpack` to get a list of `Word8`, which is now printed out.

Let's now take a look at the `Word8`. `Word8` is a 8 bits unsigned integer, ranges from 0-255. `[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,100]` contains 16 `Word8`s.

OK, but why `[Word8]` can be printed?

Because first `Word8` is an instance of typeclass `Show`. Yes, `Show` is also a typeclass, which defines a `show` function to take a type value and return a readable `String`. So a `Word8` value can be printed in GHCi.

Since `Word8` is an instance of `Show`, a list of `Show` is an instance of `Show` as well, so `[Word8]` is also an instance of `Show`, which can be printed out.

It's worth to mention, 16 `Word8`s can present a very big integer, ranges from 0 - 2^128, so that it's super hard for a hacker to guess which integer was used to create the IV.

## Encrypt message with IV and secret key
To encrypt message with IV and secret key in CTR mode, we use `ctrCombine` function from `Crypto.Cipher.Types`. The implementation is as simple as defining the type signature:

```haskell
import Crypto.Cipher.Types (cipherInit, IV, ivAdd, nullIV, ctrCombine)

encryptMsgCTR :: AES256 -> IV AES256 -> ByteString -> ByteString
encryptMsgCTR key iv plaintext = ctrCombine key iv plaintext

decryptMsgCTR :: AES256 -> IV AES256 -> ByteString -> ByteString
decryptMsgCTR key iv plaintext = ctrCombine key iv plaintext
```

The encryption and decryption are the same! Yes, Symmetric key encryption algorithm uses the same key to encrypt and decrypt. In CTR mode, we decrypt the message by encrypting the message with the same key and iv again.

For the secret key creation, we can refer to the previous blog post, and reuse the function `makeSecretKey` and `secret`.

```haskell
makeSecretKey :: ByteString -> Either CryptoError AES256
makeSecretKey secret = eitherCryptoError (cipherInit secret)

secret :: ByteString
secret = "012-456-89A-CDE-012-456-89A-CDE-"
```

Let's test in GHCi.

```
stack ghci src/SymmetricCTR.hs

ghci> let Right sKey = makeSecretKey secret

ghci> :set -XOverloadedStrings

ghci> encryptMsgCTR sKey (makeIVFromInt 100) "this is a secret"
"}Y7 \157D5\134A\129\188\163J\244(^"

ghci> encryptMsgCTR sKey (makeIVFromInt 300) "hello world"
"\128)aO\201\235\185Nb\162G"

ghci> decryptMsgCTR sKey (makeIVFromInt 100) $ encryptMsgCTR sKey (makeIVFromInt 100) "this is a secret"
"this is a secret"
```

We can encrypt and decrypt message with the same secret key and IV. What's nice with CTR mode is that, since CTR is stream cipher (not block cipher), the message size doesn't have to be a multiple of a fixed size like block cipher. So it can also encrypt message like `hello world` without padding.

## Refactor with point-free style

Observe the function `encryptMsgCTR`, the left-hand side of `=` and the right-hand side are quite similar. Both `encryptMsgCTR` and `ctrCombine` take three parameters: `key`, `iv`, and `plaintext`. In this case, we can actually omit the last parameter from both sides.

```haskell
encryptMsgCTR :: AES256 -> IV AES256 -> ByteString -> ByteString
encryptMsgCTR key iv = ctrCombine key iv
```

With the type signature, we know that `encryptMsgCTR` takes `key` and `iv`, passes them to `ctrCombine`, which will return `ByteString -> ByteString`. This is a curried function that takes the `ByteString` plaintext and returns ciphertext in `ByteString`. You can try it in GHCi, and it also works.

Since the `iv` appears at the end of the both sides of `=`. We can do it to `iv` and omit it for both sides again:

```haskell
encryptMsgCTR :: AES256 -> IV AES256 -> ByteString -> ByteString
encryptMsgCTR key = ctrCombine key
```

And again for `key`:

```haskell
encryptMsgCTR :: AES256 -> IV AES256 -> ByteString -> ByteString
encryptMsgCTR = ctrCombine
```

We end up having a very concise function implementation. And it still works exactly as before. This style called [point-free style](https://wiki.haskell.org/Pointfree)

## Use IO Monad to print result
So far, all the functions we defined are all pure functions. Pure functions are functions that have no side effect. If we pass the same parameters to pure functions, they always return the same result.

In order to do side effects like printing log, writing data to disk, or sending requests, they need to be wrapped in an IO Monad.

Lets use IO Monad to put all the experiments we did in GHCi into a function called `testCTREncryption`:

```haskell
testCTREncryption :: IO ()
testCTREncryption = do
  print "Can create and print IV:"
  print $ printIV $ makeIVFromInt 100

  let Right sKey = makeSecretKey secret
  print "Can encrypt plain text into cipher text with IV and secret in CTR mode:"
  print $ encryptMsgCTR sKey (makeIVFromInt 100) "this is a secret"

  print "Can decrypt back to original plain text"
  print $ decryptMsgCTR sKey (makeIVFromInt 100) $ encryptMsgCTR sKey (makeIVFromInt 100) "this is a secret"

  print "Can decrypt back to original plain text in any length"
  print $ decryptMsgCTR sKey (makeIVFromInt 100) $ encryptMsgCTR sKey (makeIVFromInt 100) "hello world"

  print "done"
```

The `do` notation declares that each of the following lines will return an IO Monad which does some side effect. The `print` function takes a value that can be converted into String and returns an IO Monad which prints the String as the side effect. And return value of the last statement `print "done"` will be the return value of the function `testCTREncryption` which is `IO ()`

Note that there is one statement starts with `let`. `let` defines a scoped variable that only this function has access to, pretty much like the `let` keyword in Javascript.

```haskell
  let Right sKey = makeSecretKey secret
```

Running the function in GHCi, we can see all the results are printed out correctly

```
stack ghci src/SymmetricCTR.hs

ghci> testCTREncryption

"Can create and print IV:"
[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,100]
"Can encrypt plain text into cipher text with IV and secret in CTR mode:"
"}Y7 \157D5\134A\129\188\163J\244(^"
"Can decrypt back to original plain text"
"this is a secret"
"hello world"
"done"
```

## Verify property
Now we can encrypt and decrypt messages in CTR mode. We also need to verify that if either the IV or the secret key is different than the one used for encryption, the ciphertext can't be decrypted.

Let's add more checks in the `testCTREncryption` function:

```haskell
testCTREncryption :: IO ()
testCTREncryption = do
  print "Can create and print IV:"
  print $ printIV $ makeIVFromInt 100

  let Right sKey = makeSecretKey secret
  print "Can encrypt plain text into cipher text with IV and secret in CTR mode:"
  print $ encryptMsgCTR sKey (makeIVFromInt 100) "this is a secret"

  print "Can decrypt back to the original plain text"
  print $ decryptMsgCTR sKey (makeIVFromInt 100) $ encryptMsgCTR sKey (makeIVFromInt 100) "this is a secret"

  print "Can decrypt back to the original plain text in any length"
  print $ decryptMsgCTR sKey (makeIVFromInt 100) $ encryptMsgCTR sKey (makeIVFromInt 100) "hello world"

  print "Can't decrypt back to the original plain text with a wrong IV"
  print $ decryptMsgCTR sKey (makeIVFromInt 101) $ encryptMsgCTR sKey (makeIVFromInt 100) "this is a secret"

  let Right hackerSecretKey = makeSecretKey "111-222-333-444-555-666-777-888-"
  print "Can't decrypt back to the original plain text with a wrong key"
  print $ decryptMsgCTR hackerSecretKey (makeIVFromInt 100) $ encryptMsgCTR sKey (makeIVFromInt 100) "this is a secret"

  print "done"
```

Running the function in GHCi:

```
stack ghci src/SymmetricCTR.hs

ghci> testCTREncryption
"Can create and print IV:"
[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,100]
"Can encrypt plain text into cipher text with IV and secret in CTR mode:"
"}Y7 \157D5\134A\129\188\163J\244(^"
"Can decrypt back to the original plain text"
"this is a secret"
"Can decrypt back to the original plaintext in any length"
"hello world"
"Can't decrypt back to the original plain text with a wrong IV"
"\ENQgb\175\152\207\195B\242\NUL\136\195\130\173\"("
"Can't decrypt back to the original plain text with a wrong key"
"\156TO\251\216\142\193\153\235G\219\252\247\SUBk\139"
"done"
```

## Summary
In this blog post, I introduced the CTR mode of AES algorithm. And why it's more secure than ECB mode. CTR mode of AES256 is one of the most practical symmetric key encryption algorithms that has been used in real-world applications.

We also went through how to implement CTR mode in Haskell. Along the way, we touched a few Haskell concepts like: typeclass, point-free style, IO Monad.

In the next blog post, I'm going to talk about one-way hash function, and how it can help us to check if a message was interpolated or not.
