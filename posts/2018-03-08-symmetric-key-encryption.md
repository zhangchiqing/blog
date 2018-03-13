---
title: Symmetric-key encryption in Haskell
author: Leo Zhang
tags: haskell, cryptography, aes
---

## Introduction
Symmetric key algorithms use the same cryptographic key for both encryption of plaintext and decryption of ciphertext.

Ciphertexts are bit sequences, which have no meaning.

Without the secret key, ciphertext cannot be converted back to the original plaintext.

The most popular Symmetric-key algorithms include: AES, Blowfish, ChaCha, etc

## Haskell implementations
Haskell community has an awesome library [`cryptonite`](https://hackage.haskell.org/package/cryptonite) that provides implementations for popular cryptographic algorithms.

For Symmetric-key algorithms, they are all grouped under the namespace [`Crypto.Cipher`](https://hackage.haskell.org/package/cryptonite#modules). Let's get started with AES256 and see how it works in Haskell.

## Create a secretKey for AES256 algorithm

AES algorithm has different modes for encrypting/decrypting messages. I'd like to start with ECB mode (Electronic CodeBook mode).

Although ECB mode is not very secure, it is the simplest one, so it's easy to explain and give you a taste of how it works overall.

Alright, first, let's create a secret string.

A secret string is a random string that is used to encrypt plaintext into ciphertext.

```hasekll
{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString (ByteString)

secret :: ByteString
secret = "012-456-89A-CDE-012-456-89A-CDE-"
```

The above code defines a ByteString `secret` with the value `"012-456-89A-CDE-012-456-89A-CDE-"`, which is 32-character long (256 bits) string. The length is important, and I will talk about it later.

`import Data.ByteString (ByteString)` is to tell Haskell complier where the type `ByteString` is defined. `(ByteString)` means import only the type definition from the module.

What does `{-# LANGUAGE OverloadedStrings #-}` do?

It's called language extension, which enables Haskell compiler to understand certain syntax. In this case, `OverloadedStrings` language extension is enabled to create a ByteString value with double quotes, like this `"0123-4567-890A-BCDE"`. This language extension is probably the most common one that you would like to turn on.

Now, let's try it out! Haskell provides a very powerful [REPL](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop), called GHCi that allows experiment with the code.

To start `ghci`, we use `stack ghci`. Wait, what is `stack` then? Well, it's not important for this post.

```
stack ghci
ghci> :load src/Symmetric.hs
[1 of 1] Compiling Symmetric        ( src/Symmetric.hs, interpreted )
Ok, one module loaded.
ghci> secret
"012-456-89A-CDE-012-456-89A-CDE-"
```

We can see `Symmetric.hs` is correctly being compiled. And `secretKey` prints the correct string we defined.

## Make SecretKey
A ByteString secret is not a secret key yet, we need to turn the secret string into a secret key. However, not every string can be converted into a secret key.

If the input string is not convertible, then we will get an error. And if convertible, then we will get an AES256 value, which means we will get either an error or an AES256 value. And the function definition is just like this:

```haskell
makeSecretKey :: ByteString -> Either CryptoError AES256
makeSecretKey secret = eitherCryptoError (cipherInit secret)
```

`makeSecretKey` takes the secret string as input, and returns an `Either CryptoError AES256`. This type is a Union Type. It's a type that contains two variations, as you can guess, it's either a CryptoError or an AES256 value (which is the secret key).

```
stack ghci
ghci> makeSecretKey secret

<interactive>:30:1: error:
    • No instance for (Show AES256) arising from a use of ‘print’
    • In a stmt of an interactive GHCi command: print it
```

Oops, when we try to make a secret key, it returns the above error.

This is a very common error, which basically says, GHCi doesn't know how to convert the returned type into a string in order to print it.

Well, seems like the secret key is not printable. That's OK, we can still know if it's successfully converted or not:

```
ghci> import Data.Either
ghci> isLeft (makeSecretKey secret)
False
ghci> isRight (makeSecretKey secret)
True
ghci> Right s = makeSecretKey secret
ghci> :t s
s :: AES256
ghci> :set -XOverloadedStrings
ghci> isLeft (makeSecretKey "b")
True
ghci> Left err = makeSecretKey "b"
ghci> err
CryptoError_KeySizeInvalid
```

We used `isLeft` and `isRight` to test and since we got a Right value, we can destruct it and get the secretKey `s`. and we use `:t` in the REPL to confirm that the type of `s` is `AES256`

In the real world, we won't parse the secret key like this, but here it's just to show how `makeSecretKey` works.

We then turned on the language extension `OverloadedStrings` in the REPL to be able to make a ByteString secret with `"b"`.

And when we pass that bad key to makeSecretKey, it returns CryptoError, which says `KeySizeInvalid`.

## Functions for encrypting and decrypting message with ECB mode
```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString (ByteString)
import Crypto.Cipher.AES (AES256(..))
import Crypto.Cipher.Types (ecbEncrypt, ecbDecrypt)
import Crypto.Error (CryptoError(..))

secretKey :: ByteString
secretKey = "012-456-89A-CDE-012-456-89A-CDE-"

encryptMsgECB :: AES256 -> ByteString -> ByteString
encryptMsgECB secKey msg = ecbEncrypt secKey msg

decryptMsgECB :: AES256 -> ByteString -> ByteString
decryptMsgECB secKey msg = ecbDecrypt secKey msg
```

To encrypt message with ECB mode, we import the two functions `ecbEncrypt`, and `ecbDecrypt` from `Crypto.Cipher.Types` namespace.

And we define a function `encryptMsgECB` with the type signature `AES256 -> ByteString -> ByteString`. This type signature reads as: it takes a value of type `AES256`, and another value of type `ByteString`, then return a `ByteString` value.

And the next line is the function body which says pass the two variables to the `ecbEncrypt` function. And the `decryptMsgECB` function is pretty much the same.

Let's test them out in GHCi.

Since we imported a few types and functions, we need to add them to the import

```
ghci> encryptMsgECB s "this is a secret"
"\217=\148e\143%Pb\140\226h\132\ENQ\a\206\191"

ghci> decryptMsgECB s $ encryptMsgECB s "this is a secret"
"this is a secret"
```

Nice, we encrypted a plaintext into ciphertext, which is completely different than the plaintext. And we decrypted it back to the original plaintext with the same secret key.

But how does the second statement work? What does the `$` (dollar sign) do?

`$` in Haskell acts as operators in order to avoid parentheses.

So `decryptMsgECB s $ encryptMsgECB s "this is a secret"` will be executed as `decryptMsgECB s (encryptMsgECB s "this is a secret")`. Then, the result of `encryptMsgECB s "this is a secret"` will be passed to `decryptMsgECB s`. And then the result got printed out.

What is `decryptMsgECB s` then? Isn't it got executed already? How can it take another value to execute again?

Right! We are using an important Haskell feature here. Let me explain.

`decryptMsgECB` is a function that takes two parameters - the secret key and the ciphertext. If we pass in only one parameter `s` to it, then it returns a partial function (or curried function) that will take the second parameter and return you the decrypted plaintext.

We can use `:t` in ghci to check the type of the function and the partial function:

```
ghci> :t decryptMsgECB
decryptMsgECB :: AES256 -> ByteString -> ByteString

ghci> :t decryptMsgECB s
decryptMsgECB s :: ByteString -> ByteString

ghci> :t decryptMsgECB s "cipher"
decryptMsgECB s "cipher" :: ByteString
```

This is a language feature called [Currying](https://wiki.haskell.org/Currying). In Haskell, every function is auto-curried, meaning if a function doesn't receive enough parameters, it will return another function that takes the remaining parameters and returns you the result.

## Message size
OK. Back to the topic. Let's try with a different message:

```
ghci> encryptMsgECB s "this is my secret"
"*** Exception: Encryption error: input length must be a multiple of block size (16). Its length is: 17
CallStack (from HasCallStack):
  error, called at ./Crypto/Cipher/AES/Primitive.hs:298:22 in cryptonite-0.24-3fvWA2h8jkgHY8jjamZHms:Crypto.Cipher.AES.Primitive
```

Oops, we got an exception.

The reason is that the message length was wrong. Since AES algorithms will break the message into blocks in order to encrypt it using the secret key, the message length must be a multiple of the block size.

The block size for AES256 is 16. `"this is a secret"` has 16 characters, it worked. But `"this is my secret" has 17 characters, which is not a multiple of 16, so it didn't work.

But then how to encrypt messages that are not a multiple of the block size in length?

We will need the technique called [Padding](https://en.wikipedia.org/wiki/Padding_(cryptography)#Block_cipher_mode_of_operation). We need to pad the message in order to fit the block size. For example, adding spaces to the end of the message until it's 32 in length.

The library doesn't make the decision of how to pad the message, instead, it just implements the algorithm, sets the criteria and leaves the decision of padding up to the user. In fact, ECB mode is not secure, because it's vulnerable to [Padding oracle attach](https://en.wikipedia.org/wiki/Padding_oracle_attack). Choosing the right mode is very important for being secure.

## Verify that Hacker can't decrypt with a different key
OK. So far, we are able to encrypt a message that is a multiple of 16 in length and decrypt it back to the original plaintext. But we haven't checked if it's secure. By "secure", I mean, at least a hacker shouldn't be able to decrypt the ciphertext with a different secret key.

Let's try by making a hacker's key and use that to try decrypting our message
```
ghci> Right hackerSecretKey = makeSecretKey "111-222-333-444-555-666-777-888-"

ghci> decryptMsgECB hackerSecretKey $ encryptMsgECB s "this is a secret"
"\193\&8\252`k\186=\236\a\254n\165\139\158Q\RS"
```

As we see, when a hacker is using a different key than the key used to encrypt the message, the returned message is completely different than the original message.

## Summary
In this blog post, I introduced one of the Symmetric key encryption algorithm - the AES256.

We chose the Haskell library - cryptonite, and implemented a few functions to create a secret key, encrypt and decrypt messages.

The block cipher mode we used for AES is ECB mode, which is the simplest mode.

Along the way, we also introduced some basic Haskell syntax and language features, and how to use GHCi as a REPL to quickly test and experiment the functions that we implemented.

All the source code can be found [here](https://github.com/zhangchiqing/blog/tree/master/code/cryptography/src/Symmetric.hs).

In the next blog post, I will be talking about a different block cipher mode for AES algorithm, called CTR mode. This mode is a more practical and secure mode that can be used in real-world cases.
