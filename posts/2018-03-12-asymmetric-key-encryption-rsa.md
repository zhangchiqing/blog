---
title: Asymmetric key encryption
author: Leo Zhang
tags: haskell, cryptography, rsa
---

## Asymmetric key encryption

Symmetric key encryption uses the same key for encryption and decryption. Sharing the secret key safely is a challenge.

Asymmetric key encryption mitigates that challenge by allowing to use different keys for encryption and decryption.

The key to encrypt the plaintext is called public key, which is safe to be shared. And the key to decrypt the ciphertext is called private key, which needs to keep private so that only the key holder can decrypt the ciphertext. Therefore, asymmetric encryption is also known as public key encryption.

In this blog post, we are gonna talk about how it works and how to use it in a Haskell to encrypt and decrypt messages. And we will also explain some Haskell syntax, like `<-` and pattern matching.

## RSA
RSA is one of the most popular asymmetric key encryption algorithms. It’s based on the assumption that it’s very difficult to factor a very large number.

There are other algorithms that based on different assumptions that are also known as very difficult to do certain computation.

Here, we are going to take RSA as an example.

For RSA, here are the functions for encryption and decryption:

If `m` is a message and `c` is its ciphertext, then

The ciphertext is computed by a function like this
c = m ^ e mod n

And message can be decrypted by computing the following function
m = c ^ d mod n

Observe the above functions, the variables required to encrypt the plaintext are e and n, which are two numbers. Meaning, anyone who knows these two numbers is able to encrypt a message. So the public key is actually just these two numbers: e and n.

And anyone who holds the two numbers d and n is able to decrypt the ciphertext. So the private key is just these two numbers: d and n.

The following diagram shows computation process:



```
                 public key (e, n)
                        |
                        v
   plaintext -----> RSA encrypt -----> ciphertext
                (c = m ^ e mod n)

                private key (d, n)
                        |
                        v
   plaintext <----- RSA decrypt <----- ciphertext
                (m = c ^ d mod n)

```

## Make key pair
Alright, enough theory, let’s see how to use RSA in Haskell.

First, Let’s make a key pair.

A key pair consists of a public key and private key.

```haskell
module Asymmetric where

import Crypto.PubKey.RSA.Types (PublicKey, PrivateKey, Error(..))
import Crypto.PubKey.RSA (generate)

makeKeyPair :: IO (PublicKey, PrivateKey)
makeKeyPair = generate size e
  where
    size = 256
    e = 3
```

The type signature `IO (PublicKey, PrivateKey)` defines that this function has some side effect as `IO` which will return a pair of `PublicKey` and `PrivateKey`. `(xx, xx)` is a pair of types

In the function body, the generate function takes a variable size. Size 256 here means we are generating a RSA256 key pair.
Another variable e is one of the numbers in public key. The bigger e is, the longer it takes to run the computation and also more secure.

We can make a key pair, and print it out in GHCi and see what is a key pair:

```
stack ghci src/Asymmetric.hs

ghci> makeKeyPair
(PublicKey {public_size = 256, public_n = 22001470929691721059024293937824858733953167283677511016203643254631053249708272410508212331213021293695571111691411973115863571131151933581829000361427710656488218366621521637458492079806538886822571389468489611162587529760999079821301823890091469897563564024191125674291270181749717766593101394590765721008358981658458632743404884296794470236226929125775245621179521409544803046605369864307271623418242278379496643995251925497947828726109525815068891130991796425885657847749264720083366075769994895755233146883939317458421105102906525406931825106633485347800185456552160868660694762760244935962912344309099716509859, public_e = 3},PrivateKey {private_pub = PublicKey {public_size = 256, public_n = 22001470929691721059024293937824858733953167283677511016203643254631053249708272410508212331213021293695571111691411973115863571131151933581829000361427710656488218366621521637458492079806538886822571389468489611162587529760999079821301823890091469897563564024191125674291270181749717766593101394590765721008358981658458632743404884296794470236226929125775245621179521409544803046605369864307271623418242278379496643995251925497947828726109525815068891130991796425885657847749264720083366075769994895755233146883939317458421105102906525406931825106633485347800185456552160868660694762760244935962912344309099716509859, public_e = 3}, private_d = 14667647286461147372682862625216572489302111522451674010802428836420702166472181607005474887475347529130380741127607982077242380754101289054552666907618473770992145577747681091638994719871025924548380926312326407441725019840666053214201215926727646598375709349460750449527513454499811844395400929727177147338707751611269753196630544193328648518305533266149207721686108290389565090833995638523675180831207526796203979489953345247559471691685902067595372425001512041780614596121453626172998871486577511263446933501402486525078865386867980393738602816532308364733805758951037462219905551880742976724544930276989965800427, private_p = 138507788800545872595359435296240691943417753513304741882447319712131538665878911546193699988668148318000811802388342094177338257212984453237040491568136840115940332346539490454172226814539901589169491033689770136362378964023352958284526633082124981251456899657051606247503827235025900235537355815500670631281, private_q = 158846452753457075863708571505256766825211473038129296767911654248323871688497494975565152182762839867189862957933565532431282931367688260438792001921391523098795621220544790369695541725588727270893255598145817534440428058581201858039394248752897819448019918468553069083332607704104570640557593078114097177939, private_dP = 92338525867030581730239623530827127962278502342203161254964879808087692443919274364129133325778765545333874534925561396118225504808656302158026994378757893410626888231026326969448151209693267726112994022459846757574919309348901972189684422054749987500971266438034404165002551490017266823691570543667113754187, private_dQ = 105897635168971383909139047670171177883474315358752864511941102832215914458998329983710101455175226578126575305289043688287521954245125506959194667947594348732530414147029860246463694483725818180595503732097211689626952039054134572026262832501931879632013278979035379388888405136069713760371728718742731451959, private_qinv = 47562800653117410613346288972154672405270318633866467039671552816384904581535480631961381373321314148431132525838161079435325060229935954631791639036113375718026187126820258972706757813380747668914927433515495886349202560142608580362532868864448945928957063330675939419701124629093309997897580234096193033153})
```

The public key basically includes the size and the two numbers e and n. The private key includes the public key and the private number `d`, and other internal numbers that are used to calculate `d` and `n`.

All of these numbers are very large numbers.

## Making a seed and great type alias with type
The encryption needs a seed, which is a 32 bytes random `ByteString`. Since public key is clear, a seed is necessary in order to add randomness to the ciphertext for the same message.

```haskell
makeSeed :: IO ByteString
makeSeed = CRT.getRandomBytes 32 -- 32 bytes = 256 bits
```

In Haskell, in order to distinguish the seed from other types, we can name the `ByteString` as `Seed` using the keyword `type`.

```haskell
type Seed = ByteString

makeSeed :: IO Seed
makeSeed = CRT.getRandomBytes 32 -- 32 bytes = 256 bits
```

`type` essentially gives the type `ByteString` an alias. Within the namespace, `Seed` and `ByteString` are now equivalent, both can be used in type signatures.

## `do` notation and `<-` symbol
We can define a function that encrypts a message with RSA. As the first step, we will make a seed and print it out.

```haskell
testEncryptRSA :: IO ()
testEncryptRSA = do
  print "making a seed:"
  seed <- makeSeed
  print seed
```

The above code will make a pair of public key and private key

Since `makeSeed` has side effect as IO and returns a `Seed`, the `<-` symbol is to take the result from the side effect and assign the result, which is a `Seed`, to the variable on the left-hand side of `<-`. With this syntax, Haskell’s type system is able to inferrer that the seed is a value of type `Seed`.

```
stack ghci src/Asymmetric.hs

ghci> testEncryptRSA
"making a seed:"
"\196kW\DC3s\255\188s\132Fu\SYN\190P\140\177\157\197\167\180\&6\202\210w\194\251/\149U\235\171\249"
```

## Make a key and pattern matching syntax
Let's adding key creating to the `testEncryptRSA` function.

```haskell
testEncryptRSA :: IO ()
testEncryptRSA = do
  print "making a seed:"
  seed <- makeSeed
  print seed

  (pKey, sKey) <- makeKeyPair
  print (pKey, sKey)
```

The left-hand side of `<-` is now `(pKey, sKey)`, which doesn’t look like a variable but more like an expression.

Yes, that’s the pattern matching. Since the type signature of `makeKeyPair` is `IO (PublicKey, PrivateKey)`, the left-hand side of `<-` is `(pKey, sKey)` which matches the type signature `(PublicKey, PrivateKey)`.  The `pKey` will get matched with the result of `PublicKey` returned by `makeKeyPair`, and the `sKey` will be the `PrivateKey`.

Printing both of them out as a pair with `(  ,  )` would show us the pair of keys.

## Encryption and decryption in point-free style
### How to use RSA to encrypt the message?

Let's take a look at the function that cryptonite provides. cryptonite provides two functions for encrypting message: [`encrypt`](https://hackage.haskell.org/package/cryptonite-0.25/docs/Crypto-PubKey-RSA-OAEP.html#v:encrypt) and [`encryptWithSeed`](https://hackage.haskell.org/package/cryptonite-0.25/docs/Crypto-PubKey-RSA-OAEP.html#v:encryptWithSeed). Since we've created seed, let's use the `encryptWithSeed` function.

```haskell
encryptWithSeed
    :: HashAlgorithm hash
    => ByteString                               -- Seed
    -> OAEPParams hash ByteString ByteString    -- OAEP params to use for encryption
    -> PublicKey                                -- Public key.
    -> ByteString                               -- Message to encrypt
    -> Either Error ByteString
```

The type signature of `encryptWithSeed` asks for a few arguments, other than seed and public key, which we already have, it also asks for an `OAEP` parameter. `OAEP` stands for Optimal Asymmetric Encryption Padding. Basically, it uses padding algorithm to encrypt a message in any length.

### Now the question is: How to create a variable that has type `OAEPParams hash ByteString ByteString`?

Clicking on the `OAEPParams` link on the hackage document, we found there is a constructor function [`defaultOAEPParams`](https://hackage.haskell.org/package/cryptonite-0.25/docs/Crypto-PubKey-RSA-OAEP.html#t:OAEPParams) that returns `OAEPParams` type.

```haskell
defaultOAEPParams :: (ByteArrayAccess seed, ByteArray output, HashAlgorithm hash) => hash -> OAEPParams hash seed output
```

The `defaultOAEPParams` function takes a generic type `hash`, on the left-hand side of `=>`, it restricts `hash` to be any type instance of typeclass `HashAlgorithm`, we learned how to use hash function like `SHA256`, `SHA512` in [the previous blog post](https://github.com/zhangchiqing/blog/blob/master/posts/2018-03-11-one-way-hash-function.md).

What is the type `OAEPParams hash seed output`? `seed` and `output` are also typeclasses, meaning they are generic type, it's up to us to choose what type we need, as long as they are an instance of its tyepclass.

We can use `:t` to show the type of certain function or expression in GHCi:

```
stack ghci src/Asymmetric.hs
ghci> import Crypto.PubKey.RSA.OAEP
ghci> :t defaultOAEPParams
defaultOAEPParams
  :: (memory-0.14.11:Data.ByteArray.Types.ByteArrayAccess seed,
      HashAlgorithm hash,
      memory-0.14.11:Data.ByteArray.Types.ByteArray output) =>
     hash -> OAEPParams hash seed output

ghci> import Crypto.Hash.Algorithms
ghci> :t defaultOAEPParams SHA256
defaultOAEPParams SHA256
  :: (memory-0.14.11:Data.ByteArray.Types.ByteArrayAccess seed,
      memory-0.14.11:Data.ByteArray.Types.ByteArray output) =>
     Crypto.PubKey.RSA.OAEP.OAEPParams SHA256 seed output

ghci> :t defaultOAEPParams SHA256 :: OAEPParams SHA256 ByteString ByteString
defaultOAEPParams SHA256 :: OAEPParams SHA256 ByteString ByteString
  :: OAEPParams SHA256 ByteString ByteString
```

### `::` type casting
The `::` in `defaultOAEPParams SHA256 :: OAEPParams SHA256 ByteString ByteString` is to cast the type.

Since `ByteString` are type instance of typeclass `ByteArrayAccess` and `ByteArray`, we can use `::` to cast the type of `defaultOAEPParams SHA256` to be a concrete type instance which is `OAEPParams SHA256 ByteString ByteString`

If we try to cast into an incompatible type, GHCi will show the following error:

```
ghci> :t defaultOAEPParams SHA256 :: OAEPParams SHA256 ByteString Int
error:
    • No instance for (memory-0.14.11:Data.ByteArray.Types.ByteArray
                         Int)
        arising from a use of ‘defaultOAEPParams’
    • In the expression:
          defaultOAEPParams SHA256 :: OAEPParams SHA256 ByteString Int
```

### Combine with the encryptWithSeed function
Now we have all the inputs required for `encryptWithSeed`, let's make an `encryptMsgRSA` function that takes a seed, a public key, and a message and then use RSA algorithm to encrypt the message into a ciphertext.

```haskell
encryptMsgRSA :: Seed -> PublicKey -> ByteString -> Either Error ByteString
encryptMsgRSA seed pKey message = encryptWithSeed seed (defaultOAEPParams SHA256) pKey message
```

As we introduced point-free style last time, we omit `pKey message` on both sides.

```haskell
encryptMsgRSA :: Seed -> PublicKey -> ByteString -> Either Error ByteString
encryptMsgRSA seed = encryptWithSeed seed (defaultOAEPParams SHA256)
```

Further, we can use `$` to replace `( )`, and our final `encryptMsgRSA` is like this, it returns an Either type that either returns an error or the ciphertext.

```haskell
encryptMsgRSA :: Seed -> PublicKey -> ByteString -> Either Error ByteString
encryptMsgRSA seed = encryptWithSeed seed $ defaultOAEPParams SHA256
```

### Decryption
Decryption is pretty similar to the encryption. Just that it doesn't require the seed.

```haskell
decryptMsgRSA :: PrivateKey -> ByteString -> IO (Either Error ByteString)
decryptMsgRSA = decryptSafer $ defaultOAEPParams SHA256
```

## Test encryption and decryption in GHCi

Putting everything together, we can verify the properties as follow:

```
testEncryptRSA = do
  print "making a seed:"
  seed <- makeSeed
  print seed

  (pKey, sKey) <- makeKeyPair
  print (pKey, sKey)

  print "using RSA to encrypt message with the seed and the public key"
  let Right encrypted = encryptMsgRSA seed pKey "this is a secret message"
  print encrypted

  print "encrypting with the same seed and the same public key will produce the same cipher:"
  print $ encryptMsgRSA seed pKey "this is a secret message" == encryptMsgRSA seed pKey "this is a secret message"

  print "encrypting with a different seed will produce different ciphertext:"
  seed2 <- makeSeed
  print $ encryptMsgRSA seed2 pKey "this is a secret message" /= encryptMsgRSA seed pKey "this is a secret message"

  print "can decrypt the ciphertext with private key:"
  Right decrypted <- decryptMsgRSA sKey encrypted
  print $ "this is a secret message" == decrypted

  print "can't decrypt if the ciphertext was arbitrarily modified, will get error: MessageNotRecognized"
  Left messageNotRecognized <- decryptMsgRSA sKey $ reverse encrypted
  print messageNotRecognized

  print "can't decrypt a ciphertext which was encrypted by a different publick key, otherwise will get error: MessageNotRecognized"
  (hackerPKey, hackerSKey) <- makeKeyPair
  let Right encryptedByHacker = encryptMsgRSA seed hackerPKey "this is a secret message"
  Left messageNotRecoganizedFromHacker <- decryptMsgRSA sKey encryptedByHacker
  print messageNotRecoganizedFromHacker

  print "can't decrypt a ciphertext with a wrong private key, otherwise will get error: MessageNotRecognized"
  Left errNotRecognized <- decryptMsgRSA hackerSKey encrypted
  print errNotRecognized

```

And the above function prints:
```
stack ghci src/Asymmetric.hs

ghci> testEncryptRSA
"making a seed:"
"\133'\230\NUL&\STX\218\249jbH1\158\243`\181\DC3qqK\213\141\134\144\217\154\190LK~\200\\"
(PublicKey {public_size = 256, public_n = 22728419368979421347066298512398910104968308125856340176793439028325356567334897604986470009600257590811501788714530807536593065720809708886250830979112575689566403142226262726406023563361419861298270005483227294782293079178939932155248746073575591054847203127038799863623536142554703253904530023869351879221336521774416533747912021243469574968508676421815822580312561795192040001542936819255388302660084728588656395887420210374519709983272127093263888349026272152489192017958763056046864489793424950381197735711206323280586098644727397441863128304453063063923513649500332522625579481411183178271666317747763700825707, public_e = 3},PrivateKey {private_pub = PublicKey {public_size = 256, public_n = 22728419368979421347066298512398910104968308125856340176793439028325356567334897604986470009600257590811501788714530807536593065720809708886250830979112575689566403142226262726406023563361419861298270005483227294782293079178939932155248746073575591054847203127038799863623536142554703253904530023869351879221336521774416533747912021243469574968508676421815822580312561795192040001542936819255388302660084728588656395887420210374519709983272127093263888349026272152489192017958763056046864489793424950381197735711206323280586098644727397441863128304453063063923513649500332522625579481411183178271666317747763700825707, public_e = 3}, private_d = 15152279579319614231377532341599273403312205417237560117862292685550237711556598403324313339733505060541001192476353871691062043813873139257500553986075050459710935428150841817604015708907613240865513336988818196521528719452626621436832497382383727369898135418025866575749024095036468835936353349246234586147356337592326620582268390874281521344583464213476991936893885065414577753720335868536764959749740172312293343978372336982076755722418006466697261791048063957544464349239681452634853490621030191818233435071562376646999411545451278109606809960848325962530198062356274205990303149249068680066090320991271910029611, private_p = 159662400648676897763042008393831287515341755572103470634795231216724707218605750558021788787832346974612887773800554111382932714405491065554288693892408232966367823045155525405409321834740970078194536849195029198669621512730890875111134548785564677977218483744142048424473285363743183082534742643419253565793, private_q = 142352985277925976746392923653461664118138346028231204336938965853448663743827265892219074247642123145603492146061150790021643685239626327663706968561767983206127671054085351689174932027138692575653046254667729111417359813819589402341778814395009442150998072221779165215651472173836975089996093617436582215499, private_dP = 106441600432451265175361338929220858343561170381402313756530154144483138145737167038681192525221564649741925182533702740921955142936994043702859129261605488644245215363437016936939547889827313385463024566130019465779747675153927250074089699190376451984812322496094698949648856909162122055023161762279502377195, private_dQ = 94901990185283984497595282435641109412092230685487469557959310568965775829218177261479382831761415430402328097374100526681095790159750885109137979041178655470751780702723567792783288018092461717102030836445152740944906542546392934894519209596672961433998714814519443477100981449224650059997395744957721476999, private_qinv = 130158830349220348852962551153450430029148729230286650725797843977142536756081569635237952855988745865204610792721654485682608521777347267330033426363940920549388720255347203685116020480470187575157737424812032795592619016812743773973856327014289288374693857843020308354171647788363515085780786258870509581998})
"using RSA to encrypt message with the seed and the public key"
"\174\188\n(u\US\181S\GS\a\187?q\141Zy\254V^\SYN\242\188\139\184\215P\229\204<\245\190\155\139\168\DC3\140\145\176\136\GS\221z\171\SOHt*\SYN6\234\161\129\181*\CAN&\135\149n_\201\185\152\&5m\162w\244\&6\NUL\233\248\154\166\"\218\"\147\245\185r\181\219\245\160[)\172*)\215\144\143\SISK\243m\224H\142L\RSX\240\255\176up\148b\188\216U\fF\225q\157)\140\153up\161{\STX\162\241\140\131\207\139\204\&9:U\FS\185\135\192T`\NULZ\239f\191\220W\153\146|_\155fF(`\208M?sL\223[\ENQ\159\190\152[\154\180\158'\150U\187\149\139>\145f\139\156\246?\217\222\168k\139XF%#0\243\212|\205\209\219\EOT\ETX\STX\248\230Y\161;\221'\RS\154\195\146\EOT s0\\&>\198p\220\&8x\202\215\vY\GSU\SOH\176\246\174]|\r\217U\240\&3_\239\241\SUB\142pp\t\215\199\135"
"encrypting with the same seed and the same public key will produce the same cipher:"
True
"encrypting with a different seed will produce different ciphertext:"
True
"can decrypt the ciphertext with private key:"
True
"can't decrypt if the ciphertext was arbitrarily modified, will get error: MessageNotRecognized"
MessageNotRecognized
"can't decrypt a ciphertext which was encrypted by a different publick key, otherwise will get error: MessageNotRecognized"
MessageNotRecognized
"can't decrypt a ciphertext with a wrong private key, otherwise will get error: MessageNotRecognized"
MessageNotRecognized
```

Worth to note that RSA or Asymmetric key encryption algorithms, in general, is slow. It doesn't work well if the data to encrypt is very big. We can notice the slowness when evaluating `testEncryptRSA` in GHCi. It's much slower than symmetric key encryption algorithms like AES, which is fast.

In practice, asymmetric encryption algorithms are used to encrypt the key for decrypting a message encrypted by symmetric encryption algorithms. So that the combined usage of both asymmetric and symmetric key encryption algorithms can solve the key delivery problem and meanwhile still fast to encrypt messages in any size. We will talk more about how it works in future blog posts.

## Summary

In this blog post, we introduced how asymmetric key encryption works. And we took RSA as an example to go through the key creation and message encryption and decryption in Haskell. We learned a few Haskell syntax like `<-` to get the result from an IO Monad, and pattern matching to assign values to variables.
