{-# LANGUAGE OverloadedStrings #-}

module Asymmetric where

import Prelude hiding (length, reverse)
import Data.ByteString (ByteString(..), length, reverse)
import Crypto.PubKey.RSA.OAEP (encryptWithSeed, defaultOAEPParams, decryptSafer)
import Crypto.PubKey.RSA.Types (PublicKey, PrivateKey, Error(..))
import Crypto.PubKey.RSA (generate)
import Crypto.Hash.Algorithms (SHA256(..))
import Crypto.Hash (Digest(..))
import qualified Crypto.Random.Types as CRT

type Seed = ByteString

makeSeed :: IO Seed
makeSeed = CRT.getRandomBytes 32 -- 32 bytes = 256 bits

makeKeyPair :: IO (PublicKey, PrivateKey)
makeKeyPair = generate size e
  where
    size = 256
    e = 3

encryptMsgRSA :: Seed -> PublicKey -> ByteString -> Either Error ByteString
encryptMsgRSA seed = encryptWithSeed seed $ defaultOAEPParams SHA256

decryptMsgRSA :: PrivateKey -> ByteString -> IO (Either Error ByteString)
decryptMsgRSA = decryptSafer $ defaultOAEPParams SHA256

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
