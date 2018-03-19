{-# LANGUAGE OverloadedStrings #-}

module DiffieHellman where

import Crypto.PubKey.DH
import Crypto.Cipher.Types (IV(..), ecbEncrypt, ecbDecrypt, ctrCombine, makeIV, blockSize, cipherInit)
import Crypto.Cipher.AES (AES256(..))
import Data.ByteString (ByteString, pack)
import Data.ByteArray (unpack)
import SymmetricCTR

publicPG :: IO Params
publicPG = generateParams bits generator
  where
    bits = 256
    generator = 2

testDH :: IO SharedKey
testDH = do
  print "generate publicly sharable p and g, p is 256 bits prime, g is a small prime (2)"
  pg <- publicPG
  print pg

  print "alice generates a private random number"
  alicePrivateNumber <- generatePrivate pg

  print "alice calculates a public number ( G^A mod P)"
  let alicePublicNumber = calculatePublic pg alicePrivateNumber
  print (alicePrivateNumber, alicePublicNumber)

  print "bob generates a private random number"
  bobPrivateNumber <- generatePrivate pg

  print "bob calculates a public number ( G^B mod P)"
  let bobPublicNumber = calculatePublic pg bobPrivateNumber
  print (bobPrivateNumber, bobPublicNumber)

  print "alice calcluates the shared key"
  let aliceSharedKey = getShared pg alicePrivateNumber bobPublicNumber

  print "bob calcluates the shared key"
  let bobSharedKey = getShared pg bobPrivateNumber alicePublicNumber

  print "alice and bob should get the same key without exposing their private numbers"
  print $ aliceSharedKey == bobSharedKey

  return aliceSharedKey

makeSharedKey :: SharedKey -> ByteString
makeSharedKey = pack . unpack

testEncryptWithSharedKey = do
  sharedKey <- testDH
  -- ByteString
  let Right sKey = makeSecretKey $ makeSharedKey sharedKey

  let initIV = makeIVFromInt 100
  print $ encryptMsgCTR sKey initIV "this is a message"
  print $ decryptMsgCTR sKey initIV $ encryptMsgCTR sKey initIV "this is a message encrypted by a shared key"
