{-# LANGUAGE OverloadedStrings #-}

module OneWayHashFunction where

import Data.ByteString (ByteString)
import Crypto.Hash (Digest, hash, SHA256, Keccak_256)

messageHashSHA256 :: ByteString -> Digest SHA256
messageHashSHA256 = hash

messageHashKeccak256 :: ByteString -> Digest Keccak_256
messageHashKeccak256 = hash

testMessageHashSHA256 :: IO ()
testMessageHashSHA256 = do
  print "SHA256 digest of 'hello'"
  print $ messageHashSHA256 "hello"

  print "SHA256 digest of 'hello world'"
  print $ messageHashSHA256 "hello world"

  print "SHA256 digest of 'hello world' again"
  print $ messageHashSHA256 "hello world"

  print "SHA256 digest of a long message"
  print $ messageHashSHA256 "hello world. this is a very very very very very very very very very very very long message"

  print "SHA256 digest of 'hello worlD'"
  print $ messageHashSHA256 "hello worlD"

  print "Keccak-256 digest of 'hell world'"
  print $ messageHashKeccak256 "hello worlD"
