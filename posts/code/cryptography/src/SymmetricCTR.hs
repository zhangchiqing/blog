{-# LANGUAGE OverloadedStrings #-}

module SymmetricCTR where

import Data.ByteString (ByteString)
import Crypto.Cipher.AES (AES256(..))
import Crypto.Cipher.Types (cipherInit, IV, ivAdd, nullIV, ctrCombine)
import Crypto.Error (CryptoError(..), eitherCryptoError)
import Data.ByteArray (unpack)
import GHC.Word (Word8)

makeIVFromInt :: Int -> IV AES256
makeIVFromInt = ivAdd nullIV

printIV :: IV AES256 -> [Word8]
printIV = unpack

makeSecretKey :: ByteString -> Either CryptoError AES256
makeSecretKey secret = eitherCryptoError (cipherInit secret)

secret :: ByteString
secret = "012-456-89A-CDE-012-456-89A-CDE-"

encryptMsgCTR :: AES256 -> IV AES256 -> ByteString -> ByteString
encryptMsgCTR = ctrCombine

decryptMsgCTR :: AES256 -> IV AES256 -> ByteString -> ByteString
decryptMsgCTR = ctrCombine

testCTREncryption :: IO ()
testCTREncryption = do
  print "Can create and print IV:"
  print $ printIV $ makeIVFromInt 100

  let Right sKey = makeSecretKey secret
  print "Can encrypt plain text into cipher text with IV and secret in CTR mode:"
  print $ encryptMsgCTR sKey (makeIVFromInt 100) "this is a secret"

  print "Can decrypt back to original plain text"
  print $ decryptMsgCTR sKey (makeIVFromInt 100) $ encryptMsgCTR sKey (makeIVFromInt 100) "this is a secret"

  print "done"
