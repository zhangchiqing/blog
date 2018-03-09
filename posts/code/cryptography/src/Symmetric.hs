{-# LANGUAGE OverloadedStrings #-}

module Symmetric where

import Data.ByteString (ByteString)
import Crypto.Cipher.AES (AES256(..))
import Crypto.Cipher.Types (ecbEncrypt, ecbDecrypt, cipherInit)
import Crypto.Error (CryptoError(..), eitherCryptoError)

makeSecretKey :: ByteString -> Either CryptoError AES256
makeSecretKey secret = eitherCryptoError (cipherInit secret)

secret :: ByteString
secret = "012-456-89A-CDE-012-456-89A-CDE-"

encryptMsgECB :: AES256 -> ByteString -> ByteString
encryptMsgECB = ecbEncrypt

decryptMsgECB :: AES256 -> ByteString -> ByteString
decryptMsgECB secKey msg = ecbDecrypt secKey msg


