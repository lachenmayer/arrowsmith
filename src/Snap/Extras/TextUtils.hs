{-# LANGUAGE NoMonomorphismRestriction #-}

module Snap.Extras.TextUtils
    ( readT
    , showT
    , readBS
    , showBS
    , titleCase
    ) where

-------------------------------------------------------------------------------
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Char8 (ByteString)
import           Data.Char
import qualified Data.Text             as T
import           Data.Text             (Text)
import           Safe
-------------------------------------------------------------------------------


showT :: (Show a) => a -> Text
showT = T.pack . show


showBS :: (Show a) => a -> ByteString
showBS = B.pack . show


readT :: (Read a) => Text -> a
readT = readNote "Can't read value in readT" . T.unpack



readBS :: (Read a) => ByteString -> a
readBS = readNote "Can't read value in readBS" . B.unpack


titleCase :: Text -> Text
titleCase = T.unwords . map upFirst . T.words
  where
    upFirst str = T.cons (toUpper $ T.head str) (T.tail str)
