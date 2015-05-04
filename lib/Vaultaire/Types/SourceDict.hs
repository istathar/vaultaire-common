--
-- Data vault for metrics
--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Vaultaire.Types.SourceDict
(
    SourceDict(..),
    unionSource,
    diffSource,
    lookupSource,
    hashSource,
    makeSourceDict
) where

import Blaze.ByteString.Builder (fromByteString, toByteString)
import Blaze.ByteString.Builder.Char8 (fromChar)
import Control.Applicative (many, optional, (<$>), (<*), (<*>))
import Control.Arrow ((***))
import Control.Exception (SomeException (..))
import Crypto.MAC.SipHash
import Data.Attoparsec.Text (parseOnly)
import qualified Data.Attoparsec.Text as PT
import Data.HashMap.Strict (HashMap, difference, foldlWithKey', fromList,
                            lookup, toList, union)
import Data.List (sortBy)
import Data.Maybe (isNothing)
import Data.Monoid (Monoid, mempty, (<>))
import Data.Ord (comparing)
import Data.Serialize
import Data.Text (Text, find, pack, unpack)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Word
import Prelude hiding (lookup)
import Test.QuickCheck
import Vaultaire.Classes.WireFormat

newtype SourceDict = SourceDict { unSourceDict :: HashMap Text Text }
  deriving (Eq, Monoid)

makeSourceDict :: HashMap Text Text -> Either String SourceDict
makeSourceDict hm = if foldlWithKey' allGoodKV True hm
                    then Right $ SourceDict hm
                    else Left "Bad character in source dict,\
                              \ no ',' or ':' allowed."
  where allGoodKV acc k v = acc && (allGoodChars k && allGoodChars v)
        allGoodChars = isNothing . find (\c -> c == ':' || c == ',')

instance Show SourceDict where
  show (SourceDict sd) = "dict=" <> show (toList sd)

instance WireFormat SourceDict where
    fromWire bs = either (Left . SomeException) parse (decodeUtf8' bs)
      where
        parse t = either (Left . SomeException . userError)
                         (Right . SourceDict . fromList)
                         (parseOnly tagParser t)

        tagParser = many $ (,) <$> k <*> v
          where
            k = PT.takeWhile (/= ':') <* ":"
            v = PT.takeWhile (/= ',') <* optional ","

    toWire = toByteString . foldlWithKey' f mempty . unSourceDict
      where
        f acc k v = acc <> text k <> fromChar ':' <> text v <> fromChar ','
        text = fromByteString . encodeUtf8

instance Arbitrary SourceDict where
    arbitrary = do
        attempt <- fromList . map (pack *** pack) <$> arbitrary
        either (const arbitrary) return $ makeSourceDict attempt

-- | Wrapped HashMap.union for SourceDicts
unionSource :: SourceDict -> SourceDict -> SourceDict
unionSource (SourceDict a) (SourceDict b) = SourceDict $ union a b

-- | Wrapped HashMap.difference for SourceDicts
diffSource :: SourceDict -> SourceDict -> SourceDict
diffSource (SourceDict a) (SourceDict b) = SourceDict $ difference a b

-- | Wrapped HashMap.lookup for SourceDicts
lookupSource :: Text -> SourceDict -> Maybe Text
lookupSource key sd = lookup key $ unSourceDict sd

-- | Hashes the sourcedict using SipHash
-- Hashes are used primarily to avoid redundant updates
hashSource :: SourceDict -> Word64
hashSource (SourceDict sd) =
    let canonicalList = sortBy (comparing fst) (map (unpack *** unpack) $ toList sd) in
    let (SipHash ret) = hash (SipKey 0 0) (encode canonicalList) in
    ret
