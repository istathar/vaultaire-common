--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Vaultaire.Types.Address
(
    Address(..),
    calculateBucketNumber,
    isAddressExtended,
) where

import Control.Applicative
import Data.Bits
import Data.Locator
import Data.Packer (getWord64LE, putWord64LE, runPacking, tryUnpacking)
import Data.String
import Data.Word (Word64)
import Test.QuickCheck

import Vaultaire.Classes.WireFormat

newtype Address = Address {
    unAddress :: Word64
} deriving (Eq, Ord, Hashable, Num, Bounded)

instance Read Address where
    readsPrec _ = pure . (,"") . Address . fromInteger . fromBase62

instance Show Address where
    show = padWithZeros 11 . toBase62 . toInteger . unAddress

instance IsString Address where
    fromString = fromIntegral . fromBase62

-- | There are assumptions made that the encoding of Address is fixed-length (8
-- bytes). Changing that will break things subtly.
instance WireFormat Address where
    toWire = runPacking 8 . putWord64LE . unAddress
    fromWire = tryUnpacking (Address `fmap` getWord64LE)

instance Arbitrary Address where
    arbitrary = Address <$> arbitrary

calculateBucketNumber :: Word64 -> Address -> Word64
calculateBucketNumber num_buckets (Address addr) = (addr `clearBit` 0) `mod` num_buckets

isAddressExtended :: Address -> Bool
isAddressExtended (Address addr) = addr `testBit` 0

