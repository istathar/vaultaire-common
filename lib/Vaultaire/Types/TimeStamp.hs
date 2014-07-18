--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vaultaire.Types.TimeStamp
(
    TimeStamp(..),
    convertToDiffTime
) where

import Control.Applicative
import Data.Packer (getWord64LE, putWord64LE, runPacking, tryUnpacking)
import Data.String
import Data.Time.Clock
import Data.Word (Word64)
import Test.QuickCheck

import Vaultaire.Classes.WireFormat

newtype TimeStamp = TimeStamp {
    unTimeStamp :: Word64
} deriving (Eq, Num, Bounded, Enum, Ord, Real, Integral)

instance Show TimeStamp where
    show = show . convertToDiffTime


instance WireFormat TimeStamp where
    toWire = runPacking 8 . putWord64LE . unTimeStamp
    fromWire = tryUnpacking (TimeStamp `fmap` getWord64LE)

instance Arbitrary TimeStamp where
    arbitrary = TimeStamp <$> arbitrary

convertToDiffTime :: TimeStamp -> NominalDiffTime
convertToDiffTime = fromRational . (/ 1e9) . fromIntegral

