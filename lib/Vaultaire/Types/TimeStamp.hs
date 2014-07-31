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
    convertToDiffTime,
    getCurrentTimeNanoseconds
) where

import Control.Applicative
import Data.Packer (getWord64LE, putWord64LE, runPacking, tryUnpacking)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Word (Word64)
import Test.QuickCheck

import Vaultaire.Classes.WireFormat

--
-- | Number of nanoseconds since the Unix epoch, stored in a Word64.
--
-- The Show instance displays the TimeStamp as seconds with the nanosecond precision expressed as
-- a decimal amount after the interger, ie:
--
-- >>> t <- getCurrentTimeNanoseconds
-- >>> show t
-- 1405656663.561632s
--
-- However this doesn't change the fact the underlying representation counts
-- nanoseconds since epoch:
--
-- >>> show $ unTimeStamp t
-- 1405656663561632000
--
newtype TimeStamp = TimeStamp {
    unTimeStamp :: Word64
} deriving (Eq, Num, Bounded, Enum, Ord, Real, Integral)

{-
instance Show TimeStamp where
    show = show . convertToDiffTime
-}

instance Show TimeStamp where
  show = show . unTimeStamp

instance Read TimeStamp where
  readsPrec _ s = maybeToList $ (,"") <$> Time <$> toNano <$> parse s
    where
      toNano :: UTCTime -> Word64
      toNano =  (*10^(9 :: Word64)) . read . formatTime defaultTimeLocale "%s"

      parse :: String -> Maybe UTCTime
      parse x =   parseTime defaultTimeLocale "%FT%XZ" x
              <|> parseTime defaultTimeLocale "%F" x


instance WireFormat TimeStamp where
    toWire = runPacking 8 . putWord64LE . unTimeStamp
    fromWire = tryUnpacking (TimeStamp `fmap` getWord64LE)

instance Arbitrary TimeStamp where
    arbitrary = TimeStamp <$> arbitrary

--
-- | Utility function to convert nanoseconds since Unix epoch to a
-- 'NominalDiffTime', allowing you to then use the time manipulation
-- functions in "Data.Time.Clock"
--
convertToDiffTime :: TimeStamp -> NominalDiffTime
convertToDiffTime = fromRational . (/ 1e9) . fromIntegral

--
-- | Get the current system time, expressed as a 'TimeStamp' (which is to
-- say, number of nanoseconds since the Unix epoch).
--
{-
    getPOSIXTime returns a NominalDiffTime with picosecond precision. So
    convert it to nanoseconds, and discard any remaining fractional amount.
-}
getCurrentTimeNanoseconds :: IO TimeStamp -- Word64
getCurrentTimeNanoseconds = do
    t <- getPOSIXTime
    let nanos = ((* 1e9) . fromRational . toRational) t :: Double
    let i = (TimeStamp . floor) nanos
    return i


