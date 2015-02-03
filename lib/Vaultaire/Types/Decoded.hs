module Vaultaire.Types.Decoded
  ( SimplePoint(..)
  , ExtendedPoint(..)
  ) where

import Data.Word (Word64)
import Data.ByteString (ByteString)
import Vaultaire.Types.Address
import Vaultaire.Types.TimeStamp

-- | SimplePoints are simply wrapped packets for Vaultaire
-- Each consists of 24 bytes:
-- An 8 byte Address
-- An 8 byte Timestamp (nanoseconds since Unix epoch)
-- An 8 byte Payload
data SimplePoint = SimplePoint { simpleAddress :: {-# UNPACK #-} !Address
                               , simpleTime    :: {-# UNPACK #-} !TimeStamp
                               , simplePayload :: {-# UNPACK #-} !Word64 }
  deriving (Show, Eq)


-- | ExtendedPoints are simply wrapped packets for Vaultaire
-- Each consists of 16 + 'length' bytes:
-- An 8 byte Address
-- An 8 byte Time (in nanoseconds since Unix epoch)
-- A 'length' byte Payload
-- On the wire their equivalent representation takes up
-- 24 + 'length' bytes with format:
-- 8 byte Address, 8 byte Time, 8 byte Length, Payload
data ExtendedPoint = ExtendedPoint { extendedAddress :: Address
                                   , extendedTime    :: TimeStamp
                                   , extendedPayload :: ByteString }
  deriving (Show, Eq)
