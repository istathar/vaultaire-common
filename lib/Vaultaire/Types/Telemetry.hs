{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Vaultaire.Types.Telemetry
     ( TeleResp(..)
     , TeleMsg(..)
     , TeleMsgType(..)
     , TeleMsgUOM(..)
     , msgTypeUOM
     , AgentID, agentIDLength, agentID )
where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import Data.Monoid
import Data.Packer
import Data.Word
import Test.QuickCheck

import Vaultaire.Classes.WireFormat
import Vaultaire.Types.Common
import Vaultaire.Types.TimeStamp

-- | ID string associated with a running daemon, so telemetry messages
--   can be associated with the processes which sent them.
newtype AgentID = AgentID String
        deriving (Eq, Ord, Monoid)

-- | Response for a telemetry request, sent by the profiler to clients.
data TeleResp = TeleResp
     { _timestamp :: TimeStamp
     , _aid       :: AgentID
     , _msg       :: TeleMsg
     } deriving Eq

-- | The actual telemetric data, reported by Vaultaire worker threads
--   to their profiler.
--
data TeleMsg = TeleMsg
     { _origin  :: Origin
     , _type    :: TeleMsgType
     , _payload :: Word64
     } deriving Eq

-- | Telemetry types. All counts are absolute and all latencies are in milliseconds.
data TeleMsgType
   = WriterSimplePoints       -- ^ Total number of simple points written since last message
   | WriterExtendedPoints     -- ^ Total number of extended points written since last message
   | WriterRequest            -- ^ Total number of write requests received since last message
   | WriterRequestLatency     -- ^ Mean latency for one request
   | WriterCephLatency        -- ^ Mean Ceph latency for one request
   | ReaderSimplePoints       -- ^ Total number of simple points read since last message
   | ReaderExtendedPoints     -- ^ Total number of extended points read since last message
   | ReaderRequest            -- ^ Total number of read requests received since last message
   | ReaderRequestLatency     -- ^ Mean latency for one request
   | ReaderCephLatency        -- ^ Mean Ceph latency for one request
   | ContentsEnumerate        -- ^ Total number of enumerate requests received since last message
   | ContentsUpdate           -- ^ Total number of update requests received since last message
   | ContentsEnumerateLatency -- ^ Mean latency for one enumerate request
   | ContentsUpdateLatency    -- ^ Mean latency for one update request
   | ContentsEnumerateCeph    -- ^ Mean Ceph latency for one enumerate request
   | ContentsUpdateCeph       -- ^ Mean Ceph latency for one update request
   deriving (Enum, Bounded, Eq, Ord)

data TeleMsgUOM
    = Points
    | Requests
    | Milliseconds
    deriving (Enum, Bounded, Eq, Ord)

instance Show TeleMsgUOM where
  show Points       = "points"
  show Requests     = "requests"
  show Milliseconds = "ms"

-- | Map a telemetry message type onto its associated UOM.
msgTypeUOM :: TeleMsgType -> TeleMsgUOM
msgTypeUOM WriterSimplePoints       = Points
msgTypeUOM WriterExtendedPoints     = Points
msgTypeUOM WriterRequest            = Requests
msgTypeUOM WriterRequestLatency     = Milliseconds
msgTypeUOM WriterCephLatency        = Milliseconds
msgTypeUOM ReaderSimplePoints       = Points
msgTypeUOM ReaderExtendedPoints     = Points
msgTypeUOM ReaderRequest            = Requests
msgTypeUOM ReaderRequestLatency     = Milliseconds
msgTypeUOM ReaderCephLatency        = Milliseconds
msgTypeUOM ContentsEnumerate        = Requests
msgTypeUOM ContentsUpdate           = Requests
msgTypeUOM ContentsEnumerateLatency = Milliseconds
msgTypeUOM ContentsUpdateLatency    = Milliseconds
msgTypeUOM ContentsEnumerateCeph    = Milliseconds
msgTypeUOM ContentsUpdateCeph       = Milliseconds

-- | Return (possibly empty) prefix component of a ByteString terminated
--   by one or more null bytes.
chomp :: ByteString -> ByteString
chomp = S.takeWhile (/='\0')

-- | Agent IDs are a maximum of 64 bytes.
agentIDLength :: Int
agentIDLength = 64

-- | An agent ID has to fit in 64 characters and does not contain \NUL.
agentID :: String -> Maybe AgentID
agentID s | length s <= agentIDLength && notElem '\0' s
          = Just $ AgentID s
          | otherwise = Nothing

putAgentID :: AgentID -> Packing ()
putAgentID (AgentID x)
  = putBytes $ S.pack $ x ++ replicate (64 - length x) '\0'

getAgentID :: Unpacking AgentID
getAgentID = AgentID . S.unpack . chomp <$> getBytes 64

-- | Pack a telemetry message. Assumes the origin is no longer than
--   eight bytes.
putTeleMsg :: TeleMsg -> Packing ()
putTeleMsg x = do
    -- 8 bytes for the origin.
    let o = unOrigin $ _origin x
    putBytes    $ S.append o $ S.pack $ replicate (8 - S.length o) '\0'
    -- 8 bytes for the message type.
    putWord64LE $ fromIntegral $ fromEnum $ _type x
    -- 8 bytes for the payload
    putWord64LE $ _payload x

getTeleMsg :: Unpacking (Either SomeException TeleMsg)
getTeleMsg = do
    o <- makeOrigin . chomp <$> getBytes 8
    t <- toEnum . fromIntegral <$> getWord64LE
    p <- getWord64LE
    return $ fmap (\org -> TeleMsg org t p) o

instance WireFormat AgentID where
  toWire   = runPacking 64 . putAgentID
  fromWire = tryUnpacking    getAgentID

instance WireFormat TeleMsg where
  toWire   = runPacking 24 . putTeleMsg
  fromWire = runUnpacking getTeleMsg

instance WireFormat TeleResp where
  toWire x = runPacking 96 $ do
    -- 8 bytes for the timestamp
    putWord64LE $ unTimeStamp $ _timestamp x
    -- 64 bytes for the agent ID, padded out with nuls
    putAgentID  $ _aid x
    -- 24 bytes for the message (origin, type and payload)
    putTeleMsg  $ _msg x
  fromWire x = join $ flip tryUnpacking x $ do
    s <- TimeStamp <$> getWord64LE
    a <- getAgentID
    m <- getTeleMsg
    return $ TeleResp s a <$> m


instance Arbitrary TeleMsg where
  arbitrary =   TeleMsg
            <$> arbitrary
            <*> arbitrary
            <*> arbitrary

instance Arbitrary TeleResp where
  arbitrary =   TeleResp
            <$> arbitrary
            <*> arbitrary
            <*> arbitrary

instance Arbitrary TeleMsgType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary AgentID where
  arbitrary = untilG agentID arbitrary
    where untilG :: (Arbitrary a, Arbitrary b) => (a -> Maybe b) -> Gen a -> Gen b
          untilG f a = a >>= maybe arbitrary return . f


instance Show AgentID where
  show (AgentID s) = s

instance Show TeleMsgType where
  show WriterSimplePoints       = "writer-count-simple-point      "
  show WriterExtendedPoints     = "writer-count-extended-point    "
  show WriterRequest            = "writer-count-request           "
  show WriterRequestLatency     = "writer-latency-request         "
  show WriterCephLatency        = "writer-latency-ceph            "
  show ReaderSimplePoints       = "reader-count-simple-point      "
  show ReaderExtendedPoints     = "reader-count-extended-point    "
  show ReaderRequest            = "reader-count-request           "
  show ReaderRequestLatency     = "reader-latency-request         "
  show ReaderCephLatency        = "reader-latency-ceph            "
  show ContentsEnumerate        = "contents-count-enumerate       "
  show ContentsUpdate           = "contents-count-update          "
  show ContentsEnumerateLatency = "contents-latency-enumerate     "
  show ContentsUpdateLatency    = "contents-latency-update        "
  show ContentsEnumerateCeph    = "contents-latency-ceph-enumerate"
  show ContentsUpdateCeph       = "contents-latency-ceph-update   "

instance Show TeleResp where
  show r = concat [ show $ _timestamp r, " "
                  , show $ _aid r,       " "
                  , show $ _msg r]

instance Show TeleMsg where
  show m = concat [ show $ _origin m, " "
                  , show $ _type m,   " "
                  , let s = show (fromIntegral $ _payload m :: Int)
                    in  replicate (8 - length s) ' ' ++ s
                  , " "
                  , show $ msgTypeUOM $ _type m ]
