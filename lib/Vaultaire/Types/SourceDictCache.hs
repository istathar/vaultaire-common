module Vaultaire.Types.SourceDictCache
(
    SourceDictCache,
    emptySourceCache,
    insertSourceCache,
    memberSourceCache,
    sizeOfSourceCache
) where

import Control.Applicative
import Data.Packer
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word

import Vaultaire.Classes.WireFormat

newtype SourceDictCache = SourceDictCache {
    unSourceDictCache :: Set Word64
}

instance WireFormat SourceDictCache where
    toWire (SourceDictCache sdc) =
        let elems = Set.elems sdc in
        runPacking (length elems * 8) (mapM putWord64 elems)
    fromWire bs = Right $ SourceDictCache $ Set.fromList $ runUnpacking (many getWord64) bs

emptySourceCache :: SourceDictCache
emptySourceCache = SourceDictCache Set.empty

insertSourceCache :: Word64 -> SourceDictCache -> SourceDictCache
insertSourceCache x (SourceDictCache sdc) = SourceDictCache (Set.insert x sdc)

memberSourceCache :: Word64 -> SourceDictCache -> Bool
memberSourceCache x (SourceDictCache sdc) = Set.member x sdc

sizeOfSourceCache :: SourceDictCache -> Int
sizeOfSourceCache (SourceDictCache sdc) = Set.size sdc
