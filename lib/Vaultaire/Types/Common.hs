--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Vaultaire.Types.Common
(
    Origin(..),
    makeOrigin,
    Epoch,
    NumBuckets
) where

import Control.Applicative
import Control.Exception (Exception, SomeException (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import Data.Char
import Data.Hashable (Hashable)
import Data.Locator
import Data.String (IsString)
import Data.Typeable (Typeable)
import Data.Word (Word64)
import Test.QuickCheck
import Data.Either

-- |Origin is a six-character ByteString representing a data origin.
newtype Origin = Origin { unOrigin :: ByteString }
    deriving (Eq, Ord, IsString, Hashable)

instance Arbitrary Origin where
    -- suchThat condition should be removed once locators package is fixed
    arbitrary = Origin . S.pack . toLocator16a 6 <$> arbitrary `suchThat` (>0)

instance Read Origin where
    readsPrec _ = fmap (,"") . rights . (:[]) . makeOrigin . S.pack

instance Show Origin where
    show = S.unpack . unOrigin

-- | Invalid origin Exception
data BadOrigin = BadOrigin
    deriving (Show, Typeable)

instance Exception BadOrigin

makeOrigin :: ByteString -> Either SomeException Origin
makeOrigin bs
    | S.null bs = Left (SomeException BadOrigin)
    | S.any (not . isAlphaNum) bs = Left (SomeException BadOrigin)
    | otherwise = Right (Origin bs)

-- These can all be newtype wrapped as make work, perhaps excluding DayMap.
-- They have no reason to be inter-mixed.

type Epoch = Word64
type NumBuckets = Word64
