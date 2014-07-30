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

-- | Test serialisation/deserialiastion for Vaultaire types.

import Test.Hspec
import Vaultaire.Types

main :: IO ()
main = hspec suite

suite :: Spec
suite = do
  describe "Test serialisation/deserialiastion" $ do
    it "serialises Zulu time" $ do
      read "1970-01-01" `shouldBe` (0 :: Time)

    it "deserialises into Unix time by default" $ do
      show $ read "1970-01-01" `shouldBe` "0"
