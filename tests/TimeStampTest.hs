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

import Test.Hspec
import Vaultaire.Types

main :: IO ()
main = hspec suite

suite :: Spec
suite = do
  describe "Operations on TimeStamp" $ do
    it "adds to TimeStamp" $ do
      let bang = read "1970-01-01" :: TimeStamp
      now <- getCurrentTimeNanoseconds
      addTimeStamp 0                       now  `shouldBe` now
      addTimeStamp (convertToDiffTime now) bang `shouldBe` now
      addTimeStamp 1 (addTimeStamp (-1) now)    `shouldBe` now
