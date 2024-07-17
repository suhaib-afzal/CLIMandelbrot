module ParsingSpec ( spec ) where

import Test.Hspec
import Parsing
import Datatypes ( Nat(Nil, Succ) )
import Lib (Palette(Palette))
import Data.Colour.SRGB ( sRGB24 )

spec :: Spec
spec = do
  describe "parseNatural function" $ do
    describe "failure cases" $ do
      it "fails when passing in something unparsable as integer" $ do
        parseNatural "jsda" `shouldBe` Left "Cannot parse a passed in value"
        parseNatural "kdfmd" `shouldBe` Left "Cannot parse a passed in value"
        parseNatural "1kid:" `shouldBe` Left "Cannot parse a passed in value"
      it "fails with negative integers" $ do
        parseNatural "-10" `shouldBe` Left "Number passed in less than 0 and so is not natural"
        parseNatural "-9" `shouldBe` Left "Number passed in less than 0 and so is not natural"
        parseNatural "-897" `shouldBe` Left "Number passed in less than 0 and so is not natural"
    describe "success cases" $ do
      it "succeeds at converted correct inputs" $ do
        parseNatural "5" `shouldBe` Right (Succ (Succ (Succ (Succ (Succ Nil)))))
        parseNatural "0" `shouldBe` Right Nil
        parseNatural "7" `shouldBe` Right (Succ (Succ (Succ (Succ (Succ (Succ (Succ Nil)))))))
        parseNatural "2" `shouldBe` Right (Succ (Succ Nil))
  describe "parsePalette" $ do
    describe "failure cases" $ do
      it "fails with invalid input" $ do
        parsePalette "" `shouldBe` Left "Palette input is empty"
        parsePalette "172 98 7, 899 2 3, 7 2 3" `shouldBe` Left "Please ensure you bracket the palette list"
        parsePalette "(172 98 7, 899 2 3, 7 2 3" `shouldBe` Left "Please ensure your bracketing is balanced"
        parsePalette "(172 98 7, 899 2 3)), 7 2 3" `shouldBe` Left "Please ensure your bracketing is balanced"
        parsePalette "(172 98 7, 899 2 3, 7 2 3)" `shouldBe` Left "Please ensure you include values for the list and single rgb"
        parsePalette ", 7 3 7" `shouldBe` Left "Please ensure you bracket the palette list"
        parsePalette "(), 7 3 7" `shouldBe` Left "Empty brackets passed in for list"
        parsePalette "(,,78 87 3), 7 3 7" `shouldBe` Left "Please ensure there are no empty terms in the Palette List"
        parsePalette "(   ,78 87 3), 7 3 7" `shouldBe` Left "Please ensure there are no empty terms in the Palette List"
        parsePalette "(172 98 7, 100 2), 7 2 3" `shouldBe` Left "Please ensure each element in the Palette is 3 numbers"
        parsePalette "(172 98 7, 100 2 abc), 7 2 3" `shouldBe` Left "Cannot parse an integer number passed in as part of the palette"
      it "succeeds with valid input" $ do
        parsePalette "(172 98 7, 100 2 3), 7 2 3" `shouldBe` Right (Palette [sRGB24 172 98 7, sRGB24 100 2 3] (sRGB24 7 2 3))
        parsePalette "(172 98 7, 100 2 3, 2 3 4), 7 2 3" `shouldBe` Right (Palette [sRGB24 172 98 7, sRGB24 100 2 3, sRGB24 2 3 4] (sRGB24 7 2 3))
        parsePalette "(12 9 70, 10 24 70, 2 35 4, 3 6 1, 54 3 67), 7 9 0" `shouldBe` Right (Palette [sRGB24 12 9 70, sRGB24 10 24 70, sRGB24 2 35 4, sRGB24 3 6 1, sRGB24 54 3 67] (sRGB24 7 9 0))
