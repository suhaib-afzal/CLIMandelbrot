module LibSpec ( spec ) where

import Test.Hspec
import Lib ( ComputationInfo(ComputationInfo)
           , Palette(Palette)
           , Zoom(Zoom)
           , plot )
import Datatypes ( zero
                 , one
                 , two
                 , four
                 , eight
                 , Vector (VNil, (:*))
                 , Matrix (MNil, (:^))
                 , singleton )

import Data.Complex ( Complex( (:+) ) )
import Data.Colour ( black , Colour )
import Data.Colour.SRGB ( sRGB24 )

spec :: Spec
spec = do
  describe "plot Function" $ do
    describe "failure cases" $ do
      it "fails when no non-set colours are assigned" $ do
        let compInfo = ComputationInfo const (1 :: Double) (0 :+ 0) 10
        let white = sRGB24 255 255 255
        let palette  = Palette [] black 
        let zoom     = Zoom (0 :+ 0) (0 :+ 1)
        plot one one compInfo palette zoom `shouldBe` Left "The palette is incomplete. We need at least one colour to denote points that dont lie in the set"
      it "fails when zoom factor is 0" $ do
        let compInfo = ComputationInfo const (1 :: Double) (0 :+ 0) 10
        let white = sRGB24 255 255 255
        let palette  = Palette [white] black 
        let zoom = Zoom (0 :+ 0) (0 :+ 0)
        plot one one compInfo palette zoom `shouldBe` Left "The factor given for the zoom is 0 + 0i, this is an invalid zoom factor"
      it "fails when height or width are 0" $ do
        let compInfo = ComputationInfo const (1 :: Double) (0 :+ 0) 10
        let white = sRGB24 255 255 255
        let palette  = Palette [white] black  
        let zoom = Zoom (0 :+ 0) (0 :+ 1)
        plot zero one compInfo palette zoom  `shouldBe` Left "The height value is 0"
        plot one zero compInfo palette zoom  `shouldBe` Left "The width value is 0"
        plot zero zero compInfo palette zoom `shouldBe` Left "The height width values are 0"
    describe "happy cases" $ do
      it "with single square frame, identity function, no zoom, and an orbit radius of 1 we get single pixel representing entire frame, with inside set colour" $ do
        let compInfo = ComputationInfo const (1 :: Double) (0 :+ 0) 10
        let white = sRGB24 255 255 255
        let palette  = Palette [white] black 
        let zoom = Zoom (0 :+ 0) (1 :+ 0)
        plot one one compInfo palette zoom `shouldBe` Right (singleton black)
      it "with 1x2 frame, identity function, no zoom, and an orbit radius of 1 we get two pixels represnting the left and right halves of the frame, both have outside set colour" $ do
        -- We have outside set colour for both as their centres land on -1 + 0i and 1 + 0i which lie on the edge of the orbit radius.
        let compInfo = ComputationInfo const (1 :: Double) (0 :+ 0) 10
        let white = sRGB24 255 255 255
        let palette  = Palette [white] black 
        let zoom = Zoom (0 :+ 0) (1 :+ 0)
        plot one two compInfo palette zoom `shouldBe` Right ((white :* white :* VNil) :^
                                                             MNil)
      it "with 2x1 frame, identity function, no zoom, and an orbit radius of 1 we get two pixels represnting the top and bottom halves of the frame, both have outside set colour" $ do
        -- We have outside set colour for both as their centres land on 0 - 1i and 0 + 1i which lie on the edge of the orbit radius.j
        let compInfo = ComputationInfo const (1 :: Double) (0 :+ 0) 10
        let white = sRGB24 255 255 255
        let palette  = Palette [white] black 
        let zoom = Zoom (0 :+ 0) (1 :+ 0)
        plot two one compInfo palette zoom `shouldBe` Right ((white :* VNil) :^
                                                             (white :* VNil) :^
                                                             MNil)
      it "with 2x2 frame, identity function, no zoom, and an orbit radius of 1 we get 4 pixels represeting the 4 quarters of the frame, all of which are outside set colour" $ do
        -- We have outside set colour for all 4 as their centres land on -1 - 1i, -1 + 1i, 1 - 1i and 1 + 1i which lie outside the orbit radius.
        let compInfo = ComputationInfo const (1 :: Double) (0 :+ 0) 10
        let white = sRGB24 255 255 255
        let palette  = Palette [white] black 
        let zoom = Zoom (0 :+ 0) (1 :+ 0)
        plot two two compInfo palette zoom `shouldBe` Right ((white :* white :* VNil) :^ 
                                                             (white :* white :* VNil) :^
                                                             MNil)
      it "with 2x4 frame, identity function, no zoom, and an orbit radius of 1 we get 8 pixels, all of which are outside set colour" $ do
        let compInfo = ComputationInfo const (1 :: Double) (0 :+ 0) 10
        let white = sRGB24 255 255 255
        let palette  = Palette [white] black 
        let zoom = Zoom (0 :+ 0) (1 :+ 0)
        plot two four compInfo palette zoom `shouldBe` Right ((white :* white :* white :* white :* VNil) :^
                                                              (white :* white :* white :* white :* VNil) :^
                                                              MNil)
      it "with 4x8 frame, identity function, no zoom, and an orbit radius of 1 we get 32 pixels, centre 8 of which are in set colour" $ do
        let compInfo = ComputationInfo const (1 :: Double) (0 :+ 0) 10
        let white = sRGB24 255 255 255
        let palette  = Palette [white] black 
        let zoom = Zoom (0 :+ 0) (1 :+ 0)
        plot four eight compInfo palette zoom `shouldBe` Right ((white :* white :* white :* white :* white :* white :* white :* white :* VNil) :^
                                                                (white :* white :* black :* black :* black :* black :* white :* white :* VNil) :^
                                                                (white :* white :* black :* black :* black :* black :* white :* white :* VNil) :^
                                                                (white :* white :* white :* white :* white :* white :* white :* white :* VNil) :^
                                                                MNil)
         
