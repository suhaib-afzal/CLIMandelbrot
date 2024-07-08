{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

module Lib ( ComputationInfo(ComputationInfo)
           , Palette(Palette)
           , Zoom(Zoom)
           , Frame
           , mandelbrotFrame
           , plot
           , defaultPalette 
           , csp ) where

import Datatypes ( Nat(Nil, Succ)
                 , SNat(SNil, SSucc) 
                 , Cmplx
                 , Vector(VNil, (:*))
                 , Matrix(MNil, (:^))
                 , build
                 , buildMat )

import Data.Kind ( Type )
import Data.Complex ( Complex ( (:+) )
                    , magnitude )
import Data.Colour ( Colour )
import Data.Colour.SRGB ( sRGB24 )


-- PUBLIC DATA TYPES ----------------------------------------------------------

data ComputationInfo =
 ComputationInfo { func :: Cmplx -> Cmplx -> Cmplx
                 , orbitRad :: Double
                 , initVal :: Cmplx
                 , maxIterations :: Int }

data Palette = Palette [Colour Float] (Colour Float) deriving Show
data Zoom =
   Zoom { origin :: Cmplx
        , factor :: Cmplx }
   deriving Show

type Frame m n = Matrix m n (Colour Float)

-- Private
type CoordGrid m n = Matrix m n (Cmplx)
data ComputationResult = Diverges Double | NotDiverge

-- PUBLIC FUNCTIONS -------------------------------------------------------------------

mandelbrotFrame ::  SNat m -> SNat n ->                    
                    Either String (Frame m n)
mandelbrotFrame m n =
  let info = ComputationInfo mandelbrotFunc 2 0 1000
      zoom = Zoom (0 :+ 0) (1 :+ 0)
  in plot m n info defaultPalette zoom


plot :: SNat m -> SNat n ->
        ComputationInfo ->
        Palette ->
        Zoom ->
        Either String (Frame m n)
plot m n info palette zoom = do
  (vM, vN) <- validateHeightWidth m n
  vInfo    <- validateCompInfo info
  vPalette <- validatePalette palette
  vZoom    <- validateZoom zoom
  Right $ innerPlot vM vN vInfo vPalette vZoom
  where 
  innerPlot :: SNat m -> SNat n ->
             ComputationInfo ->
             Palette ->
             Zoom ->
             Frame m n
  innerPlot m n info palette zoom = fmap (pickColour palette . doComputation info)
                                    . applyZoom zoom
                                    $ standardGrid m n


defaultPalette :: Palette
defaultPalette = Palette [sRGB24 255 179 179,
                          sRGB24 255 102 102,
                          sRGB24 204 0 0,
                          sRGB24 153 0 0,
                          sRGB24 122 0 0,
                          sRGB24 92 0 0]
                         (sRGB24 0 0 0)

-- Continuation Passing Style Function
--     m      n
csp :: Nat -> Nat -> (forall m n. SNat m -> SNat n -> r) -> r
csp Nil      Nil      k = k SNil SNil
csp (Succ m) Nil      k = csp m   Nil (k . SSucc)
csp Nil      (Succ n) k = csp Nil n   (flip (flip k . SSucc))
csp (Succ m) (Succ n) k = csp m   n   (flip (flip (k . SSucc) . SSucc))



-- PRIVATE FUNCTIONS -------------------------------------------------------------------------------

-- Validation
validateHeightWidth :: SNat m -> SNat n -> Either String (SNat m, SNat n)
validateHeightWidth SNil SNil = Left "The height width values are 0"
validateHeightWidth _    SNil = Left "The width value is 0"
validateHeightWidth SNil _    = Left "The height value is 0"
validateHeightWidth m    n    = Right (m,n)

validateCompInfo :: ComputationInfo -> Either String ComputationInfo
validateCompInfo = Right 

validatePalette :: Palette -> Either String Palette
validatePalette
  palette@(Palette list _) = if null list
                             then Left "The palette is incomplete. We need at least one colour to denote points that dont lie in the set"
                             else Right palette

validateZoom :: Zoom -> Either String Zoom
validateZoom zoom = if factor zoom == (0 :+ 0)
                    then Left "The factor given for the zoom is 0 + 0i, this is an invalid zoom factor"
                    else Right zoom

-- Implementation
standardGrid :: SNat m -> SNat n -> CoordGrid m n
standardGrid height width =
  let wincrement = getIncrement width 4
      hincrement = getIncrement height 4
      initVec = (:+ (2 - hincrement/2)) <$> build width (:*) (+ wincrement) (-2 + wincrement/2) VNil
  in buildMat height width (:^) (fmap (\x -> x - (0 :+ hincrement))) initVec MNil
  

getIncrement :: SNat n -> Double -> Double
getIncrement n len = len/(fromIntegral . fromEnum $ n)

applyZoom :: Zoom -> CoordGrid m n -> CoordGrid m n
applyZoom zoom = fmap ((*) (factor zoom) . (+) (origin zoom))

doComputation :: ComputationInfo -> Cmplx -> ComputationResult
doComputation info coord =
  let iterationResult = takeWhile (\x -> magnitude x < orbitRad info )
                        . take (maxIterations info)
                        . iterate (func info coord) $ initVal info
  in
  if length iterationResult == maxIterations info
  then NotDiverge
  else Diverges $ fromIntegral (length iterationResult) / fromIntegral (maxIterations info)
  
pickColour :: Palette -> ComputationResult -> Colour Float
pickColour (Palette _   sing) NotDiverge        = sing
pickColour (Palette lis _   ) (Diverges percen) = lis !! index
  where index = round $ percen * fromIntegral (length lis - 1)

mandelbrotFunc :: Cmplx -> Cmplx ->
                  Cmplx
mandelbrotFunc c z = z*z + c
