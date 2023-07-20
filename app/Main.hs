{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

module Main ( main ) where

import Data.Foldable ( traverse_ )
import System.Console.ANSI
import Data.Colour ( black , Colour )
import Data.Colour.SRGB ( sRGB24 )
import Data.Complex ( Complex ((:+)) )
import Control.Monad ( replicateM_ )
import Text.Read ( readMaybe )
import System.Environment
import Lib -- ( Frame
import Parsing ( parseCompInfo )
           -- , mandelbrotFrame
           -- , singleColourFrame
           -- , traverseMat_
           -- , plotAndDisplay)


-- MAIN
main :: IO ()
main = do
  let defaultCompInfo = ComputationInfo const (1 :: Double) (0 :+ 0) 10
  args <- getArgs
  case args of
    [] -> displayError "No arguments were passed to the function"
    ("help":_) -> displayText helpText
    ("mandelbrot":rest) -> case parseMandelbrotArgs rest of
      Left str -> displayError str
      Right (height,width) -> csp2 height width (\x y -> case mandelbrotFrame x y of
                                                    Left str -> displayError str
                                                    Right frame -> displayFrame frame)
    ("plot":rest) -> case parsePlotArgs rest of
      Left str -> displayError str
      Right (height,width,palette,zoom,compInfo) -> csp2 height width (\x y -> case plot x y compInfo palette zoom of
                                                                 Left str -> displayError str
                                                                 Right frame -> displayFrame frame)
    _ -> displayError "Command supplied is not recognised, please start your set of arguments with a defined command"
    

-- INPUT
parseMandelbrotArgs :: [String] -> Either String (Nat,Nat)
parseMandelbrotArgs []    = Left "No arguments passed to mandelbrot command. Please pass two Natural numbers to the command"
parseMandelbrotArgs [_]   = Left "Only one argument passed to mandelbrot command. Please pass two Natural numbers to the command"
parseMandelbrotArgs [x,y] = do
  m <- parseNatural x
  n <- parseNatural y
  Right (m,n)
parseMandelbrotArgs (_:_:_)  = Left "More than two arguments passed to mandelbrot command. Please pass two Natural numbers to the command"

parsePlotArgs :: [String] -> Either String (Nat,Nat,Palette,Zoom,ComputationInfo)
parsePlotArgs [m',n',p',z',c'] = do
  m <- parseNatural  m'
  n <- parseNatural  n'
  p <- parsePalette  p'
  z <- parseZoom     z'
  c <- parseCompInfo c'
  Right (m,n,p,z,c)
parsePlotArgs _ = Left "Please pass exactly 4 arguments to the command, you may need to wrap individual arguments in \"\" " 

parseNatural :: String -> Either String Nat
parseNatural s = do
  int <- ourReadEither s
  nat <- toNat int
  Right nat

ourReadEither :: String -> Either String Int
ourReadEither s = case readMaybe s of
  Nothing  -> Left "Cannot parse a passed in value as a Number"
  Just int -> Right int

parsePalette :: String -> Either String Palette
parsePalette str = undefined-- do
  -- blocks <- splitIntoBlocks str
  -- if length blocks /= 2 then Left "We cannot parse the Palette input into exactly two parts, please try again"

parseZoom :: String -> Either String Zoom
parseZoom = undefined

-- data ComputationInfo =
--  ComputationInfo { func :: Complex Double -> Complex Double -> Complex Double
--                  , orbitRad :: Double
--                  , initVal :: Complex Double
--                  , maxIterations :: Int }

  

splitIntoBlocks :: String -> Either String [String]
splitIntoBlocks = undefined


helpText :: [String]
helpText = ["help yoselfkk brudda"]

-- OUTPUT
displayText :: [String] -> IO ()
displayText = mapM_ putStrLn

displayError :: String -> IO ()
displayError str = do
  putStrLn "Error"
  putStrLn $ "Message: " ++ str

displayFrame :: Frame m n -> IO ()
displayFrame = traverseMat_ displayPixel lineBreak

displayPixel :: Colour Float -> IO ()
displayPixel colur = do
  setSGR [SetRGBColor Background colur]
  putStr " "

lineBreak :: IO ()
lineBreak = do
  setSGR [Reset]
  putStrLn ""


-- TESTING
outputTest :: IO ()
outputTest = do
  let rgbColour = (sRGB24 255 255 255 :: Colour Float)
  replicateM_ 16 (oneLineTest 32 rgbColour)

oneLineTest :: Int -> Colour Float -> IO ()
oneLineTest n col = do
  replicateM_ n (displayPixel col)
  lineBreak
