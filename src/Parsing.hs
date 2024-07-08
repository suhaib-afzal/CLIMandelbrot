module Parsing ( parseNatural
               , parsePalette
               , parseZoom
               , parseCompInfo )
                where

-- Expr ::= Expr - Expr | Add
-- Add  ::= Add  + Add  | Mul
-- Mul  ::= Mul  * Mul  | Div
-- Div  ::= Div  \ Div  | Ind
-- Ind  ::= Ind  ^ Ind  | Term
-- Term ::= (Expr)      | Cmpx

import Debug.Trace
import Data.Complex
import Control.Monad
import Text.Read ( readMaybe )
import Data.Char
import Data.Maybe
import Data.Colour
import Data.Tuple
import Data.Colour.SRGB ( sRGB24 )
import Lib
import Datatypes ( toNat
                 , Cmplx
                 , Nat(Nil, Succ) )

data ParseTree  = LeafP String | NodeP ParseTree String ParseTree deriving Show
data ExecTree a = LeafE a      | NodeE (ExecTree a) (a -> a -> a) (ExecTree a)
newtype Parser  = Parser  ( String -> Maybe ParseTree )

data Direction  = TakeLeft | TakeRight deriving Show
type Path       = [Direction]

--PUBLIC
-------------------------------------------------------------------------------------------

parseNatural :: String -> Either String Nat
parseNatural s = do
  int <- ourReadEither s
  nat <- toNat int
  Right nat

parsePalette :: String -> Either String Palette
parsePalette str
  | null str                                      = Left "Palette input is empty"
  | all (== 0) bracketing                         = Left "Please ensure you bracket the palette list"
  | last bracketing /= 0 || any (< 0) bracketing  = Left "Please ensure your bracketing is balanced"
  | sing listStrC || null valStr                  = Left "Please ensure you include values for the list and single rgb"
  | listStr == "()"                               = Left "Empty brackets passed in for list"
  | any null innerSplit                           = Left "Please ensure there are no empty terms in the Palette List"
  | any (all isSpace) innerSplit                  = Left "Please ensure there are no empty terms in the Palette List"
  | otherwise                                     = Palette <$> traverse parseRGB rmHeadSplit <*> parseRGB (pruneHead valStr)
    where
      rmHeadSplit = map pruneHead innerSplit
      innerSplit = splitAtAll ',' innerList
      innerList = init . tail $ listStr
      listStr = init listStrC
      (listStrC,valStr) = breakRev (== ',') str 
      bracketing = bracketDepth str

parseZoom :: String -> Either String Zoom
parseZoom inStr
  | null frst || sing tempSecnd = Left "Please ensure you specify two complex numbers for your Zoom"
  | otherwise                   = case (cmplxRead frst, cmplxRead secnd) of
                                   (Nothing,Nothing) -> Left "Could not parse either value passed into Zoom"
                                   (Nothing, _     ) -> Left "Could not parse the first value passed into the Zoom"
                                   (_      ,Nothing) -> Left "Could not parse the second value passed into the Zoom"
                                   (Just x ,Just y ) -> Right $ Zoom x y
    where
      secnd = tail tempSecnd
      (frst, tempSecnd) = break (== ',') str
      str = filter (not . isSpace) inStr

parseCompInfo :: String -> Either String ComputationInfo
parseCompInfo str = case splitAtAll ',' str of
  [f,orb,iv,mI] -> do
    func  <- parseFunc f
    orbit <- ourReadEither orb
    initV <- maybe (Left "Failed to parse the initial value passed to ComputationInfo") Right (cmplxRead iv)
    maxI  <- ourReadEither mI
    Right $ ComputationInfo func orbit initV maxI
  _            -> Left "Pass in exactly 4 values to specify your computation info"



--PARSING
------------------------------------------------------------------------------------------------------

parseRGB :: String -> Either String (Colour Float)
parseRGB str = case nums of
      Nothing      -> Left "Cannot parse an integer number passed in as part of the palette"
      Just [x',y',z'] -> do
        x <- validate x'
        y <- validate y'
        z <- validate z'
        Right (sRGB24 x y z)
      Just _       -> Left "Please ensure each element in the Palette is 3 numbers"      
  where
   nums = traverse readMaybe split
   split = splitAtAll ' ' str
   validate n = if n >= 0 && n <= 255 then Right n else Left "Please ensure each value in the Palette is >= 0 and <= 255"

splitAtAll :: Char -> String -> [String]
splitAtAll _ []  = []
splitAtAll c str = fst split : splitAtAll c (hackTail . snd $ split)
  where split = break (== c) str

hackTail :: [a] -> [a]
hackTail [] = []
hackTail l  = tail l

pruneHead :: String -> String
pruneHead x = if isSpace . head $ x then tail x else x

parseFunc :: String -> Either String (Cmplx -> Cmplx -> Cmplx)
parseFunc str = do
  parseTree <- readIntoPTree str
  case parseToExec parseTree 0 0 of
        Left e  -> Left e
        Right _ -> Right (\c z -> execETree . fromRight' . parseToExec parseTree c $ z)


readIntoPTree :: String -> Either String ParseTree
readIntoPTree str = let maybPT = recurseParse expr . filter (not . isSpace) $ str
                    in case maybPT of
                         Nothing -> Left "Cannot parse function, please read documentation for guidance"
                         Just pt -> Right pt

recurseParse :: Parser -> String -> Maybe ParseTree
recurseParse p str = do
  tree <- applyParse p str
  if isLeaf tree
  then do
    Just tree
  else do
    leftval     <- tree !!! [TakeLeft]
    rightval    <- tree !!! [TakeRight]
    leftp       <- recurseParse p leftval
    rightp      <- recurseParse p rightval
    lefAdded    <- add tree [TakeLeft] leftp
    bthAdded    <- add lefAdded [TakeRight] rightp
    Just bthAdded

applyParse :: Parser -> String -> Maybe ParseTree
applyParse (Parser p) = p

isLeaf :: ParseTree -> Bool
isLeaf (LeafP _)     = True
isLeaf (NodeP _ _ _) = False

(!!!) :: ParseTree -> Path -> Maybe String
(!!!) pt path = get pt path root

get :: ParseTree -> Path -> (ParseTree -> a) ->  Maybe a
get pt            []               f  = Just (f pt)
get (LeafP _)     (_:_)            _  = Nothing
get (NodeP l _ _) (TakeLeft:path)  f  = get l path f
get (NodeP _ _ r) (TakeRight:path) f  = get r path f

root :: ParseTree -> String
root (LeafP s)     = s
root (NodeP _ x _) = x

add :: ParseTree -> Path -> ParseTree -> Maybe ParseTree
add _              []              qt = Just qt
add (LeafP _)     (_:_)            _  = Nothing
add (NodeP l x r) (TakeLeft:path)  qt = do
  leftAdded <- add l path qt
  Just (NodeP leftAdded x r)
add (NodeP l x r) (TakeRight:path) qt = do
  rightAdded <- add r path qt
  Just (NodeP l x rightAdded)

expr :: Parser
expr = anyExpr '-' ~> anyExpr '+' ~> anyExpr '*' ~> anyExpr '/' ~> anyExpr '^'

anyExpr :: Char -> Parser 
anyExpr c = Parser (innerAnyE c)

innerAnyE :: Char -> String -> Maybe ParseTree
innerAnyE c str
  | null str                                     = Nothing
  | literal str                                  = Just $ LeafP str
  | last bracketing /= 0 || any (< 0) bracketing = Nothing
  | all (>0) (init bracketing)                   = innerAnyE c (init . tail $ str)
  | isNothing mayRightInd                        = Nothing
  | null lst || sing rst                         = Nothing
  | otherwise                                    = Just $ NodeP (LeafP lst) [c] (LeafP (tail rst))
  where
    bracketing  = bracketDepth str
    cLocs       = locate c str
    validLocs   = zipWith (&&) (map (not . intToBool) bracketing) cLocs
    mayRightInd = rightmost validLocs
    rightInd    = fromMaybe (-1) mayRightInd
    (lst,rst)   = splitAt rightInd str 

literal :: String -> Bool
literal str
  | null str  = False
  | sing str  = True
  | otherwise = numLiteral 1 str
  where
    numLiteral _ []     = True
    numLiteral n (x:xs)
      | null (x:xs) = True
      | n < 0       = False
      | x == '.'    = numLiteral (n-1) xs
      | isDigit x   = numLiteral n xs
      | otherwise   = False

bracketDepth :: String -> [Int]
bracketDepth = bracDepthHelp 0

bracDepthHelp :: Int -> String -> [Int]
bracDepthHelp _ []        = []
bracDepthHelp x ('(':str) = (x+1) : bracDepthHelp (x+1) str
bracDepthHelp x (')':str) = (x-1) : bracDepthHelp (x-1) str
bracDepthHelp x ( _ :str) = x     : bracDepthHelp  x str

locate :: (Eq a) => a -> [a] -> [Bool]
locate x = map (== x)

intToBool :: Int -> Bool
intToBool x
  | x > 0     = True
  | otherwise = False

rightmost :: [Bool] -> Maybe Int
rightmost lis = rightmostHelp (length lis - 1) lis

rightmostHelp :: Int -> [Bool] -> Maybe Int
rightmostHelp _ []  = Nothing
rightmostHelp x lis = if last lis then Just x else rightmostHelp (x-1) (init lis)

sing :: [a] -> Bool
sing []     = True
sing (_:xs) = null xs
                   
                                     
infixr ~>
(~>) :: Parser -> Parser -> Parser
(~>) try catch = Parser (\inp -> let tryRes = applyParse try inp
                                       in case tryRes of
                                   Nothing -> applyParse catch inp
                                   Just _  -> tryRes)

fromRight' :: Either a b -> b
fromRight' (Right x) = x
fromRight' (Left _)  = error "panic! 0 0 was succesfully bound to c z but the general function failed"

breakRev :: (a -> Bool) -> [a] -> ([a],[a])
breakRev predi lis = swap $ hackMap reverse $ break predi (reverse lis)

hackMap :: (a -> b) -> (a,a) -> (b,b)
hackMap f (a1,a2) = (f a1, f a2)

--EXECUTION
--------------------------------------------------------------------------------

parseToExec :: ParseTree -> Cmplx -> Cmplx -> Either String (ExecTree Cmplx)
parseToExec (LeafP vs) c z = case vs of
  "z" -> Right $ LeafE z
  "c" -> Right $ LeafE c
  "i" -> Right $ LeafE (0:+1)
  _   -> case cmplxRead vs of
    Nothing -> Left "Cannot parse a number in the function"
    Just v  -> Right $ LeafE v
parseToExec (NodeP treeL str treeR) c z = do
    operation <- opCases str
    eTreeL <- parseToExec treeL c z
    eTreeR <- parseToExec treeR c z
    Right $ NodeE eTreeL operation eTreeR

cmplxRead :: String -> Maybe Cmplx
cmplxRead = readMaybe . cmplxReadHelper
  where
  cmplxReadHelper str1
    | '+' `existsIn` str1 && 'i' `existsIn` str1 = replace '+' ":+" . remove 'i' $ str1
    | 'i' `existsIn` str1                        = (++) "0:+" . remove 'i' $ str1
    | otherwise                                  = str1 ++ ":+0"
  
existsIn :: (Eq a) => a -> [a] -> Bool
existsIn a = or . locate a

remove :: (Eq a) => a -> [a] -> [a]
remove x = filter (/=x)

replace :: (Eq a) => a -> [a] -> [a] -> [a]
replace _ _   []       = [] 
replace x rep (s:strs) = if s == x then rep ++ replace x rep strs else s : replace x rep strs
                

opCases :: (Floating a) => String -> Either String (a -> a -> a)
opCases "+" = Right (+)
opCases "-" = Right (-)
opCases "*" = Right (*)
opCases "^" = Right (**)
opCases "/" = Right (/)
opCases _   = Left "Unrecognised operation"
  
execETree :: (Floating a) => ExecTree a -> a
execETree (LeafE val) = val
execETree (NodeE treeL op treeR) =
  let leftResult = execETree treeL
      rightResult = execETree treeR
  in op leftResult rightResult

ourReadEither :: (Read a) => String -> Either String a
ourReadEither s = case readMaybe s of
  Nothing  -> Left "Cannot parse a passed in value"
  Just int -> Right int


--TESTING ONLY
------------------------------------------------------------------------------------------

fromRight'' :: Either a b -> b
fromRight'' (Right x) = x
fromRight'' (Left _)  = error "cannot"
