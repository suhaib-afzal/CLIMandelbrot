module Parsing where

-- Expr ::= Expr - Expr | Add
-- Add  ::= Add  + Add  | Mul
-- Mul  ::= Mul  * Mul  | Div
-- Div  ::= Div  \ Div  | Ind
-- Ind  ::= Ind  ^ Ind  | Term
-- Term ::= (Expr)      | Cmpx

import Data.Complex
import Text.Read ( readMaybe )
import Lib

data ParseTree  = LeafP String | NodeP ParseTree String ParseTree deriving Show
data ExecTree a = LeafE a      | NodeE (ExecTree a) (a -> a -> a) (ExecTree a)
newtype Parser  = Parser ( String -> Maybe (ParseTree ,[Path]) )

data Direction  = TakeLeft | TakeRight
type Path       = [Direction]

slice pt path = get pt path id

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

applyParse :: Parser -> String -> Maybe (ParseTree, [Path])
applyParse (Parser p) = p

-- exact :: String -> Parser
-- exact s = Parser (\inp -> if inp == s then Just(LeafP

node :: String -> String -> String -> ParseTree
node l c r = NodeP (LeafP l) c (LeafP r)

char :: Char -> Parser
char c = Parser (\inp -> do
                   (lefStr,rigStr) <- breakRev (== c) inp
                   Just (node lefStr [c] rigStr, [[TakeLeft],[TakeRight]])
                ) 

breakRev :: (a -> Bool) -> [a] -> Maybe ([a],[a])
breakRev pred lis = helpFlip $ break pred (reverse lis)
  where
  helpFlip (lA,lB) = do
    iniLB <- safeInit . reverse $ lB
    Just (iniLB, reverse lA)
                   
safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit l  = Just . init $ l             
 -- :: ([a],[b]) -> ([b],[a])
-- flip (lisA,lisB) = (reverse lisB, reverse lisA)
                                  
nodeCombine :: Parser -> Parser -> Parser -> Parser
nodeCombine left centre right = undefined --Parser (\inp -> )

failCombine :: Parser -> Parser -> Parser
failCombine try catch = Parser (\inp ->let tryRes = applyParse try inp
                                       in case tryRes of
                                   Nothing -> applyParse catch inp
                                   Just _  -> tryRes)

--leafCombine :: Parser 

andThen :: Parser -> Parser -> Parser
andThen p q = Parser ( \inp -> case applyParse p inp of
                  Nothing -> Nothing
                  Just (tr,pths) -> dominoes (tr,[]) $ map (continue q) pths) 

dominoes :: (Monad m) => a -> [a -> m a] -> m a
dominoes ini [] = pure ini
dominoes ini (ft:rest) = do
  val <- ft ini
  dominoes val rest

continue :: Parser -> Path -> (ParseTree, [Path]) -> Maybe (ParseTree, [Path])
continue q pth (pt, pths)= do
  str <- pt !!! pth
  (qt, qpths) <- applyParse q str
  Just (qt, pths ++ qpths)

-- rearrange :: (a,[b]) -> [(a,b)]
-- rearrange (a, bs) = map (a,) bs
                    
--zip (map (q . tr !!!) pths) pths
                    
--foldl (\(orgPt,orgPths) pth -> do (accPt, accPths) <- q (orgPt !!! pth) (add orgPt pth accPt, orgPths ++ map (pth ++) accPths)) (tr,[]) pths)                    

parseNatural :: String -> Either String Nat
parseNatural s = do
  int <- ourReadEither s
  nat <- toNat int
  Right nat

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

parseCompInfo :: String -> Either String ComputationInfo
parseCompInfo str = case splitAtAll ',' str of
  [f,orb,iv,mI] -> do
    func  <- parseFunc f
    orbit <- ourReadEither orb
    initV <- ourReadEither iv
    maxI  <- ourReadEither mI
    Right $ ComputationInfo func orbit initV maxI
  _            -> Left "Pass in exactly 4 values to specify your computation info"

splitAtAll :: Char -> String -> [String]
splitAtAll = undefined

parseFunc :: (Floating a, Read a) =>  String -> Either String (a -> a -> a)
parseFunc str = do
  parseTree <- readIntoPTree str
  case parseToExec parseTree 0 0 of
        Left e  -> Left e
        Right _ -> Right (\c z -> execETree . fromRight' . parseToExec parseTree c $ z)

readIntoPTree :: String -> Either String ParseTree
readIntoPTree = undefined

parseToExec :: (Read a, Floating a) => ParseTree -> a -> a -> Either String (ExecTree a)
parseToExec (LeafP vs) c z = case vs of
  "z" -> Right $ LeafE z
  "c" -> Right $ LeafE c
  _   -> case readMaybe vs of
    Nothing -> Left "Cannot parse a value written in the function"
    Just v  -> Right $ LeafE v
parseToExec (NodeP treeL str treeR) c z = do
    operation <- opCases str
    eTreeL <- parseToExec treeL c z
    eTreeR <- parseToExec treeR c z
    Right $ NodeE eTreeL operation eTreeR

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

fromRight' :: Either a b -> b
fromRight' (Right x) = x
fromRight' (Left _)  = error "panic! 0 0 was succesfully bound to c z but the general function failed"

-- Should be 8 
examplePT :: ParseTree
examplePT = NodeP (NodeP (LeafP "10") "/" (LeafP "10")) "*" (LeafP "8")

-- Should be 9
examplePT2 :: ParseTree
examplePT2 = NodeP (NodeP (LeafP "6") "-" (LeafP "9")) "^" (NodeP (LeafP "3") "-" (NodeP (LeafP "6") "^" (LeafP "0")))
