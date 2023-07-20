{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

module Lib where

-- module Lib
--     ( plot
--     , Frame
--     , mandelbrotFrame
--     , singleColourFrame
--     , traverseMat_
--     ) where

-- import GHC.TypeLits ( Nat
--                     , KnownNat
--                     , SomeNat )
import Data.Kind ( Type )
import Data.Complex ( Complex ( (:+) )
                    , magnitude)
import Data.Colour ( Colour )
import Data.Colour.SRGB ( sRGB24 )


--DATA TYPES AND CLASS INSTANCES

data Nat = Nil | Succ Nat
  deriving (Eq,Show)

data SNat :: Nat -> Type where
  SNil  :: SNat Nil
  SSucc :: SNat n -> SNat (Succ n)
  
zero  = SNil
one   = SSucc zero
two   = SSucc one
four  = SSucc (SSucc two)
eight = SSucc (SSucc (SSucc (SSucc four)))

-- instance Ord Nat where
--   (<=) Nil      _        = True
--   (<=) _        Nil      = False
--   (<=) (Succ n) (Succ m) = (<=) n m

-- instance Ord (SNat n) where
--   (<=) SNil      _         = True
--   (<=) _         SNil      = False
--   (<=) (SSucc n) (SSucc m) = (<=) n m

-- instance Enum Nat where
--   toEnum = undefined
--   fromEnum = undefined

toNat :: Int -> Either String Nat
toNat 0 = Right Nil
toNat n
  | n < 0     = Left "Number passed in less than 0 and so is not natural"
  | n > 0     = Succ <$> toNat (n - 1)
  | otherwise = Left "An unexpected error has occured when parsing an input natural, please try again" 

nsfeToNat :: Int -> Nat
nsfeToNat 0 = Nil
nsfeToNat n = Succ $ nsfeToNat (n - 1)

instance Enum (SNat n) where
  fromEnum SNil       = 0
  fromEnum (SSucc sn) = 1 + fromEnum sn   


--data SomeNat = forall n. 

-- sSing :: Nat -> SNat n
-- sSing Nil      = SNil  
-- sSing (Succ n) = SSucc $ sSing n

--     m      n
csp :: Nat -> Nat -> (forall m. SNat m -> r1 -> r2) -> (forall n. SNat n -> r1)  -> r2
csp Nil      Nil      l k = l SNil $ k SNil
csp Nil      (Succ n) l k = csp Nil n    l         (k . SSucc)
csp (Succ m) Nil      l k = csp m   Nil (l . SSucc) k
csp (Succ m) (Succ n) l k = csp m   n   (l . SSucc)(k . SSucc)

csp2 :: Nat -> Nat -> (forall m n. SNat m -> SNat n -> r) -> r
csp2 Nil      Nil      k = k SNil SNil
csp2 (Succ m) Nil      k = csp2 m   Nil (k . SSucc)
csp2 Nil      (Succ n) k = csp2 Nil n   (flip (flip k . SSucc))
csp2 (Succ m) (Succ n) k = csp2 m   n   (flip (flip (k . SSucc) . SSucc))

-- csp (Nil,   Nil)    k l = (k SNil, l SNil)
-- csp (Nil,   Succ m) k l = csp (Nil,m)  k         (l . SSucc)
-- csp (Succ n,Nil)    k l = csp (n,Nil) (k . SSucc) l 
-- csp (Succ n,Succ m) k l = csp (n,m)   (k . SSucc)(l . SSucc)

-- sSing2 :: Nat -> (SNat n -> a) -> a
-- sSing2 Nil k = k SNil
-- sSing2 (Succ n) k = sSing n (k . SSucc)

data ComputationResult = Diverges Double | NotDiverge

data ComputationInfo =
 ComputationInfo { func :: Complex Double -> Complex Double -> Complex Double
                 , orbitRad :: Double
                 , initVal :: Complex Double
                 , maxIterations :: Int }

type Frame m n = Matrix m n (Colour Float)
type CoordGrid m n = Matrix m n (Complex Double)

data SomeFrame = SomeFrame (forall m n. Frame m n)

data Palette = Palette [Colour Float] (Colour Float)
data Zoom =
   Zoom { origin :: Complex Double
        , factor :: Complex Double }

--MATRIX :: Height -> Width -> Type -> Type
data Matrix :: Nat -> Nat -> Type -> Type where
  MNil   :: Matrix Nil n a
  (:^)   :: Vector n a -> Matrix m n a -> Matrix (Succ m) n a

infixr :^

instance Functor (Matrix m n) where
  fmap :: (a -> b) -> Matrix m n a -> Matrix m n b
  fmap _ MNil = MNil
  fmap f (vec :^ mat) = fmap f vec :^ fmap f mat


instance(Show a) => Show (Matrix m n a) where
  show MNil    = "MNil"
  show (v:^vs) = "(" ++ show v ++ ":^" ++ show vs ++ ")"

instance (Eq a) => Eq (Matrix m n a) where
  (==) MNil MNil = True
  (==) MNil _    = False
  (==) _    MNil = False
  (==) (v:^vs) (a:^as) = (v == a) && (vs == as)
  

-- [a,b,c] [1,2,3]
--   1  2  3
-- c 1c 2c 3cj
-- b 1b 2b 3b
-- a 1a 2a 3a
-- productSpace :: (a -> b -> c) -> Vector m a -> Vector n b -> Matrix m n c
-- productSpace f vecH = prodSpcHelper f (reverseV vecH)
--   where
--   prodSpcHelper :: (a -> b -> c) -> Vector m a -> Vector n b -> Matrix m n c
--   prodSpcHelper _  VNil   _    = MNil
--   prodSpcHelper g (a:*as) lisW = fmap (g a) lisW :^ prodSpcHelper g as lisW


build :: SNat k ->
         (forall m. a -> t m a -> t (Succ m) a) ->
         (a -> a) ->
         a ->
         t Nil a ->
         t k a
build SNil       _ _ _   las = las
build (SSucc sn) b f ini las = b ini (build sn b f (f ini) las)


buildMat :: SNat m -> SNat n ->
            (forall k q. vec q a -> mat k q a -> mat (Succ k) q a) ->
            (forall q. vec q a -> vec q a) ->
            vec n a ->
            mat Nil n a ->
            mat m n a
buildMat SNil       _  _ _ _   las = las
buildMat (SSucc sm) sn b f ini las = b ini (buildMat sm sn b f (f ini) las)


-- buildhelper :: Int -> (a -> a) -> t a -> t a -> t a
-- buildhelper n f init last = 
-- instance Applicative (Matrix m n) where
--   pure  = undefined
--   (<*>) = undefined

-- instance Foldable (Matrix m n) where
--   foldr = undefined

-- Allows an action to be executed at the end of each vector
data SomeMat a = SomeMat (forall m n. Matrix m n a)

traverseMat_ :: (a -> IO b) -> IO b -> Matrix m n a -> IO ()
traverseMat_ _  _   MNil    = pure ()
traverseMat_ io end (v:^vs) = traverseVec_ io v >> end >> traverseMat_ io end vs

-- traverseMat_ :: (a -> IO b) -> IO b -> SomeMat a -> IO ()
-- traverseMat_ _  _   (SomeMat MNil)    = pure ()
-- traverseMat_ io end (SomeMat (v:^vs)) = traverseVec_ io v >> end >> traverseMat_ io end (SomeMat vs)

singleton :: a -> Matrix (Succ Nil) (Succ Nil) a
singleton x = (x :* VNil) :^ MNil

--VECTOR
data Vector :: Nat -> Type -> Type where
  VNil  :: Vector Nil a
  (:*) :: a -> Vector n a -> Vector (Succ n) a

infixr :*

data SomeVec a = forall n. SomeVec (Vector n a)

instance (Show a) => Show (Vector n a) where
  show VNil     = "VNil"
  show (a :* v) = "(" ++ show a ++ " :* " ++ show v ++ ")"

instance (Eq a) => Eq (Vector n a) where
  (==) VNil VNil = True
  (==) _    VNil = False
  (==) VNil _    = False
  (==) (x:*xs) (y:*ys) = (x == y) && (xs == ys)

-- unSomeVec :: SomeVec a -> Vector n a
-- unSomeVec (SomeVec vec) = vec

instance Functor (Vector n) where
  fmap :: (a -> b) -> Vector n a -> Vector n b
  fmap _ VNil = VNil
  fmap f (val :* vec) = f val :* fmap f vec

-- vmap :: (a -> b) -> Vector n a -> Vector n b
-- vmap _ VNil = VNil
-- vmap f (val :* vec) = f val :* fmap f vec


listToVec :: [a] -> SomeVec a
listToVec []     = SomeVec VNil
listToVec (x:xs) = case listToVec xs of
  SomeVec xs' -> SomeVec (x :* xs')

-- reverseV :: Vector n a -> Vector n a
-- reverseV = undefined

traverseVec_ :: (a -> IO b) -> Vector n a -> IO ()
traverseVec_ _  VNil    = pure ()
traverseVec_ io (a:*as) = io a >> traverseVec_ io as

-- instance (KnownNat n) => Applicative (Vector n) where
--   pure a = undefined--VCons a VNil
--   (<*>) = undefined

-- instance Foldable (Vector n) where
--   foldr = undefined

--PUBLIC FUNCTIONS
mandelbrotFrame ::  SNat m -> SNat n ->                    
                    Either String (Frame m n)
mandelbrotFrame m n =
  let info = ComputationInfo mandelbrotFunc 2 0 1000
      zoom = Zoom (0 :+ 0) (1 :+ 0)
  in plot m n info defaultPalette zoom

singleColourFrame :: Nat -> Nat -> Colour Float ->
                     Either String (Frame m n)
singleColourFrame = undefined

-- plotAndDisplay :: Nat -> Nat ->
--            (forall m n. SNat m -> SNat n -> Either String (Frame m n)) ->
--            (forall m n. Either String (Frame m n) -> IO ()) ->
--            IO ()
-- plotAndDisplay m n frame disp =
--   csp2 m n (\x y -> disp $ frame) 


-- plotAndDisplay :: Nat -> Nat ->
--                   ComputationInfo ->
--                   Palette ->
--                   Zoom ->
--                   (String -> IO ()) ->
--                   (forall m n. Frame m n -> IO ()) ->
--                   IO ()
-- plotAndDisplay m n info palette zoom dispLeft dispRight =
--   csp2 m n (\x y -> either dispLeft dispRight $ plot x y info palette zoom) 


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

--PRIVATE FUNCTIONS

--VALIDATION
validateHeightWidth :: SNat m -> SNat n -> Either String (SNat m, SNat n)
validateHeightWidth SNil SNil = Left "The height width values are 0"
validateHeightWidth _    SNil = Left "The width value is 0"
validateHeightWidth SNil _    = Left "The height value is 0"
validateHeightWidth m    n    = Right (m,n)

  -- if m == SNil || n == SNil
  -- then Left "The height width values are not Natural Numbers excluding 0"
  -- else Right (m,n)

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

-- build :: Nat -> (a -> t a -> t a) -> (a -> a) -> a -> t a -> t a
-- build 0 _ _ _   las = las
-- build n b f ini las = b (f ini) (build (n - 1) b f (f ini) las)

-- csp :: Nat -> Nat -> (forall m. SNat m -> r1 -> r2) -> (forall n. SNat n -> r1)  -> r2

-- IMPLEMENTATION 
standardGrid :: SNat m -> SNat n -> CoordGrid m n
standardGrid height width =
  let wincrement = getIncrement width 4
      hincrement = getIncrement height 4
      initVec = (:+ (2 - hincrement/2)) <$> build width (:*) (+ wincrement) (-2 + wincrement/2) VNil
  in buildMat height width (:^) (fmap (\x -> x - (0 :+ hincrement))) initVec MNil
  
  -- let wincrement = getIncrement width 4
  --     hincrement = getIncrement height 4
  --     initVec = (:+ (-2)) <$> build width ((:*)) (+ wincrement) (-2) VNil
  -- in build height ((:^)) (fmap (+ (0 :+ hincrement))) initVec MNil
  
  -- in csp height width
  --    (\m v -> build m ((:^)) (fmap (+ hincrement)) v MNil)
  --    (\n -> (:+ (-2)) <$> build n ((:*)) (+ wincrement) (-2) VNil)
      -- initVec = csp width (\x ->(:+ (-2)) <$> build x ((:*)) (+ wincrement) (-2) VNil)
    
  -- in csp height (\x -> build x ((:^)) (fmap (+ hincrement)) initVec MNil)
  -- = productSpace (:+) (numSplitToAxis height (-2) 4) (numSplitToAxis width (-2) 4)

getIncrement :: SNat n -> Double -> Double
getIncrement n len = len/(fromIntegral . fromEnum $ n)

-- numSplitToAxis ::  Nat -> Double -> Double -> Vector n a
-- numSplitToAxis n start len = listToVec (takeWhile (<= 2) iterate (+ increment) start)
--   where increment = (fromIntegral . fromEnum $ n)/len

applyZoom :: Zoom -> CoordGrid m n -> CoordGrid m n
applyZoom zoom = fmap ((*) (factor zoom) . (+) (origin zoom))

doComputation :: ComputationInfo -> Complex Double -> ComputationResult
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

mandelbrotFunc :: Complex Double -> Complex Double ->
                  Complex Double
mandelbrotFunc c z = z*z + c
