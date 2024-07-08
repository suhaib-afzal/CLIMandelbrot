{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

module Datatypes ( Nat(Nil, Succ)
                 , SNat(SNil, SSucc)
                 , toNat
                 , zero
                 , one
                 , two
                 , four
                 , eight
                 , Cmplx
                 , Vector(VNil, (:*))
                 , Matrix(MNil, (:^))
                 , traverseMat_
                 , singleton
                 , build
                 , buildMat ) where

import Data.Kind ( Type )
import Data.Complex ( Complex ( (:+) )
                    , magnitude )

-- Nat and SNat ----------------------------------------------------------------------------
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


-- Cmplx ---------------------------------------------------------------------------------------
type Cmplx = Complex Double

-- Matrix ---------------------------------------------------------------------------------------

--          :: Height -> Width -> Type -> Type
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

data SomeMat a = SomeMat (forall m n. Matrix m n a)

-- Allows an action to be executed at the end of each vector
traverseMat_ :: (a -> IO b) -> IO b -> Matrix m n a -> IO ()
traverseMat_ _  _   MNil    = pure ()
traverseMat_ io end (v:^vs) = traverseVec_ io v >> end >> traverseMat_ io end vs

singleton :: a -> Matrix (Succ Nil) (Succ Nil) a
singleton x = (x :* VNil) :^ MNil


-- Building vector and matrix datatypes using recursively applied functions ----------------
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



-- Vector -------------------------------------------------------------------------
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

instance Functor (Vector n) where
  fmap :: (a -> b) -> Vector n a -> Vector n b
  fmap _ VNil = VNil
  fmap f (val :* vec) = f val :* fmap f vec

traverseVec_ :: (a -> IO b) -> Vector n a -> IO ()
traverseVec_ _  VNil    = pure ()
traverseVec_ io (a:*as) = io a >> traverseVec_ io as