{-# OPTIONS --no-termination-check --no-positivity-check #-}
--
--  Free.agda - Free Monads and their fixed points
--
--  Eric Finster
--

open import Prelude
open import Mtl
open import Tree

module Free where

  data Free (F : Set → Set) (X : Set) : Set where
    end : Free F X
    fix : X → F (Free F X) → Free F X

  FreeId : Set → Set
  FreeId = Free Id

  mapFree : {F : Set → Set} ⦃ isF : Functor F ⦄ {X Y : Set} (f : X → Y) (m : Free F X) → Free F Y
  mapFree f end = end
  mapFree f (fix x m) = fix (f x) (fmap (mapFree f) m)
    where open Functor ⦃ ... ⦄ 
    
  freeIsFunctor : {F : Set → Set} ⦃ isF : Functor F ⦄ → Functor (Free F)
  freeIsFunctor = record { fmap = mapFree }

  Tr : (n : ℕ) → Set → Set
  Tr zero = Id
  Tr (suc n) = Free (Tr n)

  TrF : (n : ℕ) → Functor (Tr n)
  TrF zero = idF
  TrF (suc n) = freeIsFunctor ⦃ TrF n ⦄

  encode : (n : ℕ) (A : Set) → Tree A n → Tr n A
  encode zero A (pt a) = a
  encode (suc n) A leaf = end
  encode (suc n) A (node a sh) = 
    fix a (encode n (Free (Tr n) A) (mapTree sh (encode (suc n) A)))

  decode : (n : ℕ) (A : Set) → Tr n A → Tree A n
  decode zero A a = pt a
  decode (suc n) A end = leaf
  decode (suc n) A (fix a sh) = 
    node a (decode n (Tree A (suc n)) (fmap (decode (suc n) A) sh))
    where open Functor (TrF n)


  --
  --  Fixing the result
  --

  data FreeFix (F : Set → Set) (X : Set) : Set where
    -- done : FreeFix F X 
    more : Free (FreeFix F) X → FreeFix F X

  mutual 

    mapFreeFix : {F : Set → Set} ⦃ isF : Functor F ⦄ {X Y : Set} (f : X → Y) → FreeFix F X → FreeFix F Y
    -- mapFreeFix f done = done
    mapFreeFix {F} ⦃ isF ⦄ f (more m) = more (mapFree {FreeFix F} ⦃ freeFixIsFunctor ⦄ f m)

    freeFixIsFunctor : {F : Set → Set} ⦃ isF : Functor F ⦄ → Functor (FreeFix F)
    freeFixIsFunctor = record { fmap = mapFreeFix }

  STree : Set → Set
  STree X = Free (FreeFix Id) X

  data Nest (A : Set) : Set where
    ext : (a : A) → Nest A
    int : (a : A) → (cn : FreeFix Id (Nest A)) → Nest A

  stabilize : {A : Set} {n : ℕ} (t : Tree A n) → STree A
  stabilize (pt a) = fix a (more (fix end (more end)))
  stabilize leaf = end
  stabilize (node a sh) = fix a (more (stabilize (mapTree sh stabilize)))

  toTree : {A : Set} (n : Nest A) → STree A
  toTree (ext a) = end
  toTree (int a cn) = fix a (mapFreeFix {Id} ⦃ idF ⦄ toTree cn)
