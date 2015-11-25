--
--  Stable.agda - Stable Higher Dimensional Trees
--
--  Eric Finster
--

{-# OPTIONS --no-termination-check --no-positivity-check #-}

open import Prelude
open import Mtl
open import Tree

module Stable where

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

  test0 : Free Id ℕ
  test0 = end 

  test1 : Free Id ℕ
  test1 = fix 3 end

  test2 : Free Id ℕ
  test2 = fix 7 (fix 14 end)

  test3 : Free (Free Id) ℕ
  test3 = fix 3 (fix end (fix end end))

  data FreeFix (F : Set → Set) (X : Set) : Set where
    done : FreeFix F X
    more : Free (FreeFix F) X → FreeFix F X

  mutual 

    mapFreeFix : {F : Set → Set} ⦃ isF : Functor F ⦄ {X Y : Set} (f : X → Y) → FreeFix F X → FreeFix F Y
    mapFreeFix f done = done
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
