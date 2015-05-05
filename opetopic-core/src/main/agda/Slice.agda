--
--  Slice.agda - The Slice Construction
--

{-# OPTIONS --no-positivity-check #-}
{-# OPTIONS --no-termination-check #-}

open import Prelude
open import Tree
open import Mtl

module Slice where

  data Slice (F : Set → Set) (A : Set) : Set where
    lf : Slice F A
    nd : A → F (Slice F A) → Slice F A

  mapSlice : {F : Set → Set} → ⦃ isF : Functor F ⦄ → {A B : Set} → (A → B) → Slice F A → Slice F B
  mapSlice f lf = lf
  mapSlice f (nd a fs) = nd (f a) (fmap (λ s → mapSlice f s) fs)
    where open Functor ⦃ ... ⦄

  sliceF : {F : Set → Set} → ⦃ isF : Functor F ⦄ → Functor (Slice F)
  sliceF = record { fmap = mapSlice }

  SliceTree : ℕ → Set → Set
  SliceTree zero = Id
  SliceTree (suc n) = Slice (SliceTree n)

  toNat : Slice Id ⊤ → ℕ
  toNat lf = 0
  toNat (nd tt s) = suc (toNat s)

  toList : {A : Set} → Slice Id A → List A
  toList lf = []
  toList (nd a s) = a ∷ toList s

  data Tr (A : Set) : Set where
    L : Tr A
    N : A → List (Tr A) → Tr A

  toTree : {A : Set} → Slice (Slice Id) A → Tr A
  toTree lf = L
  toTree (nd a s) = N a (mapList toTree (toList s))

  data FFix (F : (Set → Set) → (Set → Set)) (A : Set) : Set where
    ffix : F (FFix F) A → FFix F A

  StableTree : Set → Set
  StableTree = Slice (FFix Slice)

  stabilize : {A : Set} → {n : ℕ} → Tree A n → StableTree A
  stabilize (pt a) = nd a (ffix lf) -- Is this right?
  stabilize leaf = lf
  stabilize (node a tr) = nd a (ffix (stabilize (mapTree tr stabilize) ))

  test-tr : Tree ℕ 2
  test-tr = node 0 (node (node 1 (node leaf (pt (node leaf (pt leaf))))) (pt (node (node 2 (node leaf (pt leaf))) (pt (node leaf (pt leaf))))))

  depth : {A : Set} → StableTree A → ℕ
  depth lf = 0
  depth (nd _ (ffix st)) = suc (depth st)

  obj-stack : StableTree ℕ
  obj-stack = nd 0 (ffix (nd (nd 1 (ffix lf)) (ffix lf)))

  -- Ahh.  Perhaps to have a nesting, you should introduce a different constructor
  -- on the fixed point guy.

  -- Notice that the representation of a point is as an endomorphism on an unspecified 
  -- object.  This is in some way natural.  It's kind of what you would have thought
  -- would have happened.
