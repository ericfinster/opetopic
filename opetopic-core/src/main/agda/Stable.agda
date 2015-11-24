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

  test0 : Free Id ℕ
  test0 = end 

  test1 : Free Id ℕ
  test1 = fix 3 end

  test2 : Free Id ℕ
  test2 = fix 7 (fix 14 end)

  test3 : Free (Free Id) ℕ
  test3 = fix 3 (fix end (fix end end))

  -- Okay, so this looks about right.
  -- Now, what should be next?  To get stable
  -- things, I want to take a fixedpoint of this
  -- guy.

  data EndoFix (Φ : (Set → Set) → (Set → Set)) (F : Set → Set) (X : Set) : Set where
    done : EndoFix Φ F X
    more : Φ (EndoFix Φ F) X → EndoFix Φ F X

  STree : Set → Set
  STree = EndoFix Free Id

  blorp0 : STree ℕ
  blorp0 = more (fix 3 done)
  
  blorp1 : STree ℕ
  blorp1 = more (fix 4 (more (fix end done)))

  mutual

    stabilize' : {A : Set} {n : ℕ} (t : Tree A (suc n)) → Free STree A
    stabilize' leaf = end
    stabilize' (node a sh) = fix a (stabilize (mapTree sh stabilize'))

    stabilize : {A : Set} {n : ℕ} (t : Tree A n) → STree A
    stabilize {n = zero} (pt a) = more (fix a (more (fix end done)))
    stabilize {n = suc n} t = more (stabilize' t)

    -- stabilize : {A : Set} {n : ℕ} (t : Tree A n) → STree A
    -- stabilize (pt a) = more (fix a (more (fix end done)))
    -- stabilize leaf = more end
    -- stabilize {A} (node a sh) = let ssh : STree (Free (STree) A)
    --                                 ssh = stabilize (mapTree sh (λ b → {!stabilize b!})) -- stabilize (mapTree sh stabilize)
    --                           in more (fix a ssh)
