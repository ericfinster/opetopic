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
  STree = Free (EndoFix Free Id)

  mutual

    stabilize : {A : Set} {n : ℕ} (t : Tree A n) → STree A
    stabilize (pt a) = fix a (more (fix end (more end)))
    stabilize leaf = end
    stabilize (node a sh) = fix a (more (stabilize (mapTree sh stabilize)))

  -- So, this looks pretty good actually.  Then a complex is simply a sequence
  -- of such guys (or corresponding nesting types) which satisfy the zoom relation,
  -- which of course you would need to work out.
