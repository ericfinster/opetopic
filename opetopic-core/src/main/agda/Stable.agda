{-# OPTIONS --no-termination-check --no-positivity-check #-}
--
--  Stable.agda - Stable Higher Dimensional Trees
--
--  Eric Finster
--

open import Prelude
open import Mtl
open import Tree

module Stable where

  -- An elementary description of stable trees

  data STree (A : Set) : Set where
    lf : STree A
    nd : A → STree (STree A) → STree A

  mapSTree : {A B : Set} → (f : A → B) → STree A → STree B
  mapSTree f lf = lf
  mapSTree f (nd a sh) = nd (f a) (mapSTree (mapSTree f) sh)

  obj : {A : Set} → A → STree A
  obj a = nd a (nd lf lf)

  stabilize : {A : Set} {n : ℕ} → Tree A n → STree A
  stabilize (pt a) = obj a
  stabilize leaf = lf
  stabilize (node a sh) = nd a (mapSTree stabilize (stabilize sh))

  -- Addresses and lookup

  data SA : Set where
    ⟨_⟩ : List SA → SA

  lookup : {A : Set} (tr : STree A) (addr : SA) → Maybe A
  lookup lf addr = nothing
  lookup (nd a sh) ⟨ [] ⟩ = just a
  lookup (nd a sh) ⟨ sa ∷ sas ⟩ = lookup sh sa >>= (λ b → lookup b ⟨ sas ⟩ )
    where open Monad maybeM

  -- Derivatives, Contexts and Zippers

  mutual 

    data STree-Γ (A : Set) : Set where
      • : STree-Γ A
      ⟨_,_⟩∶_ : A → STree-∂ (STree A) → STree-Γ A → STree-Γ A

    data STree-∂ (A : Set) : Set where
      at : STree (STree A) → STree-Γ A → STree-∂ A

    STree-Z : Set → Set
    STree-Z A = STree A × STree-Γ A

    plug : {A : Set} → STree-∂ A → A → STree A
    plug (at sh Γ) a = close Γ (nd a sh) 

    close : {A : Set} → STree-Γ A → STree A → STree A
    close • tr = tr
    close (⟨ a , ∂ ⟩∶ Γ) tr = close Γ (nd a (plug ∂ tr))

    goto : {A : Set} → STree-Z A → SA → Maybe (STree-Z A)
    goto z ⟨ [] ⟩ = just z
    goto z ⟨ sa ∷ sas ⟩ = 
      goto z ⟨ sas ⟩ 
      >>= (λ { (lf , Γ) → nothing 
             ; (nd a sh , Γ) → goto (sh , •) sa 
                                >>= (λ { (lf , hΓ) → nothing 
                                       ; (nd br hsh , hΓ) → just (br , ⟨ a , at hsh hΓ ⟩∶ Γ) }) })
      where open Monad maybeM

    _+∶_ : SA → SA → SA
    sa +∶ ⟨ sas ⟩ = ⟨ sa ∷ sas ⟩

    _++∶_ : SA → SA → SA
    ⟨ s ⟩ ++∶ ⟨ t ⟩ = ⟨ s ++ t ⟩

    addrOf : {A : Set} → STree-Γ A → SA
    addrOf • = ⟨ [] ⟩
    addrOf (⟨ a , at _ hΓ ⟩∶ Γ) = addrOf hΓ +∶ addrOf Γ

  -- What are stable nestings?

  data SN (A : Set) : Set where
    dot : A → SN A
    box : A → STree (SN A) → SN A

  toSTree : {A : Set} → SN A → STree A
  toSTree (dot a) = obj a
  toSTree (box a cn) = nd a (mapSTree toSTree cn)

  -- An address predicate?

  data HasAddr {A : Set} : STree A → SA → Set where
    lfAddr : HasAddr lf ⟨ [] ⟩ 
    zpAddr : (tr : STree A) → (Γ : STree-Γ A) → (sa : SA) → (ev : HasAddr tr sa) → 
             HasAddr (close Γ tr) (sa ++∶ addrOf Γ)

  --
  --  I want to test multiple trees
  -- 

  open Monad maybeM hiding (fmap)

  data MTree (A : Set) : Set where
    mobj : (a : A) → MTree A
    mfix : MTree (STree A) → MTree A

  MTreeF : Functor MTree
  MTreeF = record { fmap = λ { f (mobj a) → mobj (f a) 
                             ; f (mfix mt) → let open Functor MTreeF in mfix (fmap (mapSTree f) mt) } }

  open Functor MTreeF

  associate : {A : Set} → ℕ → MTree A → Maybe (MTree (MTree A))
  associate zero mt = just (mobj mt)
  associate (suc n) (mobj a) = nothing
  associate (suc n) (mfix mt) = associate n mt >>= (λ mm → just (fmap mfix mm)) 
    
  --  Okay, now what?

  
