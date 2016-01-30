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

  data ST (A : Set) : Set where
    lf : ST A
    nd : A → ST (ST A) → ST A

  mapST : {A B : Set} → (f : A → B) → ST A → ST B
  mapST f lf = lf
  mapST f (nd a sh) = nd (f a) (mapST (mapST f) sh)

  obj : {A : Set} → A → ST A
  obj a = nd a (nd lf lf)

  stabilize : {A : Set} {n : ℕ} → Tree A n → ST A
  stabilize (pt a) = obj a
  stabilize leaf = lf
  stabilize (node a sh) = nd a (mapST stabilize (stabilize sh))

  -- Addresses and lookup

  data SA : Set where
    ⟨_⟩ : List SA → SA

  lookup : {A : Set} (tr : ST A) (addr : SA) → Maybe A
  lookup lf addr = nothing
  lookup (nd a sh) ⟨ [] ⟩ = just a
  lookup (nd a sh) ⟨ sa ∷ sas ⟩ = lookup sh sa >>= (λ b → lookup b ⟨ sas ⟩ )
    where open Monad maybeM

  -- Derivatives, Contexts and Zippers

  mutual 

    data ST-Γ (A : Set) : Set where
      • : ST-Γ A
      ⟨_,_⟩∶_ : A → ST-∂ (ST A) → ST-Γ A → ST-Γ A

    data ST-∂ (A : Set) : Set where
      at : ST (ST A) → ST-Γ A → ST-∂ A

    ST-Z : Set → Set
    ST-Z A = ST A × ST-Γ A

    plug : {A : Set} → ST-∂ A → A → ST A
    plug (at sh Γ) a = close Γ (nd a sh) 

    close : {A : Set} → ST-Γ A → ST A → ST A
    close • tr = tr
    close (⟨ a , ∂ ⟩∶ Γ) tr = close Γ (nd a (plug ∂ tr))

    goto : {A : Set} → ST-Z A → SA → Maybe (ST-Z A)
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

    addrOf : {A : Set} → ST-Γ A → SA
    addrOf • = ⟨ [] ⟩
    addrOf (⟨ a , at _ hΓ ⟩∶ Γ) = addrOf hΓ +∶ addrOf Γ

  -- What are stable nestings?

  data SN (A : Set) : Set where
    dot : A → SN A
    box : A → ST (SN A) → SN A

  toST : {A : Set} → SN A → ST A
  toST (dot a) = obj a
  toST (box a cn) = nd a (mapST toST cn)

  -- An address predicate?

  data HasAddr {A : Set} : ST A → SA → Set where
    lfAddr : HasAddr lf ⟨ [] ⟩ 
    zpAddr : (tr : ST A) → (Γ : ST-Γ A) → (sa : SA) → (ev : HasAddr tr sa) → 
             HasAddr (close Γ tr) (sa ++∶ addrOf Γ)

  -- It's intersting to wonder about this kind of stable
  -- multiplication.  Something like it surely works.

  st-graft : {A : Set} → ST A → ST (ST A) → Maybe (ST A)
  st-graft = {!!}

  st-join : {A : Set} → ST (ST A) → Maybe (ST A)
  st-join lf = just lf
  st-join (nd st ssh) = {!!}

  -- And I guess the point of the above would be that, given a
  -- stable nesting, you would try to compute the spine and make
  -- sure that the spine of one was exactly the tree reflection
  -- of the other.

  -- Hmmm ... can I encode some kind of type system here that
  -- let's me check when functions between stable opetopes are
  -- well formed ????

