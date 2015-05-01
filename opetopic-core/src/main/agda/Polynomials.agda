--
--  Polynomials.agda - Indexed Polynomials
--
--  Eric Finster
--

open import Prelude

module Polynomials where

  infixr 2 _⇛_
  infixl 4 _⊚_ 

  record Poly (I : Set) (J : Set) : Set₁ where

    field

      γ : I → Set
      ρ : Σ I γ → Set
      τ : Σ (Σ I γ) ρ → J

  ⟦_⟧ : {I J : Set} → Poly I J → (J → Set) → (I → Set)
  ⟦ P ⟧ X i = Σ[ c ∈ γ i ] ((p : ρ (i , c)) → X (τ ((i , c) , p)))
    where open Poly P

  _⊚_ : {I J K : Set} → Poly J K → Poly I J → Poly I K
  P ⊚ Q = let open Poly in 
    record { 
      γ = ⟦ Q ⟧ (γ P) ; 
      ρ = λ { (i , c , φ) → Σ[ p ∈ ρ Q (i , c) ] ρ P (τ Q ((i , c) , p) , φ p) } ; 
      τ = λ { ((i , c , φ) , p₀ , p₁) → τ P ((τ Q ((i , c) , p₀) , φ p₀) , p₁) } 
    }

  record _⇛_ {I J : Set} (P Q : Poly I J) : Set where

    open Poly
    open _≃_

    field

      γ-map : (i : I) → γ P i → γ Q i

      ρ-eqv : (i : I) → (c : γ P i) → 
              ρ P (i , c) ≃ ρ Q (i , γ-map i c)

      τ-coh : (i : I) → (c : γ P i) → (p : ρ P (i , c)) → 
              τ P ((i , c) , p) == τ Q ((i , γ-map i c) , (f (ρ-eqv i c) p))

  Id : (I : Set) → Poly I I
  Id I = record { 
           γ = λ i → ⊤ ; 
           ρ = λ { (i , tt) → ⊤ } ; 
           τ = λ { ((i , tt) , tt) → i } 
         }

  data W {I : Set} (P : Poly I I) : I → Set where
    leaf : (i : I) → W P i
    node : (i : I) → ⟦ P ⟧ (W P) i → W P i

  module _ {I : Set} {P : Poly I I} where

    open Poly P

    leafOf : {i : I} → W P i → Set
    leafOf (leaf i) = ⊤
    leafOf (node i (c , φ)) = Σ[ p ∈ ρ (i , c) ] leafOf (φ p)
      
    leafType : {i : I} → {w : W P i} → leafOf w → I
    leafType {w = leaf i} tt = i
    leafType {w = node ._ _} (_ , l) = leafType l

    nodeOf : {i : I} → W P i → Set
    nodeOf (leaf i) = ⊥
    nodeOf (node i (c , φ)) = ⊤ ⊎ (Σ[ p ∈ ρ (i , c) ] nodeOf (φ p))

    nodeCons : {i : I} → {w : W P i} → nodeOf w → Σ I γ
    nodeCons {w = leaf ._} ()
    nodeCons {w = node i (c , _)} (inj₁ tt) = (i , c)
    nodeCons {w = node ._ _} (inj₂ (_ , n)) = nodeCons n

  record PolyMonad (I : Set) : Set₁ where

    field

      P : Poly I I
  
      η : Id I ⇛ P
      μ : P ⊚ P ⇛ P

  module FreeMonadDefn (I : Set) (P : Poly I I) where
    
    open Poly

    FmP : Poly I I
    FmP = record { 
            γ = W P ; 
            ρ = λ { (i , w) → leafOf w } ; 
            τ = λ { ((i , w) , l) → leafType l } 
          }

    fm-η : Id I ⇛ FmP
    fm-η = record { 
             γ-map = λ i c → leaf i ; 
             ρ-eqv = λ i c → id-equiv ⊤ ;
             τ-coh = λ i c p → idp
           }

    {-# TERMINATING #-}
    fm-graft : (i : I) → γ (FmP ⊚ FmP) i → γ FmP i
    fm-graft i (leaf .i , φ) = φ tt
    fm-graft i (node .i (c , ψ) , φ) = 
      node i (c , (λ p₀ → fm-graft (τ P ((i , c) , p₀)) (ψ p₀ , (λ p₁ → φ (p₀ , p₁)))))

    {-# TERMINATING #-}
    fm-grafting-eqv-f : (i : I) (c : γ (FmP ⊚ FmP) i) → ρ (FmP ⊚ FmP) (i , c) → ρ FmP (i , fm-graft i c)
    fm-grafting-eqv-f i (leaf .i , φ) (tt , p) = p
    fm-grafting-eqv-f i (node .i (c , ψ) , φ) ((p₀ , l₀) , l₁) = 
      (p₀ , fm-grafting-eqv-f (τ P ((i , c) , p₀)) (ψ p₀ , (λ p₁ → φ (p₀ , p₁))) (l₀ , l₁))

    {-# TERMINATING #-}
    fm-grafting-eqv-g : (i : I) (c : γ (FmP ⊚ FmP) i) → ρ FmP (i , fm-graft i c) → ρ (FmP ⊚ FmP) (i , c) 
    fm-grafting-eqv-g i (leaf .i , φ) p = (tt , p)
    fm-grafting-eqv-g i (node .i (c , ψ) , φ) (p₀ , l) with fm-grafting-eqv-g (τ P ((i , c) , p₀)) (ψ p₀ , (λ p₁ → φ (p₀ , p₁))) l
    fm-grafting-eqv-g i (node .i (c , ψ) , φ) (p₀ , l) | (l₀ , l₁) = (p₀ , l₀) , l₁

    fm-grafting-eqv-η : (i : I) (c : γ (FmP ⊚ FmP) i) (p : ρ (FmP ⊚ FmP) (i , c)) →
                        p == fm-grafting-eqv-g i c (fm-grafting-eqv-f i c p)
    fm-grafting-eqv-η i (leaf .i , φ) (tt , p) = idp
    fm-grafting-eqv-η i (node .i (c , ψ) , φ) ((p₀ , l₀) , l₁) = {!ap !}

    fm-grafting-eqv-ε : (i : I) (c : γ (FmP ⊚ FmP) i) (p : ρ FmP (i , fm-graft i c)) → 
                        fm-grafting-eqv-f i c (fm-grafting-eqv-g i c p) == p
    fm-grafting-eqv-ε i c p = {!!}

    fm-grafting-eqv : (i : I) (c : γ (FmP ⊚ FmP) i) → ρ (FmP ⊚ FmP) (i , c) ≃ ρ FmP (i , fm-graft i c)
    fm-grafting-eqv i c = record { 
                            f = fm-grafting-eqv-f i c ; 
                            g = fm-grafting-eqv-g i c ; 
                            η = fm-grafting-eqv-η i c ; 
                            ε = fm-grafting-eqv-ε i c 
                          }

    fm-μ : FmP ⊚ FmP ⇛ FmP
    fm-μ = record { 
             γ-map = fm-graft ; 
             ρ-eqv = fm-grafting-eqv ; 
             τ-coh = {!!} 
           }

    Fm : PolyMonad I
    Fm = record { P = FmP ; η = fm-η ; μ = fm-μ }
