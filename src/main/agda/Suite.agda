--
--  Suite.agda - Indexed sequences
--
--  Eric Finster
--

open import Prelude
open import Mtl
open import Nesting

module Suite where

  infixl 4 _▶_

  data Suite (P : ℕ → Set) : ℕ → Set where
    ∥ : Suite P 0
    _▶_ : {n : ℕ} → Suite P n → P n → Suite P (suc n)

  mapSuite : {P Q : ℕ → Set} → {n : ℕ} → Suite P n → ((k : ℕ) → P k → Q k) → Suite Q n
  mapSuite ∥ f = ∥
  mapSuite (tl ▶ hd) f = mapSuite tl f ▶ f _ hd

  drop : {P : ℕ → Set} → {n : ℕ} → (k : ℕ) → (k≤n : k ≤ n) → Suite P n → Suite P (Δ k≤n)
  drop .0 z≤n s = s
  drop .(suc k) (s≤s {k} {n} k≤n) (tl ▶ hd) = drop k k≤n tl 

  head : {P : ℕ → Set} → {n : ℕ} → Suite P (suc n) → P n
  head (tl ▶ hd) = hd
  
  tail : {P : ℕ → Set} → {n : ℕ} → Suite P (suc n) → Suite P n
  tail (tl ▶ hd) = tl

  getAt : {P : ℕ → Set} → {n : ℕ} → (k : ℕ) → (k≤n : k ≤ n) → Suite P (suc n) → P k
  getAt {P} {n} k k≤n s = head (transport (λ m → Suite P m) p (drop {n = suc n} (Δ k≤n) (≤-suc (Δ-≤-lem k≤n)) s)) 

    where p : Δ (≤-suc (Δ-≤-lem k≤n)) == suc k
          p = Δ (≤-suc (Δ-≤-lem k≤n)) =⟨ Δ-lem (Δ-≤-lem k≤n) ⟩ 
              suc (Δ (Δ-≤-lem k≤n)) =⟨ ap suc (Δ-≤-lem-eq k≤n) ⟩ 
              suc k ∎
