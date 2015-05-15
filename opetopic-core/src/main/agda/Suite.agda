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

  _↑_ : (P : ℕ → Set) → ℕ → (ℕ → Set)
  P ↑ k = λ n → P (k + n)

  data Suite (P : ℕ → Set) : ℕ → Set where
    ∥ : Suite P 0
    _▶_ : {n : ℕ} → Suite P n → P n → Suite P (suc n)

  mapSuite : {P Q : ℕ → Set} → {n : ℕ} → Suite P n → ((k : ℕ) → P k → Q k) → Suite Q n
  mapSuite ∥ f = ∥
  mapSuite (tl ▶ hd) f = mapSuite tl f ▶ f _ hd

  traverseSuite : {G : Set → Set} → ⦃ apG : Applicative G ⦄ → {P Q : ℕ → Set} → {n : ℕ} → 
                  Suite P n → ((k : ℕ) → P k → G (Q k)) → G (Suite Q n)
  traverseSuite ∥ f = pure ∥
    where open Applicative ⦃ ... ⦄
  traverseSuite (tl ▶ hd) f = (pure _▶_) ⊛ traverseSuite tl f ⊛ f _ hd
    where open Applicative ⦃ ... ⦄

  grab : {P : ℕ → Set} → {n k : ℕ} → (k≤n : k ≤ n) → Suite P n → (Suite P (Δ k≤n) × Suite (P ↑ (Δ k≤n)) k)
  grab z≤n s = s , ∥
  grab {P} (s≤s {k} {n} k≤n) (tl ▶ hd) = let x , y = grab k≤n tl in x , (y ▶ transport! P (Δ-+-lem k≤n) hd)

  smash : {P : ℕ → Set} → {n m : ℕ} → Suite P n → Suite (P ↑ n) m → Suite P (n + m)
  smash s ∥ = transport (Suite _) +-unit-r s
  smash {P} {n} {suc m} s (tl ▶ hd) = transport! (Suite _) (+-suc {n} {m}) (smash s tl ▶ hd)

  prepend : {P : ℕ → Set} → {n m : ℕ} → Suite (P ↑ (suc m)) n → P m → Suite (P ↑ m) (suc n)
  prepend {P} ∥ p = ∥ ▶ transport P +-unit-r p
  prepend {P} {suc n} {m} (tl ▶ hd) p = prepend {P} {n} {m} tl p ▶ transport! P (+-suc {m} {n}) hd

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
