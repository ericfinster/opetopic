--
--  Expressions.agda - Higher Dimensional Expressions
--

open import Prelude
open import Mtl
open import Tree
open import Nesting
open import Suite
open import Complex

module Expressions where

  -- Let's play with some expressions

  mutual

    Shell : ℕ → Set
    Shell n = Tree (Expr n) n × Expr n

    data Nook : ℕ → Set where
      in-nook : {n : ℕ} → Derivative (Expr n) n → Expr n → Nook n
      out-nook : {n : ℕ} → Tree (Expr n) n → Nook n

    data Expr : ℕ → Set where
      ⊚ : (ob : String) → Expr 0
      var : {n : ℕ} → (nm : String) → Shell n → Expr (suc n)
      tgt : {n : ℕ} → Nook n → Expr n
      fill : {n : ℕ} → Nook n → Expr (suc n)

  -- open Monad errorM

  -- complexDisc : (n : ℕ) → Expr n → Expr n → Error (Expr n)
  -- complexDisc n e₀ e₁ = succeed e₀

  -- -- Easy peasy!!
  -- toComplex : {n : ℕ} → Expr n → Error (Complex Expr n)
  -- toComplex {zero} (⊚ str) = succeed (∥ ▶ (obj (⊚ str)))
  -- toComplex {suc zero} (● str (pt (⊚ s)) (⊚ t)) = 
  --   succeed (∥ ▶ box (⊚ t) (pt (obj (⊚ s))) ▶ dot (● str (pt (⊚ s)) (⊚ t)))
  -- toComplex {suc (suc n)} (● str src tgt) = 
  --   traverseTree ⦃ errorA ⦄ src toComplex 
  --   >>= (λ srcTree → paste srcTree 
  --   >>= (λ { (tl , cn) → succeed (tl ▶ box tgt cn ▶ dot (● str src tgt)) }))

  --   where open ComplexGrafting Expr complexDisc

