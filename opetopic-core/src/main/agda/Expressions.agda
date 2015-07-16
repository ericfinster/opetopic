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

    -- Shell : ℕ → Set
    -- Shell zero = ⊤
    -- Shell (suc n) = Tree (Expr n) n × Expr n

    data Con : Set where
      ● : Con
      ⟨_ , _⟩ : {n : ℕ} → (Γ : Con) → Ty Γ n → Con

    data Ty (Γ : Con) : ℕ → Set where
      T : Ty Γ 0 
      -- shell : {n : ℕ} → Shell n → Ty
      -- balanced : {n : ℕ} → PuncturedNiche n → Ty
      -- univeral : {n : ℕ} → Expr n → Ty

    data Tm (Γ : Con) (n : ℕ) (A : Ty Γ n) : ℕ → Set where
      var : ℕ → Tm Γ n A n

  --   -- This is just a pasting diagram missing 
  --   -- an element
  --   PuncturedNiche : ℕ → Set
  --   PuncturedNiche n = Derivative (Expr n) n

  --   data Expr : ℕ → Set where
  --     var : {n : ℕ} → Shell n → (id : String) → Expr n

  --     -- comp : {n : ℕ} → Tree (Expr n) n → Expr n
  --     -- fill : {n : ℕ} → Tree (Expr n) n → Expr (suc n)

  --   -- A type can either be a shell, or a proposition about
  --   -- a punctured niche (that it is balanced) or a cell (that
  --   -- it is universal).

  --   -- These are the things that we will have in the context.


  --   record Universal (n : ℕ) : Set where
  --     coinductive
  --     field

  --       cell : Expr n

  --   -- Now what.  I want a proof in the context extended by
  --   -- by a variable in the context the same shape as the
  --   -- target of the expression that the associated punctured
  --   -- nice is balanced ....
    
  --   record Balanced (n : ℕ) : Set where
  --     coinductive
  --     field

  --       pd : Derivative (Expr n) n
  
  -- test : Expr 0
  -- test = var tt "x"

  -- test1 : Expr 1
  -- test1 = var (pt (var tt "x") , var tt "y") "f"

  -- test2 : PuncturedNiche 0
  -- test2 = tt

  -- test3 : PuncturedNiche 1
  -- test3 = (pt (node test1 (pt leaf))) , ((test1 , tt) ∷ [])
