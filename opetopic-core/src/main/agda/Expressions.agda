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

  mutual

    data Ty : Set where
      U : Ty
      E : Tm U 0 → Ty
      F : {n : ℕ} → (A : Ty) → Shell A n → Ty
      Π : Ty → Ty → Ty

    data Tm : Ty → ℕ → Set where
      ob : {A : Ty} → String → Tm A 0
      cell : {A : Ty} → {n : ℕ} → String → Shell A n → Tm A (suc n)
      comp : {A : Ty} → {n : ℕ} → Shell A n → Nook A (suc n) → Tm A (suc n)
      fill : {A : Ty} → {n : ℕ} → Shell A n → Nook A (suc n) → Tm A (suc (suc n))

      -- Errrp.
      lam₁ : {A : Ty} → {n : ℕ} → String → (τ : Tm A (suc n)) → Tm (Π {!!} (F A (ShellOf τ))) 0

    data Nook (A : Ty) : ℕ → Set where
      in-nook : {n : ℕ} → Derivative (Tm A n) n → Tm A n → Nook A n
      out-nook : {n : ℕ} → Tree (Tm A n) n → Nook A n

    Shell : Ty → ℕ → Set
    Shell A n = Tree (Tm A n) n × Tm A n

    ShellOf : {A : Ty} → {n : ℕ} → Tm A (suc n) → Shell A n
    ShellOf = {!!}

  -- An example term and its type

  pd : Tree (Tm U 1) 1
  pd = node (cell "p" (pt (ob "A") , ob "B")) (pt (node (cell "q" (pt (ob "B") , ob "C")) (pt leaf)))

  example-tm : Tm U 2
  example-tm = fill (pt (ob "A") , ob "C") (out-nook pd)

  example-ty : Ty
  example-ty = F U (pd , comp (pt (ob "A") , ob "C") (out-nook pd))

  A : Ty
  A = E (ob "A")

  a₀ : Tm A 0
  a₀ = ob "a₀"

  -- Okay, this is a problem.  We have completely lost track of the 
  -- reference to the lower dimensional information.  We have no idea
  -- *what* cell this is the identity on.

  -- Mmm. Better should be that to give a nook, you already need to give
  -- the shell which the nook should live in.

  id-a₀ : Tm A 1
  id-a₀ = comp (pt (ob "a₀") , ob "a₀") (out-nook leaf)

  -- Now, I would like to abstract over the a₀ in the previous example.  When I do
  -- that, what is the resulting type? 

  id-as-lam : Tm (Π A (F A (pt (ob "a₀") , ob "a₀"))) 0
  id-as-lam = {!!}


  a₁ : Tm A 0
  a₁ = ob "a₁"

  f₀ : Tm A 1
  f₀ = cell "f₀" (pt a₀ , a₁)

  B : Ty
  B = E (ob "B")

  -- Ah, right.  But the point is that we need to work in contexts.  And as a result, 
  -- we should not be talking about just the identity type *in* some type, but the identity
  -- type in some kind of context.  

  -- Blah blah.  Don't confuse the terms of the theory with its proof theory.  For now, the simple
  -- looking terms above suffice to have a reasonable notion of abstraction.  Let's keep going with
  -- it and see if we can write down some more complicated terms to play with.

  -- Well, fuck me.  What next.  The idea, I guess is to start writing down a kind of type
  -- checker which decides if a given term can have a given type.  
