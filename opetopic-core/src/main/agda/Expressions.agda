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

  -- Okay, since the most important case of what I want to do is determined
  -- by the desire to glue together expressions, let's try to work out that
  -- case and see what happens.

  data Expr : ℕ → Set where
    ⊚ : Expr 0
    ● : {n : ℕ} → Tree (Expr n) n → Expr n → Expr (suc n)

  sourceTree : {n : ℕ} → Expr (suc n) → Tree (Expr n) n
  sourceTree (● src tgt) = src

  -- Expressions in this sense are essentially opetopes, 
  -- wrapped up in a weird way.  Let's make some low dims

  object : Expr 0
  object = ⊚

  arrow : Expr 1
  arrow = ● (pt ⊚) ⊚

  two-drop : Expr 2
  two-drop = ● leaf arrow
  
  two-glob : Expr 2
  two-glob = ● (node arrow (pt leaf)) arrow
  
  simplex : Expr 2
  simplex = ● (node arrow (pt (node arrow (pt leaf)))) arrow

  -- Good, yes.  Now the point is simply that we should be able
  -- to build a complex from this data.  This is essentially your
  -- original representation in scala: all the extra shit is duplicated.

  -- The goal is to the reproduce the unduplicated thing.

  Unit : ℕ → Set
  Unit _ = ⊤ 

  realize : {n : ℕ} → Expr n → Complex Unit n
  realize ⊚ = ∥ ▶ (obj tt)
  realize (● src tgt) = {!mapTree src realize!}

  -- It does not seem to be any different.  The fundamental problem
  -- is still to figure out how to glue together a pasting diagram
  -- of complexes.
