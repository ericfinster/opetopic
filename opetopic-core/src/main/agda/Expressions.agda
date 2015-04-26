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

    Shell : ℕ → Set
    Shell n = Tree (Expr n) n × Expr n

    data Expr : ℕ → Set where
      ob : String → Expr 0 
      cell : {n : ℕ} → String → Shell n → Expr (suc n)
      fill : {n : ℕ} → Nook n → Expr (suc n)
      tgt : {n : ℕ} → Nook n → Expr n

    -- Now, how do you make sense of the target and it's filler?

    data Nook : ℕ → Set where
      in-nook : {n : ℕ} → Derivative (Expr n) n → Expr n → Nook n
      out-nook : {n : ℕ} → Tree (Expr n) n → Nook n

    shellComplex : {n : ℕ} → Shell n → Complex Expr n
    shellComplex (pd , t) = {!mapTree pd expressionComplex!}

    expressionComplex : {n : ℕ} → Expr n → Complex Expr n
    expressionComplex {zero} expr = ∥ ▶ (obj expr)
    expressionComplex {suc n} (cell nm sh) = shellComplex sh ▶ dot (cell nm sh)
    expressionComplex {suc n} (fill nk) = {!!}
    expressionComplex {suc n} (tgt nk) = {!!}

    x : Expr 0
    x = ob "x"

    y : Expr 0
    y = ob "y"

    z : Expr 0
    z = ob "z"

    f : Expr 1
    f = cell "f" (pt x , y)

    g : Expr 1
    g = cell "g" (pt y , z)

    g∘f : Expr 1
    g∘f = tgt (out-nook (node g (pt (node f (pt leaf)))))
    
    def-g∘f : Expr 2
    def-g∘f = fill (out-nook (node g (pt (node f (pt leaf)))))

    -- Fascinating.  Now we can have something like a predicate which
    -- says if these things are well typed or not.  Moreover, universality
    -- is also a predicate in this picture, not something intrinsic to the
    -- expression.

    -- You could phrase it this way: universality is a *tactic*.  It will try
    -- to build the algorithm for replacing universal cells by induction on 
    -- the expression.  This may fail.  And the user may choose to give an
    -- implementation himself.

    -- The next question is the most crucial one in all the land: where does one
    -- store or how does one type or describe this purported implementation?

    -- What does it *look* like?
    
