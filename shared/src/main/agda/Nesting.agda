--
--  Nesting.agda - Nestings and their zippers
--
--  Eric Finster
--

{-# OPTIONS --no-termination-check #-}

open import Prelude
open import Mtl
open import Tree

module Nesting where

  data Nesting (A : Set) : ℕ → Set where
    obj : (a : A) → Nesting A 0
    dot : {n : ℕ} → (a : A) → Nesting A (suc n)
    box : {n : ℕ} → (a : A) → (cn : Tree (Nesting A n) n) → Nesting A n

  traverseNesting : {G : Set → Set} → ⦃ isA : Applicative G ⦄ → {A B : Set} → {n : ℕ} → Nesting A n → (A → G B) → G (Nesting B n)
  traverseNesting (obj a) f = let open Applicative ⦃ ... ⦄ in pure obj ⊛ f a
  traverseNesting (dot a) f = let open Applicative ⦃ ... ⦄ in pure dot ⊛ f a
  traverseNesting (box a cn) f = pure box ⊛ f a ⊛ traverseTree cn (λ n → traverseNesting n f)
    where open Applicative ⦃ ... ⦄ 

  mapNesting : {A B : Set} → {n : ℕ} → Nesting A n → (A → B) → Nesting B n
  mapNesting = traverseNesting ⦃ idA ⦄ 

  traverseNestingWithAddr : {G : Set → Set} → ⦃ isA : Applicative G ⦄ → {A B : Set} → {n : ℕ} → Nesting A n → (A → Address (suc n) → G B) → G (Nesting B n)
  traverseNestingWithAddr {G = G} nst f = traverseWithAddr₀ nst [] f

    where open Applicative ⦃ ... ⦄

          traverseWithAddr₀ : {A B : Set} → {n : ℕ} → Nesting A n → Address (suc n) → (A → Address (suc n) → G B) → G (Nesting B n)
          traverseWithAddr₀ (obj a) base f = pure obj ⊛ f a base
          traverseWithAddr₀ (dot a) base f = pure dot ⊛ f a base
          traverseWithAddr₀ (box a cn) base f = pure box ⊛ f a base ⊛ traverseWithAddress cn (λ n d → traverseWithAddr₀ n (d ∷ base) f)

  mapNestingWithAddr : {A B : Set} → {n : ℕ} → Nesting A n → (A → Address (suc n) → B) → Nesting B n
  mapNestingWithAddr = traverseNestingWithAddr ⦃ idA ⦄ 

  baseValue : {A : Set} → {n : ℕ} → Nesting A n → A
  baseValue (obj a) = a
  baseValue (dot a) = a
  baseValue (box a cn) = a

  toTree : {A : Set} → {n : ℕ} → Nesting A n → Tree A (suc n)
  toTree {n = zero} (obj a) = leaf
  toTree {n = zero} (box a (pt nst)) = node a (pt (toTree nst))
  toTree {n = suc n} (dot a) = leaf
  toTree {n = suc n} (box a cn) = node a (mapTree cn toTree)

  extendNestingWith : {A B : Set} → {n : ℕ} → Nesting A n → B → Tree (Nesting B (suc n)) (suc n)
  extendNestingWith (obj a) b = leaf
  extendNestingWith (dot a) b = leaf
  extendNestingWith (box a cn) b = node (dot b) (mapTree cn (λ n → extendNestingWith n b))

  mutual

    spineFromCanopy : {M : Set → Set} → ⦃ isE : MonadError M ⦄ → {A : Set} → {n : ℕ} → Tree (Nesting A n) n → M (Tree A n)
    spineFromCanopy {M} {A} {n} cn = traverseWithLocalData ⦃ monadIsApp isMonad ⦄ cn (λ n _ ∂ → spineFromDeriv {A = A} n ∂) >>= join
      where open MonadError ⦃ ... ⦄

    spineFromDeriv : {M : Set → Set} → ⦃ isE : MonadError M ⦄ → {A : Set} → {n : ℕ} → Nesting A n → Derivative A n → M (Tree A n)
    spineFromDeriv (obj a) ∂ = let open MonadError ⦃ ... ⦄ in η (pt a)
    spineFromDeriv (dot a) ∂ = let open MonadError ⦃ ... ⦄ in η (∂ ← a)
    spineFromDeriv (box a cn) ∂ = spineFromCanopy cn

  ContextNst : Set → ℕ → Set
  ContextNst A n = List (A × Derivative (Nesting A n) n)

  DerivativeNst : Set → ℕ → Set
  DerivativeNst A n = Tree (Nesting A n) n × ContextNst A n

  ZipperNst : Set → ℕ → Set
  ZipperNst A n = Nesting A n × ContextNst A n

  closeNesting : {A : Set} → {n : ℕ} → ContextNst A n → Nesting A n → Nesting A n
  closeNesting [] nst = nst
  closeNesting ((a , d) ∷ ds) nst = closeNesting ds (box a (d ← nst))

  plugNesting : {A : Set} → {n : ℕ} → DerivativeNst A n → A → Nesting A n
  plugNesting (t , c) a = closeNesting c (box a t)

  --
  --  You might want to put all this in a module and open the error monad once and forall ...
  --

  visitNesting : {M : Set → Set} → ⦃ isE : MonadError M ⦄ → {A : Set} → {n : ℕ} → ZipperNst A n → Address n → M (ZipperNst A n)
  visitNesting (obj a , cntxt) addr = let open MonadError ⦃ ... ⦄ in failWith "Nesting visit fail"
  visitNesting (dot a , cntxt) addr = let open MonadError ⦃ ... ⦄ in failWith "Nesting visit fail"
  visitNesting {n = zero} (box a (pt as) , cntxt) tt = η (as , ((a , tt) ∷ cntxt))
    where open MonadError ⦃ ... ⦄
  visitNesting {n = suc n} (box a cn , cntxt) addr = 
    seek (cn , []) addr 
    >>= (λ { (leaf , cntxt₀) → failWith "Nesting visit fail" ; 
             (node nst vsh , cntxt₀) → η (nst , ((a , (vsh , cntxt₀)) ∷ cntxt)) })

    where open MonadError ⦃ ... ⦄ 

  seekNesting : {M : Set → Set} → ⦃ isE : MonadError M ⦄ → {A : Set} → {n : ℕ} → ZipperNst A n → Address (suc n) → M (ZipperNst A n)
  seekNesting z [] = let open MonadError ⦃ ... ⦄ in η z
  seekNesting z (d ∷ ds) = seekNesting z ds >>= (λ z₀ → visitNesting z₀ d)
    where open MonadError ⦃ ... ⦄

  sibling : {M : Set → Set} → ⦃ isE : MonadError M ⦄ → {A : Set} → {n : ℕ} → ZipperNst A (suc n) → Address n → M (ZipperNst A (suc n))
  sibling (fcs , []) dir = let open MonadError ⦃ ... ⦄ in failWith "Sibling error"
  sibling {n = zero} (fcs , (a , pt leaf , cntxt₀) ∷ cntxt) dir = failWith "Sibling error"
    where open MonadError ⦃ ... ⦄
  sibling {n = zero} (fcs , (a , pt (node nfcs sh) , cntxt₀) ∷ cntxt) dir = 
    η (nfcs , (a , sh , (fcs , tt) ∷ cntxt₀) ∷ cntxt)
    where open MonadError ⦃ ... ⦄
  sibling {n = suc n} (fcs , (a , verts , hcn) ∷ cntxt) dir = 
    seek (verts , []) dir 
    >>= (λ { (leaf , _) → failWith "Sibling error" ; 
             (node leaf _ , _) → failWith "Sibling error" ; 
             (node (node nfcs vrem) hmask , vcn) → 
               η (nfcs , (a , vrem , (fcs , hmask , vcn) ∷ hcn) ∷ cntxt) })
    where open MonadError ⦃ ... ⦄

  predecessor : {M : Set → Set} → ⦃ isE : MonadError M ⦄ → {A : Set} → {n : ℕ} → ZipperNst A n → M (ZipperNst A n)
  predecessor {n = zero} _ = let open MonadError ⦃ ... ⦄ in failWith "No predecessor in nesting"
  predecessor {n = suc n} (fcs , []) = let open MonadError ⦃ ... ⦄ in failWith "No predecessor in nesting"
  predecessor {n = suc n} (fcs , (a , verts , []) ∷ cs) = let open MonadError ⦃ ... ⦄ in failWith "No predecessor in nesting"
  predecessor {n = suc n} (fcs , (a , verts , (pred , ∂) ∷ vs) ∷ cs) = η (pred , (a , ∂ ← node fcs verts , vs) ∷ cs)
    where open MonadError ⦃ ... ⦄

  predecessorWhich : {M : Set → Set} → ⦃ isE : MonadError M ⦄ → {A : Set} → {n : ℕ} → ZipperNst A n → (A → Bool) → M (ZipperNst A n)
  predecessorWhich {n = zero} z p = if p (baseValue (proj₁ z)) then η z else failWith "Predecessor fail on object"
    where open MonadError ⦃ ... ⦄
  predecessorWhich {n = suc n} z p = 
    if p (baseValue (proj₁ z)) 
    then η z 
    else (predecessor z >>= (λ pred → predecessorWhich pred p))
    where open MonadError ⦃ ... ⦄

  seekToNesting : {M : Set → Set} → ⦃ isE : MonadError M ⦄ → {A : Set} → {n : ℕ} → Nesting A n → Address (suc n) → M (ZipperNst A n)
  seekToNesting nst addr = seekNesting (nst , []) addr


