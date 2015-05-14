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

  open Monad errorM

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

  external : {A : Set} → {n : ℕ} → A → Nesting A n
  external {n = zero} a = obj a
  external {n = suc n} a = dot a

  baseValue : {A : Set} → {n : ℕ} → Nesting A n → A
  baseValue (obj a) = a
  baseValue (dot a) = a
  baseValue (box a cn) = a

  toTree : {A : Set} → {n : ℕ} → Nesting A n → Tree A (suc n)
  toTree (obj a) = leaf
  toTree (dot a) = leaf
  toTree (box a cn) = node a (mapTree cn toTree)

  fromTree : {A : Set} → {n : ℕ} → Tree A (suc n) → Error (Nesting (A ⊎ (Address n)) n)
  fromTree {A} {n} tr = graftRec (λ a cn → succeed (box (inj₁ a) cn)) (λ addr → succeed (external (inj₂ addr))) tr
    where open GraftRec {A} {Nesting (A ⊎ (Address n)) n}

  extendNestingWith : {A B : Set} → {n : ℕ} → Nesting A n → B → Tree (Nesting B (suc n)) (suc n)
  extendNestingWith (obj a) b = leaf
  extendNestingWith (dot a) b = leaf
  extendNestingWith (box a cn) b = node (dot b) (mapTree cn (λ n → extendNestingWith n b))

  mutual

    spineFromCanopy : {A : Set} → {n : ℕ} → Tree (Nesting A n) n → Error (Tree A n)
    spineFromCanopy {A} {n} cn = traverseWithLocalData ⦃ monadIsApp errorM ⦄ cn (λ n _ ∂ → spineFromDeriv {A = A} n ∂) >>= join

    spineFromDeriv : {A : Set} → {n : ℕ} → Nesting A n → Derivative A n → Error (Tree A n)
    spineFromDeriv (obj a) ∂ = succeed (pt a) 
    spineFromDeriv (dot a) ∂ = succeed (∂ ← a) 
    spineFromDeriv (box a cn) ∂ = spineFromCanopy cn

  -- The following routine, given a canopy, produces a new nesting with the external cells holding 
  -- the short address of a leaf.  See sprouting routine for usage.
  canopyAddressExtend : {A : Set} → {n : ℕ} → Tree (Nesting A (suc n)) (suc n) → Error (Nesting (Address (suc n)) n)
  canopyAddressExtend tr = canopyAddressExtend₀ tr []

    where canopyAddressExtend₀ : {A : Set} → {n : ℕ} → Tree (Nesting A (suc n)) (suc n) → Address (suc n) → Error (Nesting (Address (suc n)) n)
          canopyAddressExtend₀ leaf addr = succeed (external addr)
          canopyAddressExtend₀ {A} {n} (node nst sh) addr = 
            spineFromDeriv nst (const sh leaf , []) -- This get's us a tree at the current stage, which I think we are going to join or something
            >>= (λ tr → traverseWithAddress ⦃ monadIsApp errorM ⦄ sh (λ b d → canopyAddressExtend₀ b (d ∷ addr)) -- Here we loop the computation
            >>= (λ shRes → graftRec (λ _ cn → succeed (box addr cn)) (valueAt shRes) tr))

            where open GraftRec {A} {Nesting (Address (suc n)) n}

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

  visitNesting : {A : Set} → {n : ℕ} → ZipperNst A n → Address n → Error (ZipperNst A n)
  visitNesting (obj a , cntxt) addr = fail "Cannot visit the initial object"
  visitNesting (dot a , cntxt) addr = fail "Cannot visit a dot"
  visitNesting {n = zero} (box a (pt as) , cntxt) tt = succeed (as , ((a , tt) ∷ cntxt))
  visitNesting {n = suc n} (box a cn , cntxt) addr = 
    seek (cn , []) addr 
    >>= (λ { (leaf , cntxt₀) → fail "Invalid address in canopy" ; 
             (node nst vsh , cntxt₀) → succeed (nst , ((a , (vsh , cntxt₀)) ∷ cntxt)) })

  seekNesting : {A : Set} → {n : ℕ} → ZipperNst A n → Address (suc n) → Error (ZipperNst A n)
  seekNesting z [] = succeed z
  seekNesting z (d ∷ ds) = seekNesting z ds >>= (λ z₀ → visitNesting z₀ d)

  seekToNesting : {A : Set} → {n : ℕ} → Nesting A n → Address (suc n) → Error (ZipperNst A n)
  seekToNesting nst addr = seekNesting (nst , []) addr

  sibling : {A : Set} → {n : ℕ} → ZipperNst A (suc n) → Address n → Error (ZipperNst A (suc n))
  sibling (fcs , []) dir = fail "Sibling error"
  sibling {n = zero} (fcs , (a , pt leaf , cntxt₀) ∷ cntxt) dir = fail "Sibling error"
  sibling {n = zero} (fcs , (a , pt (node nfcs sh) , cntxt₀) ∷ cntxt) dir = 
    succeed (nfcs , (a , sh , (fcs , tt) ∷ cntxt₀) ∷ cntxt)
  sibling {n = suc n} (fcs , (a , verts , hcn) ∷ cntxt) dir = 
    seek (verts , []) dir 
    >>= (λ { (leaf , _) → fail "Sibling error" ; 
             (node leaf _ , _) → fail "Sibling error" ; 
             (node (node nfcs vrem) hmask , vcn) → 
               succeed (nfcs , (a , vrem , (fcs , hmask , vcn) ∷ hcn) ∷ cntxt) })

  predecessor : {A : Set} → {n : ℕ} → ZipperNst A n → Error (ZipperNst A n)
  predecessor {n = zero} _ = fail "No predecessor in nesting"
  predecessor {n = suc n} (fcs , []) = fail "No predecessor in nesting"
  predecessor {n = suc n} (fcs , (a , verts , []) ∷ cs) = fail "No predecessor in nesting"
  predecessor {n = suc n} (fcs , (a , verts , (pred , ∂) ∷ vs) ∷ cs) = succeed (pred , (a , ∂ ← node fcs verts , vs) ∷ cs)

  predecessorWhich : {A : Set} → {n : ℕ} → ZipperNst A n → (A → Bool) → Error (ZipperNst A n)
  predecessorWhich {n = zero} z p = if p (baseValue (proj₁ z)) then η z else fail "Predecessor fail on object"
  predecessorWhich {n = suc n} z p = 
    if p (baseValue (proj₁ z)) 
    then η z 
    else (predecessor z >>= (λ pred → predecessorWhich pred p))


