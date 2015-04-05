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

  traverseNestingWithAddr : {G : Set → Set} → ⦃ isA : Applicative G ⦄ → {A B : Set} → {n : ℕ} → Nesting A n → (A → Address (suc n) → G B) → G (Nesting B n)
  traverseNestingWithAddr {G = G} nst f = traverseWithAddr₀ nst [] f

    where open Applicative ⦃ ... ⦄

          traverseWithAddr₀ : {A B : Set} → {n : ℕ} → Nesting A n → Address (suc n) → (A → Address (suc n) → G B) → G (Nesting B n)
          traverseWithAddr₀ (obj a) base f = pure obj ⊛ f a base
          traverseWithAddr₀ (dot a) base f = pure dot ⊛ f a base
          traverseWithAddr₀ (box a cn) base f = pure box ⊛ f a base ⊛ traverseWithAddress cn (λ n d → traverseWithAddr₀ n (d ∷ base) f)

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

  -- -- extrude : {n : ℕ} → {A : Set} → Tree n (Nesting n A) → (A → Bool) → A → Maybe (Tree n (Nesting n A))
  -- -- extrude tr p dflt = 
  -- --   dejoin tr (λ nst → p (baseValue nst)) 
  -- --   >>= (λ tr₀ → just (mapTree (λ { (inj₁ nst) → nst 
  -- --                                 ; (inj₂ tr₁) → int dflt tr₁ }) tr₀))

  -- -- Right-o.  That looks pretty good actually.  Now, what's next?

  -- extrudeNesting : {n : ℕ} → {A B : Set} → A → Address n → Tree n (Nesting n A) → Tree n B → Maybe (Tree n (Nesting n A))
  -- extrudeNesting {zero} a addr tr msk = just (pt (int a tr))
  -- extrudeNesting {suc n} a addr tr msk = 
  --   seekTo addr tr 
  --   >>= (λ { (fcs , cntxt) → replace fcs msk (λ canopy → int a canopy) 
  --   >>= (λ newFcs → just (cntxt ↓ newFcs)) })

  mutual

    DerivativeNst : Set → ℕ → Set
    DerivativeNst A n = Tree (Nesting A n) n × ContextNst A n

    ContextNst : Set → ℕ → Set
    ContextNst A n = List (A × Derivative (Nesting A n) n)

    ZipperNst : Set → ℕ → Set
    ZipperNst A n = Nesting A n × ContextNst A n

  --   plug : {n : ℕ} → {A : Set} → DerivativeNst n A → A → Nesting n A
  --   plug (t , c) a = close c (int a t)

  --   close : {n : ℕ} → {A : Set} → ContextNst n A → Nesting n A → Nesting n A
  --   close [] nst = nst
  --   close ((a , d) ∷ ds) nst = close ds (int a (d ← nst))

  --   visitNesting : {n : ℕ} → {A : Set} → Address n → ZipperNst n A → Maybe (ZipperNst n A)
  --   visitNesting addr (obj a , cntxt) = nothing
  --   visitNesting addr (ext a , cntxt) = nothing
  --   visitNesting {zero} [] (int h (pt t) , cntxt) = just (t , (h , tt) ∷ cntxt)
  --   visitNesting {zero} (() ∷ addr) (int a sh , cntxt)
  --   visitNesting {suc n} addr (int a sh , cntxt) = 
  --     seek addr (sh , []) 
  --     >>= (λ { (leaf , c) → nothing ; 
  --              (node pd hsh , c) → just (pd , ((a , (hsh , c)) ∷ cntxt)) 
  --            })

  --   seekNesting : {n : ℕ} → {A : Set} → Address (suc n) → ZipperNst n A → Maybe (ZipperNst n A)
  --   seekNesting [] z = just z
  --   seekNesting (d ∷ ds) z = seekNesting ds z >>= visitNesting d

  -- sibling : {n : ℕ} → {A : Set} → Address n → ZipperNst (suc n) A → Maybe (ZipperNst (suc n) A)
  -- sibling dir (fcs , []) = nothing
  -- sibling {zero} dir (fcs , (a , pt leaf , hcn) ∷ cn) = nothing
  -- sibling {zero} dir (fcs , (a , pt (node nfcs shell) , hcn) ∷ cn) = 
  --   just (nfcs , ((a , (shell , ((fcs , tt) ∷ hcn))) ∷ cn))
  -- sibling {suc n} dir (fcs , (a , verts , hcn) ∷ cn) = 
  --   seek dir (verts , []) 
  --   >>= (λ { (leaf , vcn) → nothing ; 
  --            (node leaf shell , vcn) → nothing ; 
  --            (node (node nfcs vrem) hmask , vcn) → 
  --              just (nfcs , (a , (vrem , (fcs , (hmask , vcn)) ∷ hcn)) ∷ cn) })

  -- predecessor : {n : ℕ} → {A : Set} → ZipperNst n A → Maybe (ZipperNst n A)
  -- predecessor {zero} _ = nothing
  -- predecessor {suc n} (fcs , []) = nothing
  -- predecessor {suc n} (fcs , (a , verts , []) ∷ cs) = nothing
  -- predecessor {suc n} (fcs , (a , verts , (pred , ∂) ∷ vs) ∷ cs) = just (pred , (a , ∂ ← node fcs verts , vs) ∷ cs)

  -- predecessorWhich : {n : ℕ} → {A : Set} → (A → Bool) → ZipperNst n A → Maybe (ZipperNst n A)
  -- predecessorWhich {zero} p z = if p (baseValue (proj₁ z)) then just z else nothing
  -- predecessorWhich {suc n} p z = 
  --   if p (baseValue (proj₁ z)) 
  --   then just z 
  --   else (predecessor z >>= (λ pred → predecessorWhich p pred))

  -- seekToNesting : {n : ℕ} → {A : Set} → Address (suc n) → Nesting n A → Maybe (ZipperNst n A)
  -- seekToNesting addr nst = seekNesting addr (nst , [])

