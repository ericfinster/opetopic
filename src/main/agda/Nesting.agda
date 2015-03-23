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

  open Monad maybeM hiding (fmap ; η ; μ)

  data Nesting₀ (A : Set) : ℕ → Set where
    obj : (a : A) → Nesting₀ A 0
    ext : {n : ℕ} → (a : A) → Nesting₀ A (suc n)
    int : {n : ℕ} → (a : A) → (sh : Tree n (Nesting₀ A n)) → Nesting₀ A n
  
  Nesting : ℕ → Set → Set
  Nesting n A = Nesting₀ A n

  mapNesting : {n : ℕ} → {A B : Set} → (f : A → B) → Nesting n A → Nesting n B
  mapNesting {zero} f (obj a) = obj (f a)
  mapNesting {suc n} f (ext a) = ext (f a) 
  mapNesting {n} f (int a shell) = int (f a) (fmap (mapNesting f) shell)
    where open Functor (TreeF n)

  traverseNesting : {n : ℕ} → {A B : Set} → {G : Set → Set} → (isA : Applicative G) → (f : A → G B) → Nesting n A → G (Nesting n B)
  traverseNesting {zero} isA f (obj a) = let open Applicative isA in pure obj ⊛ f a
  traverseNesting {suc n} isA f (ext a) = let open Applicative isA in pure ext ⊛ f a 
  traverseNesting {n} isA f (int a shell) = pure int ⊛ f a ⊛ traverse isA (λ p → traverseNesting isA f p) shell
    where open Applicative isA
          open Traverse (TreeT n)

  NestingF : (n : ℕ) → Functor (Nesting n)
  NestingF n = record { fmap = mapNesting {n} }
  
  NestingT : (n : ℕ) → Traverse (Nesting n)
  NestingT n = record { isFunctor = NestingF n ; traverse = traverseNesting {n} }

  nestingWithPrefix : {n : ℕ} → {A : Set} → Address (suc n) → Nesting n A → Nesting n (A × Address (suc n))
  nestingWithPrefix {zero} pref (obj a) = obj (a , pref)
  nestingWithPrefix {suc n} pref (ext a) = ext (a , pref)
  nestingWithPrefix {n} pref (int a sh) = 
    int (a , pref) (mapTree (λ { (dir , nst) → nestingWithPrefix (dir ∷ pref) nst }) (zipWithAddress sh)) 

  nestingWithAddr : {n : ℕ} → {A : Set} → Nesting n A → Nesting n (A × Address (suc n))
  nestingWithAddr pd = nestingWithPrefix [] pd

  baseValue : {n : ℕ} → {A : Set} → Nesting n A → A
  baseValue (obj a) = a
  baseValue (ext a) = a
  baseValue (int a sh) = a

  toTree : {n : ℕ} → {A : Set} → Nesting n A → Tree (suc n) A
  toTree {zero} (obj a) = leaf
  toTree {zero} (int a (pt nst)) = node a (pt (toTree nst))
  toTree {suc n} (ext a) = leaf
  toTree {suc n} (int a sh) = node a (mapTree toTree sh)

  extendNesting : {n : ℕ} → {A B : Set} → B → Nesting n A → Tree (suc n) (Nesting (suc n) B)
  extendNesting b (obj a) = leaf
  extendNesting b (ext a) = leaf
  extendNesting b (int a sh) = node (ext b) (mapTree (extendNesting b) sh)

  mutual

    spineFromCanopy : {n : ℕ} → {A : Set} → Tree n (Nesting n A) → Maybe (Tree n A)
    spineFromCanopy {A = A} cn = traverseTree maybeA (uncurry spineFromDeriv) (zipWithDeriv {B = A} cn) >>= join

    spineFromDeriv : {n : ℕ} → {A : Set} → Derivative n A → Nesting n A → Maybe (Tree n A)
    spineFromDeriv ∂ (obj a) = just (pt a)
    spineFromDeriv ∂ (ext a) = just (∂ ← a)
    spineFromDeriv ∂ (int a cn) = spineFromCanopy cn

  -- extrude : {n : ℕ} → {A : Set} → Tree n (Nesting n A) → (A → Bool) → A → Maybe (Tree n (Nesting n A))
  -- extrude tr p dflt = 
  --   dejoin tr (λ nst → p (baseValue nst)) 
  --   >>= (λ tr₀ → just (mapTree (λ { (inj₁ nst) → nst 
  --                                 ; (inj₂ tr₁) → int dflt tr₁ }) tr₀))

  -- Right-o.  That looks pretty good actually.  Now, what's next?

  extrudeNesting : {n : ℕ} → {A B : Set} → A → Address n → Tree n (Nesting n A) → Tree n B → Maybe (Tree n (Nesting n A))
  extrudeNesting {zero} a addr tr msk = just (pt (int a tr))
  extrudeNesting {suc n} a addr tr msk = 
    seekTo addr tr 
    >>= (λ { (fcs , cntxt) → replace fcs msk (λ canopy → int a canopy) 
    >>= (λ newFcs → just (cntxt ↓ newFcs)) })

  mutual

    DerivativeNst : (n : ℕ) → Set → Set
    DerivativeNst n A = Tree n (Nesting n A) × ContextNst n A

    ContextNst : (n : ℕ) → Set → Set
    ContextNst n A = List (A × Derivative n (Nesting n A))

    ZipperNst : (n : ℕ) → Set → Set
    ZipperNst n A = Nesting n A × ContextNst n A

    plug : {n : ℕ} → {A : Set} → DerivativeNst n A → A → Nesting n A
    plug (t , c) a = close c (int a t)

    close : {n : ℕ} → {A : Set} → ContextNst n A → Nesting n A → Nesting n A
    close [] nst = nst
    close ((a , d) ∷ ds) nst = close ds (int a (d ← nst))

    visitNesting : {n : ℕ} → {A : Set} → Address n → ZipperNst n A → Maybe (ZipperNst n A)
    visitNesting addr (obj a , cntxt) = nothing
    visitNesting addr (ext a , cntxt) = nothing
    visitNesting {zero} [] (int h (pt t) , cntxt) = just (t , (h , tt) ∷ cntxt)
    visitNesting {zero} (() ∷ addr) (int a sh , cntxt)
    visitNesting {suc n} addr (int a sh , cntxt) = 
      seek addr (sh , []) 
      >>= (λ { (leaf , c) → nothing ; 
               (node pd hsh , c) → just (pd , ((a , (hsh , c)) ∷ cntxt)) 
             })

    seekNesting : {n : ℕ} → {A : Set} → Address (suc n) → ZipperNst n A → Maybe (ZipperNst n A)
    seekNesting [] z = just z
    seekNesting (d ∷ ds) z = seekNesting ds z >>= visitNesting d

    sibling : {n : ℕ} → {A : Set} → Address n → ZipperNst (suc n) A → Maybe (ZipperNst (suc n) A)
    sibling dir (fcs , []) = nothing
    sibling {zero} dir (fcs , (a , pt leaf , hcn) ∷ cn) = nothing
    sibling {zero} dir (fcs , (a , pt (node nfcs shell) , hcn) ∷ cn) = 
      just (nfcs , ((a , (shell , ((fcs , tt) ∷ hcn))) ∷ cn))
    sibling {suc n} dir (fcs , (a , verts , hcn) ∷ cn) = 
      seek dir (verts , []) 
      >>= (λ { (leaf , vcn) → nothing ; 
               (node leaf shell , vcn) → nothing ; 
               (node (node nfcs vrem) hmask , vcn) → 
                 just (nfcs , (a , (vrem , (fcs , (hmask , vcn)) ∷ hcn)) ∷ cn) })

  
  seekToNesting : {n : ℕ} → {A : Set} → Address (suc n) → Nesting n A → Maybe (ZipperNst n A)
  seekToNesting addr nst = seekNesting addr (nst , [])

  -- dualRecurse : {n : ℕ} → {A B C : Set} → Nesting n A → Nesting (suc n) B → (B → Tree n (Nesting n A) → C) → Maybe C
  -- dualRecurse (obj a) (ext b) f = {!!}
  -- dualRecurse (ext a) (ext b) f = nothing -- We must have a tree of leaves?
  -- dualRecurse (int a sh) (ext b) f = just (f b sh)
  -- dualRecurse n0 (int b cn) f = spineFromCanopy cn >>= (λ sp → {!zipComplete (toTree n0) sp!})

  -- Okay, so one solution is to not set the edges at all during the actual rendering pass except for the leaves.
  -- Then you use the spine from canopy idea to zip the two together and set edge values to the result.  Would this
  -- satisfy you?
