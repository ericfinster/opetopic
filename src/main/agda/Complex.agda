--
--  Complex.agda - Complexes
--

{-# OPTIONS --no-termination-check #-}

open import Prelude
open import Mtl
open import Tree
open import Nesting
-- open import Suite

module Complex where

  -- Complex : Set → ℕ → Set
  -- Complex A n = Suite (λ k → Nesting k A) (suc n)

  -- ComplexZipper : Set → ℕ → Set
  -- ComplexZipper A n = Suite (λ k → ZipperNst k A) (suc n)

  -- NComplex : Set → Set
  -- NComplex A = Σ[ n ∈ ℕ ] Complex A n

  -- toZipper : {n : ℕ} → {A : Set} → Complex A n → ComplexZipper A n
  -- toZipper {zero} (∥ ▶ bs) = ∥ ▶ (bs , [])
  -- toZipper {suc n} (tl ▶ hd) = toZipper tl ▶ (hd , [])

  -- seal : {n : ℕ} → {A : Set} → ComplexZipper A n → Complex A n
  -- seal {zero} (∥ ▶ (nst , cntx)) = ∥ ▶ (close cntx nst)
  -- seal {suc n} (cz ▶ (nst , cntx)) = seal cz ▶ close cntx nst

  -- updateFocus : {n : ℕ} → {A : Set} → ComplexZipper A n → Nesting n A → ComplexZipper A n
  -- updateFocus {zero} (∥ ▶ (_ , cntx)) nst = ∥ ▶ (nst , cntx)
  -- updateFocus {suc n} (c ▶ (_ , cntx)) nst = c ▶ (nst , cntx) 

  -- focusValue : {n : ℕ} → {A : Set} → ComplexZipper A n → A
  -- focusValue c = baseValue (proj₁ (head c))

  -- focusDeriv : {n : ℕ} → {A : Set} → ComplexZipper A n → Maybe (Derivative (suc n) A)
  -- focusDeriv (zc ▶ (obj a , cntxt)) = just (pt leaf , [])
  -- focusDeriv (zc ▶ (ext a , cntxt)) = nothing
  -- focusDeriv (zc ▶ (int a cn , cntxt)) = just (const leaf cn , [])

  -- focusSpine : {n : ℕ} → {A : Set} → ComplexZipper A n → Maybe (Tree n A)
  -- focusSpine (∥ ▶ (obj a , cntx)) = just (pt a)
  -- focusSpine (zc ▶ (ext a , cntx)) = focusDeriv zc >>= (λ ∂ → just (∂ ← a))
  -- focusSpine (zc ▶ (int a cn , cntx)) = spineFromCanopy cn

  -- focusCanopy : {n : ℕ} → {A : Set} → ComplexZipper A n → Maybe (Tree n (Address n))
  -- focusCanopy {zero} (∥ ▶ _) = just (pt [])
  -- focusCanopy {suc n} (zc ▶ (ext a , cntx)) = nothing
  -- focusCanopy {suc n} (zc ▶ (int a cn , cntx)) = just (addrTree cn)

  -- focusUnit : {n : ℕ} → {A : Set} → ComplexZipper A n → Maybe (Tree n (Nesting n A))
  -- focusUnit {zero} zc = just (pt (proj₁ (head zc)))
  -- focusUnit {suc n} zc = 
  --   let fcs = proj₁ (head zc) in 
  --   focusSpine zc 
  --   >>= (λ { leaf → focusUnit (tail zc) >>= (λ u → just (node (proj₁ (head zc)) (const leaf u))) ; 
  --            (node a sh) → shellExtents sh >>= (λ extents → just (node fcs (const leaf extents))) })

  -- mutual

  --   visitComplex : {n : ℕ} → {A : Set} → ComplexZipper A n → Address n → Maybe (ComplexZipper A n)
  --   visitComplex {zero} (∥ ▶ bs) addr = visitNesting addr bs >>= (λ r → just (∥ ▶ r))
  --   visitComplex {suc n} (tl ▶ hd) [] = visitNesting [] hd >>= (λ r → just (tl ▶ r))
  --   visitComplex {suc n} zc (d ∷ ds) =
  --     visitComplex {suc n} zc ds 
  --     >>= (λ zp → sibling d (head zp) 
  --     >>= (λ sz → focusSpine zp 
  --     >>= (λ { leaf → just (tail zp ▶ sz) ;   -- The recusive call has left us on a drop.  The lower dimensions should already be set.
  --              (node a sh) → shellExtents sh  -- The extents give us all the options for where we might have to go ...
  --                            >>= (λ extents → extents valueAt d  -- ... and we choose the one for the direction we want
  --                            >>= (λ recAddr → seekComplex {n} (tail zp) recAddr 
  --                            >>= (λ rz → just (rz ▶ sz)))) 
  --            })))

  --   seekComplex : {n : ℕ} → {A : Set} → ComplexZipper A n → Address (suc n) → Maybe (ComplexZipper A n)
  --   seekComplex {n} zc [] = just zc
  --   seekComplex {n} zc (d ∷ ds) = seekComplex {n} zc ds >>= (λ zc₀ → visitComplex {n} zc₀ d)

  -- SourceM : (n : ℕ) → Set → Set → Set
  -- SourceM n A = StateT Maybe (Complex A n)

  -- SourceMN : (n : ℕ) → (A : Set) → Monad (SourceM n A)
  -- SourceMN n A = stateTM maybeM

  -- SourceMS : (n : ℕ) → (A : Set) → MonadState (SourceM n A) (Complex A n)
  -- SourceMS n A = stateTS maybeM

  -- SourceAP : (n : ℕ) → (A : Set) → Applicative (SourceM n A)
  -- SourceAP n A = monadIsApp (SourceMN n A)

  -- liftS : {n : ℕ} → {A : Set} → {B : Set} → Maybe B → SourceM n A B
  -- liftS nothing = toS (λ c → nothing)
  -- liftS (just b) = toS (λ c → just (c , b))

  -- mutual

  --   sourceAt : {n : ℕ} → {A : Set} → Complex A n → Address (suc n) → Maybe (Complex A n)
  --   sourceAt {n} c addr = restrictAt {n} c addr >>= (λ c₀ → contractAt {n} c₀ [])

  --   restrictAt : {n : ℕ} → {A : Set} → Complex A n → Address (suc n) → Maybe (Complex A n)
  --   restrictAt {n} c addr = 
  --     seekComplex {n} (toZipper {n} c) addr 
  --     >>= (λ zc → restrictFocus {n} zc 
  --     >>= (λ zc₀ → just (seal {n} zc₀)))

  --   contractAt : {n : ℕ} → {A : Set} → Complex A n → Address (suc n) → Maybe (Complex A n)
  --   contractAt {n} c addr =
  --     seekComplex {n} (toZipper {n} c) addr 
  --     >>= (λ zc → contractFocus {n} zc 
  --     >>= (λ zc₀ → just (seal {n} zc₀)))

  --   restrictFocus : {n : ℕ} → {A : Set} → ComplexZipper A n → Maybe (ComplexZipper A n)
  --   restrictFocus {zero} (∥ ▶ (pd , cn)) = just (∥ ▶ (pd , []))
  --   restrictFocus {suc n} zc = 
  --     focusSpine zc            
  --     >>= (λ fpd → restrictFocus {n} (tail zc)
  --     >>= (λ zc₀ → eval-stateT maybeF (seal {n} zc₀) (exciseLocal [] fpd) 
  --     >>= (λ nc → just (toZipper {n} nc ▶ (proj₁ (head zc) , [])) )))

  --   contractFocus : {n : ℕ} → {A : Set} → ComplexZipper A n → Maybe (ComplexZipper A n)
  --   contractFocus {zero} (∥ ▶ (pd , cn)) = just (∥ ▶ (obj (baseValue pd) , cn))
  --   contractFocus {suc n} zc = 
  --     focusSpine zc
  --     >>= (λ fpd → compressFocus (tail zc) fpd 
  --     >>= (λ zc₀ → just (zc₀ ▶ (ext (baseValue (proj₁ (head zc))) , (proj₂ (head zc))))) )

  --   compressFocus : {n : ℕ} → {A : Set} → ComplexZipper A n → Tree (suc n) A → Maybe (ComplexZipper A n)
  --   compressFocus {n} zc tr = compressLocal {n} zc tr >>= (λ pd → just (updateFocus {n} zc (int (focusValue {n} zc) pd)))

  --   compressLocal : {n : ℕ} → {A : Set} → ComplexZipper A n → Tree (suc n) A → Maybe (Tree n (Nesting n A))
  --   compressLocal {n} zc leaf = focusUnit zc
  --   compressLocal {n} zc (node a sh) = 
  --     focusCanopy {n} zc 
  --     >>= (λ cp → zipComplete cp sh
  --     >>= (λ zsh → traverseTree maybeA 
  --            (λ { (d , tr) → visitComplex {n} zc d 
  --                            >>= (λ zc₀ → compressLocal zc₀ tr) 
  --            }) zsh >>= join))

  --   exciseLocal : {n : ℕ} → {A : Set} → Address (suc n) → Tree (suc n) A → SourceM n A ⊤
  --   exciseLocal {n} {A} addr leaf = 
  --     get >>=ₛ (λ c → liftS {n} (contractAt {n} c addr) >>=ₛ put) 
  --     where open MonadState (SourceMS n A) renaming (_>>=_ to _>>=ₛ_)
  --   exciseLocal {n} {A} addr (node a shell) =
  --     traverseTree (SourceAP n A) (λ { (d , t) → exciseLocal (d ∷ addr) t }) (zipWithAddress shell)
  --     >>=ₛ (λ _ → η tt)
  --     where open MonadState (SourceMS n A) renaming (_>>=_ to _>>=ₛ_) 

  -- -- comultiply : {n : ℕ} → {A : Set} → Complex A n → Maybe (Complex (NComplex A) n)
  -- -- comultiply {zero} (∥ ▶ (obj ob)) = just (∥ ▶ (obj (0 , (∥ ▶ (obj ob)))))
  -- -- comultiply {zero} (∥ ▶ (int a (pt nst))) = 
  -- --   comultiply (∥ ▶ nst) >>= (λ { (∥ ▶ res) → just (∥ ▶ (int (0 , (∥ ▶ (obj a))) (pt res))) }) 
  -- -- comultiply {suc n} (ic ▶ nst) = 
  -- --   traverseNesting maybeA (λ { (_ , addr) → (sourceAt (ic ▶ nst) addr >>= (λ cm → just (suc n , cm))) }) (nestingWithAddr nst) 
  -- --   >>= (λ hd → comultiply ic 
  -- --   >>= (λ tl → just (tl ▶ hd) )) 


