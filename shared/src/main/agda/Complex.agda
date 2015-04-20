--
--  Complex.agda - Complexes
--

{-# OPTIONS --no-termination-check #-}

open import Prelude
open import Mtl
open import Tree
open import Nesting
open import Suite

module Complex where

  Complex : (ℕ → Set) → ℕ → Set
  Complex A n = Suite (λ k → Nesting (A k) k) (suc n)

  ComplexZipper : (ℕ → Set) → ℕ → Set
  ComplexZipper A n = Suite (λ k → ZipperNst (A k) k) (suc n)

  complexToZipper : {A : ℕ → Set} → {n : ℕ} → Complex A n → ComplexZipper A n
  complexToZipper c = mapSuite c (λ k nst → nst , [])

  seal : {A : ℕ → Set} → {n : ℕ} → ComplexZipper A n → Complex A n
  seal z = mapSuite z (λ k nz → closeNesting (proj₂ nz) (proj₁ nz))

  updateFocus : {A : ℕ → Set} → {n : ℕ} → ComplexZipper A n → Nesting (A n) n → ComplexZipper A n
  updateFocus (tl ▶ hd) nst = tl ▶ (nst , proj₂ hd)

  focusOf : {A : ℕ → Set} → {n : ℕ} → ComplexZipper A n → Nesting (A n) n
  focusOf c = proj₁ (head c)

  contextOf : {A : ℕ → Set} → {n : ℕ} → ComplexZipper A n → ContextNst (A n) n
  contextOf c = proj₂ (head c)

  focusValue : {A : ℕ → Set} → {n : ℕ} → ComplexZipper A n → A n
  focusValue c = baseValue (focusOf c)

  module SourceCalculation {M : Set → Set} ⦃ isE : MonadError M ⦄ where

    open MonadError ⦃ ... ⦄
    
    focusDeriv : {A : ℕ → Set} → {n : ℕ} → ComplexZipper A n → M (Derivative (A (suc n)) (suc n))
    focusDeriv (z ▶ (obj a , cntxt)) = η (pt leaf , [])
    focusDeriv (z ▶ (dot a , cntxt)) = failWith "Focus Derivative fail"
    focusDeriv (z ▶ (box a cn , cntxt)) = η (const cn leaf , [])

    focusSpine : {A : ℕ → Set} → {n : ℕ} → ComplexZipper A n → M (Tree (A n) n)
    focusSpine (z ▶ (obj a , cntxt)) = η (pt a)
    focusSpine (z ▶ (dot a , cntxt)) = focusDeriv z >>= (λ ∂ → η (∂ ← a))
    focusSpine (z ▶ (box a cn , cntxt)) = spineFromCanopy cn

    focusCanopy : {A : ℕ → Set} → {n : ℕ} → ComplexZipper A n → M (Tree (Address n) n)
    focusCanopy (z ▶ (obj a , cntxt)) = η (pt tt)
    focusCanopy (z ▶ (dot a , cntxt)) = failWith "No canopy for dot"
    focusCanopy (z ▶ (box a cn , cntxt)) = η (mapWithAddress cn (λ _ addr → addr))

    focusUnit : {A : ℕ → Set} → {n : ℕ} → ComplexZipper A n → M (Tree (Nesting (A n) n) n)
    focusUnit {n = zero} z = η (pt (proj₁ (head z)))
    focusUnit {n = suc n} z = 
      focusSpine z >>= (λ { leaf → focusUnit (tail z) >>= (λ u → η (node (focusOf z) (const u leaf))) ; 
                            (node a sh) → shellExtents sh >>= (λ extents → η (node (focusOf z) (const extents leaf))) 
                          })

    mutual

      visitComplex : {A : ℕ → Set} → {n : ℕ} → ComplexZipper A n → Address n → M (ComplexZipper A n)
      visitComplex {n = zero} (∥ ▶ nst) tt = visitNesting nst tt >>= (λ z → η (∥ ▶ z))
      visitComplex {n = suc n} c [] = visitNesting (head c) [] >>= (λ z → η (tail c ▶ z))
      visitComplex {n = suc n} c (d ∷ ds) = 
        visitComplex c ds 
        >>= (λ z → sibling (head z) d 
        >>= (λ z₀ → focusSpine z 
        >>= (λ { leaf → η (tail z ▶ z₀) ;         -- The recusive call has left us on a drop.  The lower dimensions should already be set.
                 (node a sh) → shellExtents sh    -- The extents give us all the options for where we might have to go ...
                               >>= (λ extents → valueAt extents d  -- ... and we choose the one for the direction we want
                               >>= (λ recAddr → seekComplex (tail z) recAddr 
                               >>= (λ tl → η (tl ▶ z₀)))) })))
 
      seekComplex : {A : ℕ → Set} → {n : ℕ} → ComplexZipper A n → Address (suc n) → M (ComplexZipper A n)
      seekComplex z [] = η z
      seekComplex z (d ∷ ds) = 
        seekComplex z ds >>= (λ z₀ → visitComplex z₀ d)


    SourceM : (ℕ → Set) → ℕ → Set → Set
    SourceM A n = StateT M (Complex A n)

    SourceMN : (A : ℕ → Set) → (n : ℕ) → Monad (SourceM A n)
    SourceMN A n = stateTM isMonad

    SourceMS : (A : ℕ → Set) → (n : ℕ) → MonadState (SourceM A n) (Complex A n)
    SourceMS A n = stateTS isMonad

    SourceAP : (A : ℕ → Set) → (n : ℕ) → Applicative (SourceM A n)
    SourceAP A n = monadIsApp (SourceMN A n)

    liftS : {A : ℕ → Set} → {n : ℕ} → {B : Set} → M B → SourceM A n B
    liftS mb = toS (λ c → mb >>= (λ b → η (c , b)))

    mutual

      sourceAt : {A : ℕ → Set} → {n : ℕ} → Complex A n → Address (suc n) → M (Complex A n)
      sourceAt c addr = restrictAt c addr >>= (λ c₀ → contractAt c₀ [])

      restrictAt : {A : ℕ → Set} → {n : ℕ} → Complex A n → Address (suc n) → M (Complex A n)
      restrictAt c addr = 
        seekComplex (complexToZipper c) addr 
        >>= (λ z → restrictFocus z 
        >>= (λ z₀ → η (seal z₀)))

      contractAt : {A : ℕ → Set} → {n : ℕ} → Complex A n → Address (suc n) → M (Complex A n)
      contractAt c addr = 
        seekComplex (complexToZipper c) addr 
        >>= (λ z → contractFocus z 
        >>= (λ z₀ → η (seal z₀)))

      restrictFocus : {A : ℕ → Set} → {n : ℕ} → ComplexZipper A n → M (ComplexZipper A n)
      restrictFocus {n = zero} z = η (∥ ▶ (focusOf z , []))
      restrictFocus {n = suc n} z = 
        focusSpine z 
        >>= (λ tr → restrictFocus (tail z) 
        >>= (λ tl → eval-stateT isFunctor (seal tl) (exciseLocal [] tr) 
        >>= (λ c → η (complexToZipper c ▶ (focusOf z , [])))))

      contractFocus : {A : ℕ → Set} → {n : ℕ} → ComplexZipper A n → M (ComplexZipper A n)
      contractFocus {n = zero} (∥ ▶ (fcs , cntxt)) = η (∥ ▶ (obj (baseValue fcs) , cntxt))
      contractFocus {n = suc n} z = 
        focusSpine z 
        >>= (λ tr → compressFocus (tail z) tr 
        >>= (λ tl → η (tl ▶ (dot (focusValue z) , contextOf z))))

      compressFocus : {A : ℕ → Set} → {n : ℕ} → ComplexZipper A n → Tree (A (suc n)) (suc n) → M (ComplexZipper A n)
      compressFocus z tr = compressLocal z tr >>= (λ cn → η (updateFocus z (box (focusValue z) cn)))

      compressLocal : {A : ℕ → Set} → {n : ℕ} → ComplexZipper A n → Tree (A (suc n)) (suc n) → M (Tree (Nesting (A n) n) n)
      compressLocal z leaf = focusUnit z
      compressLocal z (node a sh) = 
        focusCanopy z 
        >>= (λ cn → match (λ d tr → visitComplex z d >>= (λ z₀ → compressLocal z₀ tr)) cn sh 
        >>= join)

      exciseLocal : {A : ℕ → Set} → {n : ℕ} → Address (suc n) → Tree (A (suc n)) (suc n) → SourceM A n ⊤
      exciseLocal {A} {n} addr leaf = 
        get >>=ₛ (λ c → liftS (contractAt c addr) >>=ₛ put)
        where open MonadState (SourceMS A n) renaming (_>>=_ to _>>=ₛ_)
      exciseLocal {A} {n} addr (node _ sh) = 
        traverseWithAddress {{SourceAP A n}} sh (λ t d → exciseLocal (d ∷ addr) t) >>=ₛ (λ _ → ηₛ tt)
        where open MonadState (SourceMS A n) renaming (_>>=_ to _>>=ₛ_ ; η to ηₛ)

    comultiply : {A : ℕ → Set} → {n : ℕ} → Complex A n → M (Complex (Complex A) n)
    comultiply {n = zero} (∥ ▶ obj a) = η (∥ ▶ (obj (∥ ▶ (obj a))))
    comultiply {n = zero} (∥ ▶ box a (pt nst)) = 
      comultiply (∥ ▶ nst) >>= (λ { (∥ ▶ res) → η (∥ ▶ box (∥ ▶ obj a) (pt res)) })
    comultiply {n = suc n} c = 
      traverseNestingWithAddr {{monadIsApp isMonad}} (head c) (λ _ addr → sourceAt c addr)
      >>= (λ hd → comultiply (tail c) 
      >>= (λ tl → η (tl ▶ hd)))


  testComplex : Complex (λ k → ℕ) 1
  testComplex = ∥ ▶ box 0 (pt (box 1 (pt (obj 2)))) ▶ box 3 (node (dot 4) (pt (node (dot 5) (pt leaf))))

  testSeek : Maybe (ComplexZipper (λ k → ℕ) 1)
  testSeek = seekComplex (complexToZipper testComplex) ((tt ∷ []) ∷ [])
    where open SourceCalculation ⦃ maybeE ⦄

  testSource : Maybe (Complex (λ k → ℕ) 1)
  testSource = sourceAt testComplex ((tt ∷ []) ∷ [])
    where open SourceCalculation ⦃ maybeE ⦄
    
  testCanopy : Tree (Nesting ℕ 2) 2
  testCanopy = node (dot 6) (node (node (dot 4) leaf) (pt (node (node (dot 2) leaf) (pt leaf))))

  testGraft : Maybe (Tree (Nesting ℕ 2) 2)
  testGraft = graft ⦃ maybeE ⦄ testCanopy leaf
