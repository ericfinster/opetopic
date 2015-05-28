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

  open Monad errorM

  focusValue : {A : ℕ → Set} → {n : ℕ} → ComplexZipper A n → A n
  focusValue c = baseValue (focusOf c)

  focusDeriv : {A : ℕ → Set} → {n : ℕ} → ComplexZipper A n → Error (Derivative (A (suc n)) (suc n))
  focusDeriv (z ▶ (obj a , cntxt)) = succeed (pt leaf , [])
  focusDeriv (z ▶ (dot a , cntxt)) = fail "Focus Derivative fail"
  focusDeriv (z ▶ (box a cn , cntxt)) = succeed (const cn leaf , [])

  focusSpine : {A : ℕ → Set} → {n : ℕ} → ComplexZipper A n → Error (Tree (A n) n)
  focusSpine (z ▶ (obj a , cntxt)) = succeed (pt a)
  focusSpine (z ▶ (dot a , cntxt)) = focusDeriv z >>= (λ ∂ → succeed (∂ ← a))
  focusSpine (z ▶ (box a cn , cntxt)) = spineFromCanopy cn

  focusCanopy : {A : ℕ → Set} → {n : ℕ} → ComplexZipper A n → Error (Tree (Address n) n)
  focusCanopy (z ▶ (obj a , cntxt)) = succeed (pt tt)
  focusCanopy (z ▶ (dot a , cntxt)) = fail "No canopy for dot"
  focusCanopy (z ▶ (box a cn , cntxt)) = succeed (mapWithAddress cn (λ _ addr → addr))

  focusUnit : {A : ℕ → Set} → {n : ℕ} → ComplexZipper A n → Error (Tree (Nesting (A n) n) n)
  focusUnit {n = zero} z = succeed (pt (proj₁ (head z)))
  focusUnit {n = suc n} z = 
    focusSpine z >>= (λ { leaf → focusUnit (tail z) >>= (λ u → succeed (node (focusOf z) (const u leaf))) ; 
                          (node a sh) → shellExtents sh >>= (λ extents → succeed (node (focusOf z) (const extents leaf))) 
                        })

  mutual

    visitComplex : {A : ℕ → Set} → {n : ℕ} → ComplexZipper A n → Address n → Error (ComplexZipper A n)
    visitComplex {n = zero} (∥ ▶ nst) tt = visitNesting nst tt >>= (λ z → succeed (∥ ▶ z))
    visitComplex {n = suc n} c [] = visitNesting (head c) [] >>= (λ z → succeed (tail c ▶ z))
    visitComplex {n = suc n} c (d ∷ ds) = 
      visitComplex c ds 
      >>= (λ z → sibling (head z) d 
      >>= (λ z₀ → focusSpine z 
      >>= (λ { leaf → succeed (tail z ▶ z₀) ;   -- The recusive call has left us on a drop.  The lower dimensions should already be set.
               (node a sh) → shellExtents sh    -- The extents give us all the options for where we might have to go ...
                             >>= (λ extents → valueAt extents d  -- ... and we choose the one for the direction we want
                             >>= (λ recAddr → seekComplex (tail z) recAddr 
                             >>= (λ tl → succeed (tl ▶ z₀)))) })))

    seekComplex : {A : ℕ → Set} → {n : ℕ} → ComplexZipper A n → Address (suc n) → Error (ComplexZipper A n)
    seekComplex z [] = succeed z
    seekComplex z (d ∷ ds) = 
      seekComplex z ds >>= (λ z₀ → visitComplex z₀ d)

  SourceM : (ℕ → Set) → ℕ → Set → Set
  SourceM A n = StateT Error (Complex A n)

  SourceMN : (A : ℕ → Set) → (n : ℕ) → Monad (SourceM A n)
  SourceMN A n = stateTM errorM

  SourceMS : (A : ℕ → Set) → (n : ℕ) → MonadState (SourceM A n) (Complex A n)
  SourceMS A n = stateTS errorM

  SourceAP : (A : ℕ → Set) → (n : ℕ) → Applicative (SourceM A n)
  SourceAP A n = monadIsApp (SourceMN A n)

  liftS : {A : ℕ → Set} → {n : ℕ} → {B : Set} → Error B → SourceM A n B
  liftS mb = toS (λ c → mb >>= (λ b → η (c , b)))

  SourceE : (A : ℕ → Set) → (n : ℕ) → MonadError (SourceM A n)
  SourceE A n = record { isMonad = SourceMN A n ; failWith = λ msg → liftS (fail msg) }

  mutual

    sourceAt : {A : ℕ → Set} → {n : ℕ} → Complex A n → Address (suc n) → Error (Complex A n)
    sourceAt c addr = restrictAt c addr >>= (λ c₀ → contractAt c₀ [])

    restrictAt : {A : ℕ → Set} → {n : ℕ} → Complex A n → Address (suc n) → Error (Complex A n)
    restrictAt c addr = 
      seekComplex (complexToZipper c) addr 
      >>= (λ z → restrictFocus z 
      >>= (λ z₀ → succeed (seal z₀)))

    contractAt : {A : ℕ → Set} → {n : ℕ} → Complex A n → Address (suc n) → Error (Complex A n)
    contractAt c addr = 
      seekComplex (complexToZipper c) addr 
      >>= (λ z → contractFocus z 
      >>= (λ z₀ → succeed (seal z₀)))

    restrictFocus : {A : ℕ → Set} → {n : ℕ} → ComplexZipper A n → Error (ComplexZipper A n)
    restrictFocus {n = zero} z = succeed (∥ ▶ (focusOf z , []))
    restrictFocus {n = suc n} z = 
      focusSpine z 
      >>= (λ tr → restrictFocus (tail z) 
      >>= (λ tl → eval-stateT isFunctor (seal tl) (exciseLocal [] tr) 
      >>= (λ c → succeed (complexToZipper c ▶ (focusOf z , [])))))

    contractFocus : {A : ℕ → Set} → {n : ℕ} → ComplexZipper A n → Error (ComplexZipper A n)
    contractFocus {n = zero} (∥ ▶ (fcs , cntxt)) = succeed (∥ ▶ (obj (baseValue fcs) , cntxt))
    contractFocus {n = suc n} z = 
      focusSpine z 
      >>= (λ tr → compressFocus (tail z) tr 
      >>= (λ tl → succeed (tl ▶ (dot (focusValue z) , contextOf z))))

    compressFocus : {A : ℕ → Set} → {n : ℕ} → ComplexZipper A n → Tree (A (suc n)) (suc n) → Error (ComplexZipper A n)
    compressFocus z tr = compressLocal z tr >>= (λ cn → succeed (updateFocus z (box (focusValue z) cn)))

    compressLocal : {A : ℕ → Set} → {n : ℕ} → ComplexZipper A n → Tree (A (suc n)) (suc n) → Error (Tree (Nesting (A n) n) n)
    compressLocal z leaf = focusUnit z
    compressLocal z (node a sh) = 
      focusCanopy z 
      >>= (λ cn → match ⦃ errorE ⦄ (λ d tr → visitComplex z d >>= (λ z₀ → compressLocal z₀ tr)) cn sh 
      >>= join)

    exciseLocal : {A : ℕ → Set} → {n : ℕ} → Address (suc n) → Tree (A (suc n)) (suc n) → SourceM A n ⊤
    exciseLocal {A} {n} addr leaf = 
      get >>=ₛ (λ c → liftS (contractAt c addr) >>=ₛ put)
      where open MonadState (SourceMS A n) renaming (_>>=_ to _>>=ₛ_)
    exciseLocal {A} {n} addr (node _ sh) = 
      traverseWithAddress {{SourceAP A n}} sh (λ t d → exciseLocal (d ∷ addr) t) >>=ₛ (λ _ → ηₛ tt)
      where open MonadState (SourceMS A n) renaming (_>>=_ to _>>=ₛ_ ; η to ηₛ)

  comultiply : {A : ℕ → Set} → {n : ℕ} → Complex A n → Error (Complex (Complex A) n)
  comultiply {n = zero} (∥ ▶ obj a) = succeed (∥ ▶ (obj (∥ ▶ (obj a))))
  comultiply {n = zero} (∥ ▶ box a (pt nst)) = 
    comultiply (∥ ▶ nst) >>= (λ { (∥ ▶ res) → succeed (∥ ▶ box (∥ ▶ obj a) (pt res)) })
  comultiply {n = suc n} c = 
    traverseNestingWithAddr ⦃ monadIsApp errorM ⦄ (head c) (λ _ addr → sourceAt c addr)
    >>= (λ hd → comultiply (tail c) 
    >>= (λ tl → succeed (tl ▶ hd)))

  module ComplexGrafting (A : ℕ → Set) (disc : (n : ℕ) → A n → A n → Error (A n)) where

    complexGraft : {n : ℕ} → Nesting (Complex A (suc n)) n → Error (Complex A n × Tree (A (suc n)) (suc n))
    complexGraft (obj c) = succeed (tail c , leaf)
    complexGraft (dot c) = succeed (tail c , leaf)
    complexGraft {zero} (box (∥ ▶ (box t₀ (pt (obj s₀))) ▶ dot a₀) (pt pd)) = 
      complexGraft pd >>= (λ { ((∥ ▶ outC) , arrStck) → disc _ s₀ (baseValue outC)
                      >>= (λ v₀ → succeed ((∥ ▶ (box t₀ (pt (withBase v₀ outC)))) , (node a₀ (pt arrStck)))) })
    complexGraft {zero} _ = fail "Malformed (arrow) complex"
    complexGraft {n = suc n} (box (c ▶ box t₀ cn₀ ▶ dot a₀) cn) = 
      traverseTree ⦃ monadIsApp errorM ⦄ cn complexGraft 
      >>= (λ toGraft → let cmplxSh , aSh = unzip toGraft 
                       in matchWithAddress ⦃ errorE ⦄ complexGraftLocal cn₀ cmplxSh 
                          >>= (λ prTr → let newCnpy , fillerTr = unzip prTr 
                                        in toNesting fillerTr (λ addr → seekNesting (head c , []) addr >>= (λ zp → succeed (proj₁ zp))) 
                                           >>= (λ nstNst → nestingJoin nstNst 
                                           >>= (λ cdim2 → succeed ((tail c ▶ cdim2 ▶ box t₀ newCnpy) , node a₀ aSh)))))

      where complexGraftLocal : Nesting (A (suc n)) (suc n) → Complex A (suc n) → Address (suc n) → 
                                Error (Nesting (A (suc n)) (suc n) × Nesting (A n) n)
            complexGraftLocal (dot s₀) (c₀ ▶ nst) addr = 
              disc _ s₀ (baseValue nst) >>= (λ v₀ → succeed (withBase v₀ nst , head c₀))
            complexGraftLocal _ _ addr = fail "Malformed complex"

    complexGraft {n = suc n} _ = fail "Malformed complex"

