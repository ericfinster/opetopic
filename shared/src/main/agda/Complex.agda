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

  -- Now, I would like to implement something like complex grafting.  How is this going to
  -- work? Eventually, you should have a tree of complexes.  But of course this decomposes
  -- into steps.

  -- The inductive step is that you are looking at an (n+1)-complex.  You then have an n-tree
  -- of (n+1)-complexes.  The assumption, of course is that the target of each of these complexes
  -- agrees with the source to which it is attached.  Now, that source is exactly the canopy of
  -- the box one dimension below.

  -- Right.  You see, the problem is what is the result.  It's not really going to be a new complex,
  -- unless you provide extra data for the filler and the new target, which of course is indeed 
  -- possible.  But the intermediate steps should be .... well, almost stupid if everything is well
  -- formed.

  -- Indeed.  If you perform no compatibility checks, the you actually just *throw away* the higher
  -- codimension crap.  Is this really true?

  -- module ComplexGrafting {M : Set → Set} ⦃ isE : MonadError M ⦄ where
  
  --   open MonadError ⦃ ... ⦄

  --   complexGraft : {A : ℕ → Set} → {n : ℕ} → Tree (Complex A (suc n)) (suc n) → M (Complex A n × Tree (Nesting (A (suc n)) (suc n)) (suc n))
  --   complexGraft leaf = {!!}
  --   complexGraft (node c pd) = {!!}

  --   -- Mmmm.  So the totally naive thing doesn't work, because you don't have the lower dimensional information when you get to the leaves.
  --   -- You need something to "send back" as it were.  Like there is a sort of environment consisting of the lower dimensional information.

  --   -- Roughly, the idea would be that the thing you send back is that face.

  --   graftInContext : {A : ℕ → Set} → {n : ℕ} → Complex A n → Tree (Complex A (suc n)) (suc n) → M (Complex A n × Tree (Nesting (A (suc n)) (suc n)) (suc n))
  --   graftInContext c₀ leaf = η (c₀ , leaf)
  --   graftInContext c₀ (node (∥ ▶ obj a₀ ▶ dot a₁) pd) = failWith "Malformed pasting diagram"
  --   graftInContext c₀ (node (c₁ ▶ dot a₁ ▶ dot a₂) pd) = failWith "Malformed pasting diagram"
  --   graftInContext c₀ (node (_ ▶ box _ _) _) = failWith "Malformed Pasting diagram"
  --   graftInContext c₀ (node (c₁ ▶ box a₀ cn ▶ dot a₁) sh) = 
  --     traverseTree {{monadIsApp isMonad}} sh (λ pd → graftInContext {!!} pd) 
  --     >>= (λ zt → let (cmplxTr , newSh) = unzip zt    
  --                   in match (λ { nst cmplx → {!head cmplx!} }) cn cmplxTr 
  --     >>= {!!} )


    -- Umm-hmm. The deal is that something needs to be zipped with the canopy of the box.  If the complex
    -- is well formed, Then this contains a shitload of dots.  These dots should be replaced with boxes.  
    -- The question is: which boxes?

    -- Well shite.  As expected, it's not totally clear what to do here, even if basically you know what you want.
    -- Let's move on to expressions and come back to this when we think we can use it.
