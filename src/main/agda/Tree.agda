--
--  Tree.agda - Higher Dimensional Trees
--
--  Eric Finster
--

{-# OPTIONS --no-termination-check #-}

open import Prelude
open import Mtl

module Tree where

  data Tree (A : Set) : ℕ → Set where
    pt : (a : A) → Tree A 0
    leaf : {n : ℕ} → Tree A (suc n)
    node : {n : ℕ} → (a : A) → (sh : Tree (Tree A (suc n)) n) → Tree A (suc n)

  Address : (n : ℕ) → Set
  Address zero = ⊤
  Address (suc n) = List (Address n)

  rootAddr : (n : ℕ) → Address n
  rootAddr zero = tt
  rootAddr (suc n) = []

  mutual 

    Derivative : Set → ℕ → Set
    Derivative A zero = ⊤
    Derivative A (suc n) = Tree (Tree A (suc n)) n × Context A (suc n)

    Context : Set → ℕ → Set
    Context A zero = ⊤
    Context A (suc n) = List (A × Derivative (Tree A (suc n)) n)

    Zipper : Set → ℕ → Set
    Zipper A n = Tree A n × Context A n

    _←_ : {A : Set} → {n : ℕ} → Derivative A n → A → Tree A n
    _←_ {n = zero} tt a = pt a
    _←_ {n = suc n} (sh , context) a = context ↓ node a sh

    _↓_ : {A : Set} → {n : ℕ} → Context A n → Tree A n → Tree A n
    _↓_ {n = zero} tt t = t
    _↓_ {n = suc n} [] t = t
    _↓_ {n = suc n} ((a , d) ∷ c) t = c ↓ node a (d ← t)

    visit : {M : Set → Set} → ⦃ isE : MonadError M ⦄ → {A : Set} → {n : ℕ} → Address n → Zipper A (suc n) → M (Zipper A (suc n))
    visit {n = zero} tt (leaf , cntxt) = let open MonadError ⦃ ... ⦄ in failWith "Visit fail"
    visit {n = zero} tt (node hd (pt tl) , cntxt) = let open MonadError ⦃ ... ⦄ in η (tl , ((hd , tt) ∷ cntxt))
    visit {n = suc n} d (leaf , cntxt) = let open MonadError ⦃ ... ⦄ in failWith "Visit fail"
    visit {n = suc n} d (node a sh , cntxt) = 
      let open MonadError ⦃ ... ⦄ 
      in seek (sh , []) d
         >>= (λ { (leaf , cntxt₀) → failWith "Visit fail" ; 
                  (node tr hsh , cntxt₀) → η (tr , (a , hsh , cntxt₀) ∷ cntxt) })

    seek : {M : Set → Set} → ⦃ isE : MonadError M ⦄ → {A : Set} → {n : ℕ} → Zipper A (suc n) → Address (suc n) → M (Zipper A (suc n))
    seek z [] = let open MonadError ⦃ ... ⦄ in η z
    seek z (d ∷ ds) = let open MonadError ⦃ ... ⦄ in seek z ds >>= visit d

    parent : {M : Set → Set} → ⦃ isE : MonadError M ⦄ → {A : Set} → {n : ℕ} → Zipper A n → M (Zipper A n)
    parent {n = zero} z = let open MonadError ⦃ ... ⦄ in failWith "No parent"
    parent {n = suc n} (fcs , []) = let open MonadError ⦃ ... ⦄ in failWith "No parent"
    parent {n = suc n} (fcs , (a , ∂) ∷ cs) = η (node a (∂ ← fcs) , cs)
      where open MonadError ⦃ ... ⦄

    parentWhich : {M : Set → Set} → ⦃ isE : MonadError M ⦄ → {A : Set} → {n : ℕ} → Zipper A n → (A → Bool) → M (Zipper A n)
    parentWhich {n = zero} z p = let open MonadError ⦃ ... ⦄ in failWith "No parent"
    parentWhich {n = suc n} (fcs , []) p = let open MonadError ⦃ ... ⦄ in failWith "No parent"
    parentWhich {n = suc n} (fcs , (a , ∂) ∷ cs) p = 
      let parent = (node a (∂ ← fcs) , cs)
      in if p a then η parent else parentWhich parent p
      where open MonadError ⦃ ... ⦄
  
  globDerivative : (A : Set) → (n : ℕ) → Derivative A n
  globDerivative A zero = tt
  globDerivative A (suc n) = (globDerivative (Tree A (suc n)) n ← leaf) , []

  seekTo : {M : Set → Set} → ⦃ isE : MonadError M ⦄ → {A : Set} → {n : ℕ} → Tree A n → Address n → M (Zipper A n)
  seekTo {n = zero} tr tt = let open MonadError ⦃ ... ⦄ in η (tr , tt)
  seekTo {n = suc n} tr addr = let open MonadError ⦃ ... ⦄ in seek (tr , []) addr 

  rootValue : {M : Set → Set} → ⦃ isE : MonadError M ⦄ → {A : Set} → {n : ℕ} → Tree A n → M A
  rootValue (pt a) = let open MonadError ⦃ ... ⦄ in η a
  rootValue leaf = let open MonadError ⦃ ... ⦄ in failWith "Root exception"
  rootValue (node a sh) = let open MonadError ⦃ ... ⦄ in η a

  valueAt : {M : Set → Set} → ⦃ isE : MonadError M ⦄ → {A : Set} → {n : ℕ} → Tree A n → Address n → M A
  valueAt tr addr = 
    let open MonadError ⦃ ... ⦄
    in seekTo tr addr >>= (λ z → rootValue (proj₁ z))

  traverseTree : {G : Set → Set} → ⦃ isA : Applicative G ⦄ → {A B : Set} → {n : ℕ} → Tree A n → (A → G B) → G (Tree B n)
  traverseTree (pt a) f = let open Applicative ⦃ ... ⦄ in pure pt ⊛ f a
  traverseTree leaf f = let open Applicative ⦃ ... ⦄ in pure leaf
  traverseTree (node a sh) f = let open Applicative ⦃ ... ⦄ in 
    pure node ⊛ f a ⊛ traverseTree sh (λ b → traverseTree b f)

  traverseWithAddress : {G : Set → Set} → ⦃ isA : Applicative G ⦄ → {A B : Set} → {n : ℕ} → Tree A n → (A → Address n → G B) → G (Tree B n)
  traverseWithAddress {G = G} tr f = traverseWithAddress₀ tr (rootAddr _) f

    where open Applicative ⦃ ... ⦄ 

          traverseWithAddress₀ : {A B : Set} → {n : ℕ} → Tree A n → Address n → (A → Address n → G B) → G (Tree B n)
          traverseWithAddress₀ (pt a) base f = pure pt ⊛ f a base
          traverseWithAddress₀ leaf base f = pure leaf
          traverseWithAddress₀ (node a sh) base f = 
            pure node ⊛ f a base ⊛ traverseWithAddress sh (λ b d → traverseWithAddress₀ b (d ∷ base) f)

  mapTree : {A B : Set} → {n : ℕ} → Tree A n → (A → B) → Tree B n
  mapTree tr f = traverseTree ⦃ idA ⦄ tr f

  mapWithAddress : {A B : Set} → {n : ℕ} → Tree A n → (A → Address n → B) → Tree B n
  mapWithAddress tr f = traverseWithAddress ⦃ idA ⦄ tr f

  const : {A B : Set} → {n : ℕ} → Tree A n → B → Tree B n
  const tr b = mapTree tr (λ _ → b)

  unzip : {n : ℕ} → {A B : Set} → Tree (A × B) n → (Tree A n × Tree B n)
  unzip (pt (a , b)) = (pt a , pt b)
  unzip leaf = leaf , leaf
  unzip (node (a , b) shAB) with unzip (mapTree shAB unzip)
  unzip (node (a , b) shAB) | (shA , shB) = (node a shA , node b shB)

  traverseWithLocalData : {G : Set → Set} → ⦃ isA : Applicative G ⦄ → {A B C : Set} → {n : ℕ} → 
                          Tree A n → (A → Address n → Derivative B n → G C) → G (Tree C n)
  traverseWithLocalData {G = G} tr f = traverseWithLocalData₀ tr (rootAddr _) f

    where open Applicative ⦃ ... ⦄

          traverseWithLocalData₀ : {A B C : Set} → {n : ℕ} → Tree A n → Address n → (A → Address n → Derivative B n → G C) → G (Tree C n)
          traverseWithLocalData₀ (pt a) base f = pure pt ⊛ f a base tt
          traverseWithLocalData₀ leaf base f = pure leaf
          traverseWithLocalData₀ (node a sh) base f = 
            pure node ⊛ f a base (const sh leaf , []) ⊛ traverseWithAddress sh (λ b d → traverseWithLocalData₀ b (d ∷ base) f)
  
  module Matching {M : Set → Set} ⦃ isE : MonadError M ⦄ where

    open MonadError ⦃ ... ⦄
    open Applicative (monadIsApp isMonad)

    match : {A B C : Set} → {n : ℕ} → (A → B → M C) → Tree A n → Tree B n → M (Tree C n)
    match φ (pt a) (pt b) = pure pt ⊛ φ a b 
    match φ leaf leaf = pure leaf
    match φ leaf (node b shB) = failWith "Match error"
    match φ (node a shA) leaf = failWith "Match error"
    match φ (node a shA) (node b shB) = 
      pure node ⊛ φ a b ⊛ match (match φ) shA shB

    matchWithDerivative : {A B C D : Set} → {n : ℕ} → (A → B → Derivative C n → M D) → 
                          Tree A n → Tree B n → M (Tree D n)
    matchWithDerivative φ (pt a) (pt b) = pure pt ⊛ φ a b tt
    matchWithDerivative φ leaf leaf = pure leaf
    matchWithDerivative φ leaf (node b shB) = failWith "Match error"
    matchWithDerivative φ (node a shA) leaf = failWith "Match error"
    matchWithDerivative φ (node a shA) (node b shB) = 
      pure node ⊛ φ a b (const shB leaf , []) ⊛ match (matchWithDerivative φ) shA shB

  open Matching public

  join : {M : Set → Set} → ⦃ isE : MonadError M ⦄ → {A : Set} → {n : ℕ} → Tree (Tree A n) n → M (Tree A n)

  module GraftRec {M : Set → Set} ⦃ isE : MonadError M ⦄ {A B : Set} where

    open MonadError ⦃ ... ⦄ public

    instance apM : Applicative M
             apM = monadIsApp isMonad

    module PositiveDim {n : ℕ} (ν-rec : (A → Tree B (suc n) → M B)) (λ-rec : (Address (suc n) → M B)) where

      unzipAndJoin : Tree (Tree B (suc n) × Tree (Address (suc n)) n) n → M (Tree (Tree B (suc n)) n × Tree (Address (suc n)) n)
      unzipAndJoin zt with unzip zt
      unzipAndJoin zt | (bSh , adrJnSh) = join adrJnSh >>= (λ adTr → η (bSh , adTr))

      unzipJoinAndAppend : Tree (Tree B (suc n) × Tree (Address (suc n)) n) n → M B → M (Tree B (suc n) × Tree (Address (suc n)) n)
      unzipJoinAndAppend zt mb = 
        unzipAndJoin zt >>= (λ { (bSh , adTr) → 
        mb >>= (λ b → η (node b bSh , adTr)) })

      horizontalPass : Address (suc n) → Tree (Tree A (suc (suc n))) (suc n) → Derivative (Address (suc n)) n → M (Tree B (suc n) × Tree (Address (suc n)) n)
      horizontalPass base leaf ∂ = η (leaf , ∂ ← base)
      horizontalPass base (node leaf hsh) ∂ = 
        traverseWithLocalData hsh (λ hbr d ∂₀ → horizontalPass (d ∷ base) hbr ∂₀) 
        >>= (λ res → unzipJoinAndAppend res (λ-rec base))
      horizontalPass base (node (node a vsh) hsh) ∂ = 
        horizontalPass base vsh ∂ 
        >>= (λ { (bTr , adTr₀) → matchWithDerivative {C = Address (suc n)} horizontalPass adTr₀ hsh 
        >>= (λ res → unzipJoinAndAppend res (ν-rec a bTr)) })

      initHorizontal : A → Tree (Tree (Tree A (suc (suc n))) (suc n)) n → M (B × Tree (Address (suc n)) n) → M (B × Tree (Address (suc n)) n)
      initHorizontal a hsh m = 
        m >>= (λ { (b₀ , adTr₀) → matchWithDerivative {C = Address (suc n)} horizontalPass adTr₀ hsh 
          >>= (λ res → unzipAndJoin res 
          >>= (λ { (bSh , adTr) → ν-rec a (node b₀ bSh) 
          >>= (λ b → η (b , adTr)) })) })

      initVertial : A → Tree A (suc (suc n)) → Tree (Tree (Tree A (suc (suc n))) (suc n)) n → M (B × Tree (Address (suc n)) n)
      initVertial a₀ leaf hsh₀ = initHorizontal a₀ hsh₀ (λ-rec [] >>= (λ b → η (b , mapWithAddress hsh₀ (λ _ d → d ∷ []))))
      initVertial a₀ (node a₁ leaf) hsh₀ = initHorizontal a₀ hsh₀ (ν-rec a₀ leaf >>= (λ b → η (b , const hsh₀ [])))
      initVertial a₀ (node a₁ (node v hsh₁)) hsh₀ = initHorizontal a₀ hsh₀ (initVertial a₁ v hsh₁)

    graftRec : {n : ℕ} → (A → Tree B n → M B) → (Address n → M B) → Tree A (suc n) → M B
    graftRec {n = zero} ν-rec λ-rec leaf = λ-rec tt
    graftRec {n = zero} ν-rec λ-rec (node a (pt tr)) = graftRec ν-rec λ-rec tr >>= (λ b → ν-rec a (pt b))
    graftRec {suc n} ν-rec λ-rec leaf = λ-rec []
    graftRec {suc n} ν-rec λ-rec (node a leaf) = ν-rec a leaf
    graftRec {suc n} ν-rec λ-rec (node a (node v hsh)) = initVertial a v hsh >>= (λ { (b , _) → η b })
      where open PositiveDim ν-rec λ-rec


  graft : {M : Set → Set} → ⦃ isE : MonadError M ⦄ → {A : Set} → {n : ℕ} → Tree A (suc n) → Tree (Tree A (suc n)) n → M (Tree A (suc n))
  graft {M} {A} {n} tr brs = graftRec (λ a sh → η (node a sh)) (valueAt brs) tr
    where open GraftRec {M} {A} {Tree A (suc n)}

  join (pt (pt a)) = let open MonadError ⦃ ... ⦄ in η (pt a)
  join leaf = let open MonadError ⦃ ... ⦄ in η leaf
  join (node tr trSh) = traverseTree ⦃ monadIsApp isMonad ⦄ trSh join >>= graft tr
    where open MonadError ⦃ ... ⦄

  shellExtents : {M : Set → Set} → ⦃ isE : MonadError M ⦄ → {A : Set} → {n : ℕ} → Tree (Tree A (suc n)) n → M (Tree (Address (suc n)) n)
  shellExtents {M} {A} {n} sh = shellExtents₀ sh []

    where open MonadError ⦃ ... ⦄

          shellExtents₀ : Tree (Tree A (suc n)) n → Address (suc n) → M (Tree (Address (suc n)) n)
          shellExtents₀ sh ds = 
            traverseWithLocalData {{monadIsApp isMonad}} sh 
              (λ { leaf d ∂ → η (∂ ← (d ∷ ds)) ; 
                   (node _ sh₀) d ∂ → shellExtents₀ sh₀ (d ∷ ds) }) 
            >>= join


  exciseWithProp : {M : Set → Set} → ⦃ isE : MonadError M ⦄ → {A : Set} → {n : ℕ} → 
                         Tree A (suc n) → Derivative (Tree A (suc n)) n → (A → Bool) →
                         M (Tree A (suc n) × Tree (Tree A (suc n)) n)
  exciseWithProp leaf ∂ p = η (leaf , ∂ ← leaf)
    where open MonadError ⦃ ... ⦄
  exciseWithProp (node a sh) ∂ p = 
    if p a then 
      traverseWithLocalData {{monadIsApp isMonad}} sh
        (λ b _ ∂₀ → exciseWithProp b ∂₀ p)
        >>= (λ ztr → let (newSh , toJn) = unzip ztr
                       in join toJn >>= (λ jnd → η (node a newSh , jnd))) 
    else 
      η (leaf , sh)
    where open MonadError ⦃ ... ⦄

  exciseWithMask₀ : {M : Set → Set} → ⦃ isE : MonadError M ⦄ → {A B : Set} → {n : ℕ} →
                   Tree A (suc n) → Derivative (Tree A (suc n)) n → Tree B (suc n) → 
                   M (Tree A (suc n) × Tree (Tree A (suc n)) n)
  exciseWithMask₀ tr ∂ leaf = η (leaf , ∂ ← tr)
    where open MonadError ⦃ ... ⦄ 
  exciseWithMask₀ leaf ∂ (node a msk) = failWith "Incorrect Mask"
    where open MonadError ⦃ ... ⦄
  exciseWithMask₀ (node a trSh) ∂ (node _ mskSh) = 
    matchWithDerivative (λ t m₀ ∂₀ → exciseWithMask₀ t ∂₀ m₀) trSh mskSh
    >>= (λ ztr → let (nsh , toJn) = unzip ztr in join toJn 
    >>= (λ jn → η (node a nsh , jn)))
    where open MonadError ⦃ ... ⦄

  exciseWithMask : {M : Set → Set} → ⦃ isE : MonadError M ⦄ → {A B : Set} → {n : ℕ} → 
                   Tree A (suc n) → Tree B (suc n) → M (Tree A (suc n) × Tree (Tree A (suc n)) n)
  exciseWithMask tr msk = exciseWithMask₀ tr (globDerivative _ _) msk

  takeWhile : {A : Set} → {n : ℕ} → Tree A (suc n) → (A → Bool) → Tree A (suc n)
  takeWhile leaf p = leaf
  takeWhile (node a sh) p = if p a then node a (mapTree sh (λ t → takeWhile t p)) else leaf
