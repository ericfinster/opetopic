--
--  Tree.agda - Higher Dimensional Trees
--
--  Eric Finster
--

{-# OPTIONS --no-termination-check #-}

open import Prelude
open import Mtl

module Tree where

  open Monad errorM

  data Tree (A : Set) : ℕ → Set where
    pt : (a : A) → Tree A 0
    leaf : {n : ℕ} → Tree A (suc n)
    node : {n : ℕ} → (a : A) → (sh : Tree (Tree A (suc n)) n) → Tree A (suc n)

  isPoint : {A : Set} → {n : ℕ} → Tree A n → Bool
  isPoint (pt a) = true
  isPoint _ = false

  isLeaf : {A : Set} → {n : ℕ} → Tree A n → Bool
  isLeaf leaf = true
  isLeaf _ = false

  isNode : {A : Set} → {n : ℕ} → Tree A n → Bool
  isNode (node a sh) = true
  isNode _ = false

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

    visit : {A : Set} → {n : ℕ} → Address n → Zipper A (suc n) → Error (Zipper A (suc n))
    visit {n = zero} tt (leaf , cntxt) = fail "Cannot visit a leaf"
    visit {n = zero} tt (node hd (pt tl) , cntxt) = succeed (tl , ((hd , tt) ∷ cntxt))
    visit {n = suc n} d (leaf , cntxt) = fail "Cannot visit a leaf"
    visit {n = suc n} d (node a sh , cntxt) = 
      seek (sh , []) d
      >>= (λ { (leaf , cntxt₀) → fail "Seek failed in shell" ; 
               (node tr hsh , cntxt₀) → succeed (tr , (a , hsh , cntxt₀) ∷ cntxt) })

    seek : {A : Set} → {n : ℕ} → Zipper A (suc n) → Address (suc n) → Error (Zipper A (suc n))
    seek z [] = succeed z
    seek z (d ∷ ds) = seek z ds >>= visit d

    parent : {A : Set} → {n : ℕ} → Zipper A n → Error (Zipper A n)
    parent {n = zero} z = fail "No parent"
    parent {n = suc n} (fcs , []) = fail "No parent"
    parent {n = suc n} (fcs , (a , ∂) ∷ cs) = succeed (node a (∂ ← fcs) , cs)

    parentWhich : {A : Set} → {n : ℕ} → Zipper A n → (A → Bool) → Error (Zipper A n)
    parentWhich {n = zero} z p = fail "No parent"
    parentWhich {n = suc n} (fcs , []) p = fail "No parent"
    parentWhich {n = suc n} (fcs , (a , ∂) ∷ cs) p = 
      let parent = (node a (∂ ← fcs) , cs)
      in if p a then succeed parent else parentWhich parent p
  
  globDerivative : (A : Set) → (n : ℕ) → Derivative A n
  globDerivative A zero = tt
  globDerivative A (suc n) = (globDerivative (Tree A (suc n)) n ← leaf) , []

  seekTo : {A : Set} → {n : ℕ} → Tree A n → Address n → Error (Zipper A n)
  seekTo {n = zero} tr tt = succeed (tr , tt)
  seekTo {n = suc n} tr addr = seek (tr , []) addr 

  rootValue : {A : Set} → {n : ℕ} → Tree A n → Error A
  rootValue (pt a) = succeed a
  rootValue leaf = fail "No root value"
  rootValue (node a sh) = succeed a

  valueAt : {A : Set} → {n : ℕ} → Tree A n → Address n → Error A
  valueAt tr addr = seekTo tr addr >>= (λ z → rootValue (proj₁ z))

  traverseTree : {G : Set → Set} → ⦃ isA : Applicative G ⦄ → {A B : Set} → {n : ℕ} → Tree A n → (A → G B) → G (Tree B n)
  traverseTree (pt a) f = let open Applicative ⦃ ... ⦄ in pure pt ⊛ f a
  traverseTree leaf f = let open Applicative ⦃ ... ⦄ in pure leaf
  traverseTree (node a sh) f = let open Applicative ⦃ ... ⦄ in 
    pure node ⊛ f a ⊛ traverseTree sh (λ b → traverseTree b f)

  mapTree : {A B : Set} → {n : ℕ} → Tree A n → (A → B) → Tree B n
  mapTree = traverseTree ⦃ idA ⦄ 

  traverseWithAddress : {G : Set → Set} → ⦃ isA : Applicative G ⦄ → {A B : Set} → {n : ℕ} → Tree A n → (A → Address n → G B) → G (Tree B n)
  traverseWithAddress {G = G} tr f = traverseWithAddress₀ tr (rootAddr _) f

    where open Applicative ⦃ ... ⦄ 

          traverseWithAddress₀ : {A B : Set} → {n : ℕ} → Tree A n → Address n → (A → Address n → G B) → G (Tree B n)
          traverseWithAddress₀ (pt a) base f = pure pt ⊛ f a base
          traverseWithAddress₀ leaf base f = pure leaf
          traverseWithAddress₀ (node a sh) base f = 
            pure node ⊛ f a base ⊛ traverseWithAddress sh (λ b d → traverseWithAddress₀ b (d ∷ base) f)

  mapWithAddress : {A B : Set} → {n : ℕ} → Tree A n → (A → Address n → B) → Tree B n
  mapWithAddress = traverseWithAddress ⦃ idA ⦄

  const : {A B : Set} → {n : ℕ} → Tree A n → B → Tree B n
  const tr b = mapTree tr (λ _ → b)

  splitWith : {A B C : Set} → {n : ℕ} → (f : A → B × C) → Tree A n → Tree B n × Tree C n
  splitWith f (pt a) = let (b , c) = f a in (pt b , pt c)
  splitWith f leaf = leaf , leaf
  splitWith f (node a sh) = 
    let (aSh , bSh) = splitWith (λ br → splitWith f br) sh 
        (a , b) = f a
    in (node a aSh , node b bSh)

  unzip : {n : ℕ} → {A B : Set} → Tree (A × B) n → (Tree A n × Tree B n)
  unzip tr = splitWith (λ pr → pr) tr

  traverseWithLocalData : {G : Set → Set} → ⦃ isA : Applicative G ⦄ → {A B C : Set} → {n : ℕ} → 
                          Tree A n → (A → Address n → Derivative B n → G C) → G (Tree C n)
  traverseWithLocalData {G = G} tr f = traverseWithLocalData₀ tr (rootAddr _) f

    where open Applicative ⦃ ... ⦄

          traverseWithLocalData₀ : {A B C : Set} → {n : ℕ} → Tree A n → Address n → (A → Address n → Derivative B n → G C) → G (Tree C n)
          traverseWithLocalData₀ (pt a) base f = pure pt ⊛ f a base tt
          traverseWithLocalData₀ leaf base f = pure leaf
          traverseWithLocalData₀ (node a sh) base f = 
            pure node ⊛ f a base (const sh leaf , []) ⊛ traverseWithAddress sh (λ b d → traverseWithLocalData₀ b (d ∷ base) f)
  
  module Matching {M : Set → Set} ⦃ isErr : MonadError M ⦄ where

    open MonadError ⦃ ... ⦄
    open Applicative (monadIsApp isMonad)

    match : {A B C : Set} → {n : ℕ} → (A → B → M C) → Tree A n → Tree B n → M (Tree C n)
    match φ (pt a) (pt b) = pure pt ⊛ φ a b 
    match φ leaf leaf = pure leaf
    match φ leaf (node b shB) = failWith "Match error"
    match φ (node a shA) leaf = failWith "Match error"
    match φ (node a shA) (node b shB) = 
      pure node ⊛ φ a b ⊛ match (match φ) shA shB

    matchWithAddress₀ : {A B C : Set} → {n : ℕ} → (A → B → Address n → M C) → Address n → Tree A n → Tree B n → M (Tree C n)
    matchWithAddress₀ φ base (pt a) (pt b) = pure pt ⊛ φ a b tt
    matchWithAddress₀ φ base leaf leaf = pure leaf
    matchWithAddress₀ φ base leaf (node a shB) = failWith "Match error"
    matchWithAddress₀ φ base (node a shA) leaf = failWith "Match error"
    matchWithAddress₀ φ base (node a shA) (node b shB) = 
      pure node ⊛ φ a b base ⊛ matchWithAddress₀ (λ brA brB dir → matchWithAddress₀ φ (dir ∷ base) brA brB) (rootAddr _) shA shB

    matchWithAddress : {A B C : Set} → {n : ℕ} → (A → B → Address n → M C) → Tree A n → Tree B n → M (Tree C n)
    matchWithAddress φ trA trB = matchWithAddress₀ φ (rootAddr _) trA trB 

    matchWithDerivative : {A B C D : Set} → {n : ℕ} → (A → B → Derivative C n → M D) → 
                          Tree A n → Tree B n → M (Tree D n)
    matchWithDerivative φ (pt a) (pt b) = pure pt ⊛ φ a b tt
    matchWithDerivative φ leaf leaf = pure leaf
    matchWithDerivative φ leaf (node b shB) = failWith "Match error"
    matchWithDerivative φ (node a shA) leaf = failWith "Match error"
    matchWithDerivative φ (node a shA) (node b shB) = 
      pure node ⊛ φ a b (const shB leaf , []) ⊛ match (matchWithDerivative φ) shA shB

  open Matching public

  join : {A : Set} → {n : ℕ} → Tree (Tree A n) n → Error (Tree A n)

  module GraftRec {A B : Set} where

    module PositiveDim {n : ℕ} (ν-rec : (A → Tree B (suc n) → Error B)) (λ-rec : (Address (suc n) → Error B)) where

      unzipAndJoin : Tree (Tree B (suc n) × Tree (Address (suc n)) n) n → Error (Tree (Tree B (suc n)) n × Tree (Address (suc n)) n)
      unzipAndJoin zt with unzip zt
      unzipAndJoin zt | (bSh , adrJnSh) = join adrJnSh >>= (λ adTr → η (bSh , adTr))

      unzipJoinAndAppend : Tree (Tree B (suc n) × Tree (Address (suc n)) n) n → Error B → Error (Tree B (suc n) × Tree (Address (suc n)) n)
      unzipJoinAndAppend zt mb = 
        unzipAndJoin zt >>= (λ { (bSh , adTr) → 
        mb >>= (λ b → η (node b bSh , adTr)) })

      horizontalPass : Address (suc n) → Tree (Tree A (suc (suc n))) (suc n) → Derivative (Address (suc n)) n → Error (Tree B (suc n) × Tree (Address (suc n)) n)
      horizontalPass base leaf ∂ = η (leaf , ∂ ← base)
      horizontalPass base (node leaf hsh) ∂ = 
        traverseWithLocalData ⦃ monadIsApp errorM ⦄ hsh (λ hbr d ∂₀ → horizontalPass (d ∷ base) hbr ∂₀) 
        >>= (λ res → unzipJoinAndAppend res (λ-rec base))
      horizontalPass base (node (node a vsh) hsh) ∂ = 
        horizontalPass base vsh ∂ 
        >>= (λ { (bTr , adTr₀) → matchWithDerivative ⦃ errorE ⦄ {C = Address (suc n)} horizontalPass adTr₀ hsh 
        >>= (λ res → unzipJoinAndAppend res (ν-rec a bTr)) })

      initHorizontal : A → Tree (Tree (Tree A (suc (suc n))) (suc n)) n → Error (B × Tree (Address (suc n)) n) → Error (B × Tree (Address (suc n)) n)
      initHorizontal a hsh m = 
        m >>= (λ { (b₀ , adTr₀) → matchWithDerivative ⦃ errorE ⦄ {C = Address (suc n)} horizontalPass adTr₀ hsh 
          >>= (λ res → unzipAndJoin res 
          >>= (λ { (bSh , adTr) → ν-rec a (node b₀ bSh) 
          >>= (λ b → η (b , adTr)) })) })

      initVertial : A → Tree A (suc (suc n)) → Tree (Tree (Tree A (suc (suc n))) (suc n)) n → Error (B × Tree (Address (suc n)) n)
      initVertial a₀ leaf hsh₀ = initHorizontal a₀ hsh₀ (λ-rec [] >>= (λ b → η (b , mapWithAddress hsh₀ (λ _ d → d ∷ []))))
      initVertial a₀ (node a₁ leaf) hsh₀ = initHorizontal a₀ hsh₀ (ν-rec a₁ leaf >>= (λ b → η (b , const hsh₀ [])))
      initVertial a₀ (node a₁ (node v hsh₁)) hsh₀ = initHorizontal a₀ hsh₀ (initVertial a₁ v hsh₁)

    graftRec : {n : ℕ} → (A → Tree B n → Error B) → (Address n → Error B) → Tree A (suc n) → Error B
    graftRec {n = zero} ν-rec λ-rec leaf = λ-rec tt
    graftRec {n = zero} ν-rec λ-rec (node a (pt tr)) = graftRec ν-rec λ-rec tr >>= (λ b → ν-rec a (pt b))
    graftRec {suc n} ν-rec λ-rec leaf = λ-rec []
    graftRec {suc n} ν-rec λ-rec (node a leaf) = ν-rec a leaf
    graftRec {suc n} ν-rec λ-rec (node a (node v hsh)) = initVertial a v hsh >>= (λ { (b , _) → η b })
      where open PositiveDim ν-rec λ-rec

  graft : {A : Set} → {n : ℕ} → Tree A (suc n) → Tree (Tree A (suc n)) n → Error (Tree A (suc n))
  graft {A} {n} tr brs = graftRec (λ a sh → η (node a sh)) (valueAt brs) tr
    where open GraftRec {A} {Tree A (suc n)}

  join (pt (pt a)) = succeed (pt a) 
  join leaf = succeed leaf 
  join (node tr trSh) = traverseTree ⦃ monadIsApp errorM ⦄ trSh join >>= graft tr

  shellExtents : {A : Set} → {n : ℕ} → Tree (Tree A (suc n)) n → Error (Tree (Address (suc n)) n)
  shellExtents {A} {n} sh = shellExtents₀ sh []

    where shellExtents₀ : Tree (Tree A (suc n)) n → Address (suc n) → Error (Tree (Address (suc n)) n)
          shellExtents₀ sh ds = 
            traverseWithLocalData ⦃ monadIsApp errorM ⦄ sh 
              (λ { leaf d ∂ → η (∂ ← (d ∷ ds)) ; 
                   (node _ sh₀) d ∂ → shellExtents₀ sh₀ (d ∷ ds) }) 
            >>= join

  exciseWithProp₀ : {A : Set} → {n : ℕ} → Tree A (suc n) → Derivative (Tree A (suc n)) n → (A → Bool) →
                   Error (Tree A (suc n) × Tree (Tree A (suc n)) n)
  exciseWithProp₀ leaf ∂ p = succeed (leaf , ∂ ← leaf)
  exciseWithProp₀ (node a sh) ∂ p = 
    if p a then 
      traverseWithLocalData ⦃ monadIsApp errorM ⦄ sh
        (λ b _ ∂₀ → exciseWithProp₀ b ∂₀ p)
        >>= (λ ztr → let (newSh , toJn) = unzip ztr
                       in join toJn >>= (λ jnd → η (node a newSh , jnd))) 
    else 
      succeed (leaf , ∂ ← node a sh)

  exciseWithProp : {A : Set} → {n : ℕ} → Tree A (suc n) → (A → Bool) → Error (Tree A (suc n) × Tree (Tree A (suc n)) n)
  exciseWithProp tr p = exciseWithProp₀ tr (globDerivative _ _) p

  exciseWithMask₀ : {A B : Set} → {n : ℕ} → Tree A (suc n) → Derivative (Tree A (suc n)) n → Tree B (suc n) → 
                    Error (Tree A (suc n) × Tree (Tree A (suc n)) n)
  exciseWithMask₀ tr ∂ leaf = succeed (leaf , ∂ ← tr)
  exciseWithMask₀ leaf ∂ (node a msk) = fail "Incomplete Mask"
  exciseWithMask₀ (node a trSh) ∂ (node _ mskSh) = 
    matchWithDerivative ⦃ errorE ⦄ (λ t m₀ ∂₀ → exciseWithMask₀ t ∂₀ m₀) trSh mskSh
    >>= (λ ztr → let (nsh , toJn) = unzip ztr in join toJn 
    >>= (λ jn → succeed (node a nsh , jn)))

  exciseWithMask : {A B : Set} → {n : ℕ} → Tree A (suc n) → Tree B (suc n) → Error (Tree A (suc n) × Tree (Tree A (suc n)) n)
  exciseWithMask tr msk = exciseWithMask₀ tr (globDerivative _ _) msk

  takeWhile : {A : Set} → {n : ℕ} → Tree A (suc n) → (A → Bool) → Tree A (suc n)
  takeWhile leaf p = leaf
  takeWhile (node a sh) p = if p a then node a (mapTree sh (λ t → takeWhile t p)) else leaf
