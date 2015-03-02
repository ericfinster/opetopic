--
--  Cardinal.agda - Opetopic Cardinals
--

{-# OPTIONS --no-termination-check #-}

open import Prelude
open import Mtl
open import Tree
open import Nesting
open import Suite
open import Complex

module Cardinal where

  open Monad maybeM hiding (fmap ; η ; μ)

  TreeSeq : ℕ → ℕ → Set → Set
  TreeSeq n zero A = Tree n A
  TreeSeq n (suc k) A = Tree n (TreeSeq (suc n) k A)

  treeSeqAssoc : {n m k : ℕ} → {A : Set} → (k≤m : k ≤ m) → 
                 TreeSeq n (suc m) A == TreeSeq n k (TreeSeq (suc (k + n)) (Δ k≤m) A)
  treeSeqAssoc z≤n = idp
  treeSeqAssoc {n} {A = A} (s≤s {k} {m} k≤m) = 
    Tree n (Tree (suc n) (TreeSeq (suc (suc n)) m A))                     =⟨ treeSeqAssoc {suc n} {m} {k} {A} k≤m |in-ctx (λ B → Tree n B) ⟩ 
    Tree n (TreeSeq (suc n) k (TreeSeq (suc (k + suc n)) (Δ k≤m) A))      =⟨ +-suc {k} {n} |in-ctx (λ l → Tree n (TreeSeq (suc n) k (TreeSeq (suc l) (Δ k≤m) A))) ⟩
    Tree n (TreeSeq (suc n) k (TreeSeq (suc (suc (k + n))) (Δ k≤m) A)) ∎

  CardinalTree : ℕ → Set → Set
  CardinalTree zero A = Tree zero A
  CardinalTree (suc n) A = CardinalTree n (Tree (suc n) A)

  cardinalTreeIsSeq : {n : ℕ} → {A : Set} → CardinalTree n A == TreeSeq zero n A
  cardinalTreeIsSeq {zero} = idp
  cardinalTreeIsSeq {suc n} {A} = 
    CardinalTree n (Tree (suc n) A)                                 =⟨ cardinalTreeIsSeq {n} {Tree (suc n) A} ⟩
    TreeSeq zero n (Tree (suc n) A)                                 =⟨ +-unit-r |in-ctx (λ l → TreeSeq zero n (Tree (suc l) A)) ⟩ 
    TreeSeq zero n (TreeSeq (suc (n + zero)) zero A)                =⟨ ! (Δ-refl-lem {n}) |in-ctx (λ l → TreeSeq zero n (TreeSeq (suc (n + zero)) l A)) ⟩ 
    TreeSeq zero n (TreeSeq (suc (n + zero)) (Δ (≤-refl {n})) A)    =⟨ ! (treeSeqAssoc {zero} {n} {n} {A} (≤-refl {n})) ⟩ 
    Tree zero (TreeSeq (suc zero) n A) ∎

  cardinalTreeAssoc : {n k : ℕ} → {A : Set} → (k≤n : k ≤ n) → CardinalTree (suc n) A == CardinalTree k (TreeSeq (suc k) (Δ k≤n) A)
  cardinalTreeAssoc {n} {k} {A} k≤n = 
    CardinalTree (suc n) A                                      =⟨ cardinalTreeIsSeq {suc n}  ⟩
    TreeSeq zero (suc n) A                                      =⟨ treeSeqAssoc {zero} {n} {k} {A} k≤n ⟩ 
    TreeSeq zero k (TreeSeq (suc (k + zero)) (Δ k≤n) A)         =⟨ ! (+-unit-r {k}) |in-ctx (λ l → TreeSeq zero k (TreeSeq (suc l) (Δ k≤n) A)) ⟩
    TreeSeq zero k (TreeSeq (suc k) (Δ k≤n) A)                  =⟨ ! (cardinalTreeIsSeq {k}) ⟩ 
    CardinalTree k (TreeSeq (suc k) (Δ k≤n) A) ∎

  CardinalAddress : ℕ → Set
  CardinalAddress = DimSeq Address

  CardinalDerivative : ℕ → Set → Set
  CardinalDerivative zero A = ⊤
  CardinalDerivative (suc n) A = CardinalDerivative n (Tree (suc n) A) × Derivative (suc n) A

  completeWith : {n : ℕ} → {A : Set} → CardinalTree n A → A → Tree n A
  completeWith {zero} ct _ = ct
  completeWith {suc n} ct a = node a (completeWith {n} ct leaf)

  toShell : {n : ℕ} → {A : Set} → CardinalTree (suc n) A → Tree n (Tree (suc n) A)
  toShell ct = completeWith ct leaf 

  valueAtAddress : {n : ℕ} → {A : Set} → CardinalTree n A → CardinalAddress n → Maybe A
  valueAtAddress {zero} (pt a) ca = just a
  valueAtAddress {suc n} ct (tl ▶ hd) = 
    valueAtAddress {n} ct tl 
    >>= (λ t → t valueAt hd )

  shellAtAddress : {n : ℕ} → {A : Set} → CardinalTree (suc n) A → CardinalAddress n → Maybe (Tree (suc n) A)
  shellAtAddress {n} ct ca = valueAtAddress {n} ct ca

  poke : {n : ℕ} → {A : Set} → CardinalTree n A → CardinalAddress n → Maybe (CardinalDerivative n A × A)
  poke {zero} (pt a) ca = just (tt , a)
  poke {suc n} ct (tl ▶ hd) = 
    poke {n} ct tl 
    >>= (λ { (∂ , tr) → seekTo {suc n} hd tr 
                        >>= (λ { (leaf , ctx) → nothing ; 
                                 (node a sh , ctx) → just ((∂ , (sh , ctx)) , a) }) })

  tailDeriv : {n k : ℕ} → {A : Set} → (k≤n : k ≤ n) → CardinalTree (suc n) A → CardinalAddress k → 
              Maybe (CardinalDerivative k (TreeSeq (suc k) (Δ k≤n) A) × (TreeSeq (suc k) (Δ k≤n) A))
  tailDeriv k≤n ct ca = poke (coe (cardinalTreeAssoc k≤n) ct) ca

  plug-cardinal : {n : ℕ} → {A : Set} → CardinalDerivative n A → A → CardinalTree n A
  plug-cardinal {zero} tt a = pt a
  plug-cardinal {suc n} (cd , ∂) a = plug-cardinal cd (∂ ← a)

  plug-tree : {n : ℕ} → {A : Set} → CardinalDerivative (suc n) A → Tree (suc n) A → CardinalTree (suc n) A
  plug-tree {n} (cd , ∂) tr = plug-cardinal cd tr

  map-cardinal-tree : {n : ℕ} → {A B : Set} → (f : A → B) → CardinalTree n A → CardinalTree n B
  map-cardinal-tree {zero} f (pt a) = pt (f a)
  map-cardinal-tree {suc n} f ct = map-cardinal-tree {n} (mapTree f) ct

  CardinalNesting : ℕ → Set → Set
  CardinalNesting n A = CardinalTree n (Nesting n A)

  map-cardinal-nesting : {n : ℕ} → {A B : Set} → (f : A → B) → CardinalNesting n A → CardinalNesting n B
  map-cardinal-nesting {n} f cnst = map-cardinal-tree {n} (mapNesting f) cnst

  rootAddr : (n : ℕ) → CardinalAddress n 
  rootAddr zero = ∥ ▶ []
  rootAddr (suc n) = rootAddr n ▶ []

  rootTree : {n : ℕ} → {A : Set} → CardinalNesting n A → Maybe (Tree n (Nesting n A))
  rootTree {zero} cn = just cn
  rootTree {suc n} cn = poke cn (rootAddr n) >>= (λ res → just (proj₂ res))

  Cardinal : ℕ → Set → Set
  Cardinal n A = DimSeq (λ k → CardinalNesting k A) n

  objectCardinal : {A : Set} → A → Cardinal 0 A
  objectCardinal a = ∥ ▶ (pt (obj a))

  data Polarity (A : Set) : Set where
    pos : Polarity A
    neg : Polarity A
    neutral : A → Polarity A

  extend : {n : ℕ} → {A : Set} → A → Cardinal n A → Cardinal (suc n) A
  extend {n} a (tl ▶ hd) = tl ▶ hd ▶ (map-cardinal-tree {n} (extendNesting a) hd)

  toComplex : {n : ℕ} → {A : Set} → Cardinal n A → Complex (Polarity A) n
  toComplex {zero} (∥ ▶ (pt cnst)) = ∥ ▶ (int pos (pt (mapNesting neutral cnst)))
  toComplex {suc n} {A} (tl ▶ hd) = toComplex tl ▶ int pos (node (ext neg) shell)
    where shell : Tree n (Tree (suc n) (Nesting (suc n) (Polarity A)))
          shell = toShell {n} (map-cardinal-nesting {suc n} neutral hd)

  seqLeaf : {n k : ℕ} → {A : Set} → TreeSeq (suc n) k A
  seqLeaf {k = zero} = leaf
  seqLeaf {k = suc k} = leaf

  extrudeNestingAt : {n : ℕ} → {A B : Set} → A → CardinalNesting (suc n) A → CardinalAddress (suc n) → Tree (suc n) B → Maybe (CardinalNesting (suc n) A)
  extrudeNestingAt a cn (ca ▶ addr) msk = 
    poke cn ca 
    >>= (λ { (∂ , tr) → extrudeNesting a addr tr msk 
    >>= (λ res → just (plug-cardinal ∂ res)) })

  encloseAt : {n : ℕ} → {A : Set} → A → Address (suc n) → Tree (suc n) (Tree (suc (suc n)) A) → Maybe (Tree (suc n) (Tree (suc (suc n)) A))
  encloseAt a addr tr = 
    seekTo addr tr 
    >>= (λ { (fcs , cntxt) → flatten fcs
    >>= (λ flt → just (cntxt ↓ node (node a fcs) (const leaf flt))) })

  padWithLeaf : {n k : ℕ} → {A : Set} → Address (suc n) → TreeSeq (suc n) (suc (suc k)) A → Maybe (TreeSeq (suc n) (suc (suc k)) A)
  padWithLeaf {n} {k} addr seq = 
    seekTo addr seq 
    >>= (λ { (fcs , cntxt) → flatten fcs 
    >>= (λ flt → just (cntxt ↓ node (node (seqLeaf {2 + n} {k}) fcs) (const leaf flt))) })

  doTail : {n k : ℕ} → {A : Set} → (k≤n : k ≤ n) → CardinalNesting (2 + n) A → CardinalAddress k → Maybe (CardinalNesting (2 + n) A)
  doTail (z≤n {n}) cn ca = 
    tailDeriv {suc n} {0} z≤n cn ca 
    >>= (λ { (∂ , seq) → just (coe! (cardinalTreeAssoc (s≤s (z≤n {n}))) (plug-cardinal ∂ (node (seqLeaf {1} {n}) (pt seq)))) })
  doTail {A = A} (s≤s {k} {n} k≤n) cn (ca ▶ addr) = 
    tailDeriv {suc (suc n)} {k} (≤-suc (≤-suc k≤n)) cn ca 
    >>= (λ { (∂ , seq) → padWithLeaf {k = Δ k≤n} addr (transport P p seq) 
    >>= (λ res → just (coe! (cardinalTreeAssoc (≤-suc (≤-suc k≤n))) (plug-cardinal ∂ (transport! P p res)))) })

    where p : Δ (≤-suc (≤-suc k≤n)) == suc (suc (Δ k≤n))
          p = Δ-lem (≤-suc k≤n) ∙ (ap suc (Δ-lem k≤n))

          P : ℕ → Set
          P m = TreeSeq (suc k) m (Nesting (suc (suc (suc n))) A)

          Q : ℕ → Set
          Q m = CardinalTree m (Nesting (suc (suc (suc n))) A)

  doFiller : {n : ℕ} → {A : Set} → A → CardinalNesting (suc n) A → CardinalAddress n → Maybe (CardinalNesting (suc n) A)
  doFiller {zero} a cn (∥ ▶ hdAddr) = just (pt (node (ext a) cn))
  doFiller {suc n} {A} a cn (ca ▶ hdAddr) = 
    tailDeriv {n} {n} ≤-refl cn ca 
    >>= (λ { (∂ , seq) → encloseAt (ext a) hdAddr (transport P (Δ-refl-lem {n}) seq) 
    >>= (λ res → just (coe! (ap (λ A₁ → CardinalTree n A₁) (ap P (! (Δ-refl-lem {n})))) (plug-cardinal ∂ (transport! P (Δ-refl-lem {n}) res)))) })

    where P : ℕ → Set
          P m = TreeSeq (suc n) m (Tree (suc (suc n)) (Nesting (suc (suc n)) A))
  
  data CardinalDimFlag : ℕ → ℕ → Set where
    dimEq : {k : ℕ} → CardinalDimFlag k k 
    dimSucc : {k : ℕ} → CardinalDimFlag (suc k) k
    dimLt : {n k : ℕ} → suc n ≤ k → CardinalDimFlag n k
    dimDblSucc : {n k : ℕ} → suc (suc k) ≤ n → CardinalDimFlag n k

  getFlag : (n : ℕ) → (k : ℕ) → CardinalDimFlag n k
  getFlag zero zero = dimEq
  getFlag zero (suc k) = dimLt (s≤s z≤n)
  getFlag (suc zero) zero = dimSucc
  getFlag (suc (suc n)) zero = dimDblSucc (s≤s (s≤s z≤n))
  getFlag (suc n) (suc k) with getFlag n k
  getFlag (suc n) (suc .n) | dimEq = dimEq
  getFlag (suc .(suc k)) (suc k) | dimSucc = dimSucc
  getFlag (suc n) (suc k) | dimLt sn≤k = dimLt (s≤s sn≤k)
  getFlag (suc n) (suc k) | dimDblSucc ssk≤n = dimDblSucc (s≤s ssk≤n)

  extrudeDispatch : {n k : ℕ} → {A B : Set} → A → A → Tree k B → CardinalAddress k → CardinalDimFlag n k → CardinalNesting n A → Maybe (CardinalNesting n A)
  extrudeDispatch {k = zero} a₀ a₁ msk ca dimEq cn = just (pt (int a₀ cn))
  extrudeDispatch {k = suc k} a₀ a₁ msk ca dimEq cn = extrudeNestingAt a₀ cn ca msk
  extrudeDispatch a₀ a₁ msk ca dimSucc cn = doFiller a₁ cn ca
  extrudeDispatch a₀ a₁ msk ca (dimLt sn≤k) cn = just cn
  extrudeDispatch a₀ a₁ msk ca (dimDblSucc (s≤s (s≤s k≤n))) cn = doTail k≤n cn ca 

  traverseCardinal : {n : ℕ} → {A : Set} → ((m : ℕ) → CardinalNesting m A → Maybe (CardinalNesting m A)) → Cardinal n A → Maybe (Cardinal n A)
  traverseCardinal {zero} f (∥ ▶ hd) = f 0 hd >>= (λ newHd → just (∥ ▶ newHd))
  traverseCardinal {suc n} f (c ▶ hd) = traverseCardinal {n} f c >>= (λ newTl → f (suc n) hd >>= (λ newHd → just (newTl ▶ newHd)))

  doExtrude : {n k : ℕ} → {A B : Set} → A → A → Tree k B → CardinalAddress k → Cardinal n A → Maybe (Cardinal n A)
  doExtrude {k = k} a₀ a₁ msk ca c = traverseCardinal (λ m → extrudeDispatch a₀ a₁ msk ca (getFlag m k)) c

  doRootExtrusion : {n : ℕ} → {A : Set} → (k : ℕ) → (k≤n : k ≤ n) → A → A → Cardinal (suc n) A → Maybe (Cardinal (suc n) A)
  doRootExtrusion {n} {A} k k≤n a₀ a₁ c = rootTree {k} {A} (getAt n k k≤n (tail c)) >>= (λ tr → doExtrude a₀ a₁ tr (rootAddr k) c)

  -- A bit weird, since the upper information passed to extend gets thrown away
  doTopRootExtrusion : {n : ℕ} → {A : Set} → A → A → Cardinal n A → Maybe (Cardinal (suc n) A)
  doTopRootExtrusion {n} a₀ a₁ c = doRootExtrusion n ≤-refl a₀ a₁ (extend a₁ c)
