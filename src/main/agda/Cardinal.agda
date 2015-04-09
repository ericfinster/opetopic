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

  TreeSeq : Set → ℕ → ℕ → Set
  TreeSeq A n zero = Tree A n
  TreeSeq A n (suc l) = Tree (TreeSeq A (suc n) l) n

  seqLeaf : {A : Set} → {n k : ℕ} → TreeSeq A (suc n) k
  seqLeaf {k = zero} = leaf
  seqLeaf {k = suc k} = leaf

  treeSeqAssoc : {A : Set} → {n m k : ℕ} → (k≤m : k ≤ m) → 
                 TreeSeq A n (suc m) == TreeSeq (TreeSeq A (suc (k + n)) (Δ k≤m)) n k
  treeSeqAssoc z≤n = idp
  treeSeqAssoc {A} {n} (s≤s {k} {m} k≤m) = 
    Tree (Tree (TreeSeq A (suc (suc n)) m) (suc n)) n                       =⟨ treeSeqAssoc {A} {suc n} {m} {k} k≤m |in-ctx (λ B → Tree B n) ⟩ 
    Tree (TreeSeq (TreeSeq A (suc (k + suc n)) (Δ k≤m)) (suc n) k) n        =⟨ +-suc {k} {n} |in-ctx (λ l → Tree (TreeSeq (TreeSeq A (suc l) (Δ k≤m))(suc n) k) n) ⟩
    Tree (TreeSeq (TreeSeq A (suc (suc (k + n))) (Δ k≤m)) (suc n) k) n ∎

  CardinalTree : Set → ℕ → Set
  CardinalTree A zero = Tree A zero
  CardinalTree A (suc n) = CardinalTree (Tree A (suc n)) n 

  cardinalTreeIsSeq : {A : Set} → {n : ℕ} → CardinalTree A n == TreeSeq A zero n
  cardinalTreeIsSeq {A} {zero} = idp
  cardinalTreeIsSeq {A} {suc n} = 
    CardinalTree (Tree A (suc n)) n                                 =⟨ cardinalTreeIsSeq {Tree A (suc n)} {n} ⟩
    TreeSeq (Tree A (suc n)) zero n                                 =⟨ +-unit-r |in-ctx (λ l → TreeSeq (Tree A (suc l)) zero n) ⟩ 
    TreeSeq (TreeSeq A (suc (n + zero)) zero) zero n                =⟨ ! (Δ-refl-lem {n}) |in-ctx (λ l → TreeSeq (TreeSeq A (suc (n + zero)) l) zero n) ⟩ 
    TreeSeq (TreeSeq A (suc (n + zero)) (Δ (≤-refl {n}))) zero n    =⟨ ! (treeSeqAssoc {A} {zero} {n} {n} (≤-refl {n})) ⟩ 
    Tree (TreeSeq A (suc zero) n) zero ∎

  cardinalTreeAssoc : {A : Set} → {n k : ℕ} → (k≤n : k ≤ n) → CardinalTree A (suc n) == CardinalTree (TreeSeq A (suc k) (Δ k≤n)) k 
  cardinalTreeAssoc {A} {n} {k} k≤n = 
    CardinalTree A (suc n)                                      =⟨ cardinalTreeIsSeq {n = suc n}  ⟩
    TreeSeq A zero (suc n)                                      =⟨ treeSeqAssoc {A} {zero} {n} {k} k≤n ⟩ 
    TreeSeq (TreeSeq A (suc (k + zero)) (Δ k≤n)) zero k         =⟨ ! (+-unit-r {k}) |in-ctx (λ l → TreeSeq (TreeSeq A (suc l) (Δ k≤n)) zero k) ⟩
    TreeSeq (TreeSeq A (suc k) (Δ k≤n)) zero k                  =⟨ ! (cardinalTreeIsSeq {n = k}) ⟩ 
    CardinalTree (TreeSeq A (suc k) (Δ k≤n)) k ∎

  CardinalAddress : ℕ → Set
  CardinalAddress n = Suite Address (suc n)

  CardinalDerivative : Set → ℕ → Set
  CardinalDerivative A zero = ⊤
  CardinalDerivative A (suc n) = CardinalDerivative (Tree A (suc n)) n × Derivative A (suc n)

  CardinalNesting : Set → ℕ → Set
  CardinalNesting A n = CardinalTree (Nesting A n) n 

  Cardinal : (ℕ → Set) → ℕ → Set
  Cardinal A n = Suite (λ k → CardinalNesting (A k) k) (suc n)

  -- topZipper : {n : ℕ} → {A : Set} → CardinalDerivative (suc n) A → Zipper (suc n) A
  -- topZipper (_ , (sh , cntxt)) = leaf , cntxt

  -- completeWith : {n : ℕ} → {A : Set} → CardinalTree n A → A → Tree n A
  -- completeWith {zero} ct _ = ct
  -- completeWith {suc n} ct a = node a (completeWith {n} ct leaf)

  -- toShell : {n : ℕ} → {A : Set} → CardinalTree (suc n) A → Tree n (Tree (suc n) A)
  -- toShell ct = completeWith ct leaf 

  -- valueAtAddress : {n : ℕ} → {A : Set} → CardinalTree n A → CardinalAddress n → Maybe A
  -- valueAtAddress {zero} (pt a) ca = just a
  -- valueAtAddress {suc n} ct (tl ▶ hd) = 
  --   valueAtAddress {n} ct tl 
  --   >>= (λ t → t valueAt hd )

  -- shellAtAddress : {n : ℕ} → {A : Set} → CardinalTree (suc n) A → CardinalAddress n → Maybe (Tree (suc n) A)
  -- shellAtAddress {n} ct ca = valueAtAddress {n} ct ca

  poke : {M : Set → Set} → ⦃ isE : MonadError M ⦄ → {A : Set} → {n : ℕ} → CardinalTree A n → CardinalAddress n → M (A × CardinalDerivative A n)
  poke {n = zero} (pt a) ca = η (a , tt)
    where open MonadError ⦃ ... ⦄
  poke {n = suc n} ct (tl ▶ hd) = 
    poke {n = n} ct tl 
    >>= (λ { (tr , ∂) → seekTo tr hd 
                        >>= (λ { (leaf , cntxt) → failWith "Error in poke" ; 
                                 (node a sh , cntxt) → η (a , ∂ , (sh , cntxt)) }) })
    where open MonadError ⦃ ... ⦄

  tailDerivative : {M : Set → Set} → ⦃ isE : MonadError M ⦄ → {A : Set} → {n k : ℕ} → (k≤n : k ≤ n) → 
                   CardinalTree A (suc n) → CardinalAddress k → 
                   M ((TreeSeq A (suc k) (Δ k≤n)) × CardinalDerivative (TreeSeq A (suc k) (Δ k≤n)) k)
  tailDerivative k≤n ct ca = poke (coe (cardinalTreeAssoc k≤n) ct) ca
                   
  plugCardinal : {A : Set} → {n : ℕ} → CardinalDerivative A n → A → CardinalTree A n
  plugCardinal {n = zero} cd a = pt a
  plugCardinal {n = suc n} (cd , ∂) a = plugCardinal cd (∂ ← a)

  -- plug-tree : {n : ℕ} → {A : Set} → CardinalDerivative (suc n) A → Tree (suc n) A → CardinalTree (suc n) A
  -- plug-tree {n} (cd , ∂) tr = plug-cardinal cd tr

  -- map-cardinal-tree : {n : ℕ} → {A B : Set} → (f : A → B) → CardinalTree n A → CardinalTree n B
  -- map-cardinal-tree {zero} f (pt a) = pt (f a)
  -- map-cardinal-tree {suc n} f ct = map-cardinal-tree {n} (mapTree f) ct

  -- mapCardinalTreeWithAddr : {n : ℕ} → {A B : Set} → (f : CardinalAddress n → A → B) → CardinalTree n A → CardinalTree n B
  -- mapCardinalTreeWithAddr {zero} f (pt a) = pt (f (∥ ▶ []) a)
  -- mapCardinalTreeWithAddr {suc n} f ct = mapCardinalTreeWithAddr {n} (λ ca tr → mapWithAddress (λ addr a → f (ca ▶ addr ) a) tr) ct

  -- map-cardinal-nesting : {n : ℕ} → {A B : Set} → (f : A → B) → CardinalNesting n A → CardinalNesting n B
  -- map-cardinal-nesting {n} f cnst = map-cardinal-tree {n} (mapNesting f) cnst

  -- rootAddr : (n : ℕ) → CardinalAddress n 
  -- rootAddr zero = ∥ ▶ []
  -- rootAddr (suc n) = rootAddr n ▶ []

  -- rootTree : {n : ℕ} → {A : Set} → CardinalNesting n A → Maybe (Tree n (Nesting n A))
  -- rootTree {zero} cn = just cn
  -- rootTree {suc n} cn = poke cn (rootAddr n) >>= (λ res → just (proj₂ res))

  -- objectCardinal : {A : Set} → A → Cardinal 0 A
  -- objectCardinal a = ∥ ▶ (pt (obj a))

  -- extend : {n : ℕ} → {A : Set} → A → Cardinal n A → Cardinal (suc n) A
  -- extend {n} a (tl ▶ hd) = tl ▶ hd ▶ (map-cardinal-tree {n} (extendNesting a) hd)

  -- toComplex : {n : ℕ} → {A : Set} → Cardinal n A → Complex (Polarity A) n
  -- toComplex {zero} (∥ ▶ (pt cnst)) = ∥ ▶ (int pos (pt (mapNesting neutral cnst)))
  -- toComplex {suc n} {A} (tl ▶ hd) = toComplex tl ▶ int pos (node (ext neg) shell)
  --   where shell : Tree n (Tree (suc n) (Nesting (suc n) (Polarity A)))
  --         shell = toShell {n} (map-cardinal-nesting {suc n} neutral hd)

  data Polarity (A : Set) : Set where
    pos : Polarity A
    neg : Polarity A
    neutral : A → Polarity A

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

  module Extrusion {M : Set → Set} ⦃ isE : MonadError M ⦄ where

    open MonadError ⦃ ... ⦄

    getSelection : {A : Set} → {n : ℕ} → CardinalNesting A n → CardinalAddress n → (A → Bool) → M (Tree (Nesting A n) n)
    getSelection {n = zero} (pt nst) ca p = if p (baseValue nst) then η (pt (nst)) else failWith "Nothing selected"
    getSelection {n = suc n} cn (tl ▶ hd) p = 
      poke cn tl 
      >>= (λ { (tr , ∂) → seekTo tr hd 
      >>= (λ { (fcs , cntxt) → η (takeWhile fcs (λ nst → p (baseValue nst))) }) })

    extrudeAt : {A B : Set} → {n : ℕ} → CardinalNesting A n → CardinalAddress n → Tree B n → A → M (CardinalNesting A n)
    extrudeAt {n = zero} cn ca msk a = η (pt (box a cn))
    extrudeAt {n = suc n} cn (tl ▶ hd) msk a = 
      poke cn tl 
      >>= (λ { (tr , ∂) → seekTo tr hd 
      >>= (λ { (fcs , cntxt) → exciseWithMask tr msk 
      >>= (λ { (cn , verts) → η (plugCardinal ∂ (cntxt ↓ node (box a cn) verts)) }) }) })

    insertFillerAt : {A B : Set} → {n : ℕ} → CardinalNesting A (suc n) → CardinalAddress n → Tree B n → A → M (CardinalNesting A (suc n))
    insertFillerAt {n = zero} cn (∥ ▶ hd) msk a = η (pt (node (dot a) cn))
    insertFillerAt {n = suc n} cn (tl ▶ hd) msk a = 
      poke cn tl 
      >>= (λ { (tr , ∂) → seekTo tr hd 
      >>= (λ { (fcs , cntxt) → exciseWithMask fcs msk 
      >>= (λ { (cut , cutSh) → η (plugCardinal ∂ (cntxt ↓ node (node (dot a) cut) cutSh)) }) }) })

    insertLeafAt : {A B : Set} → {n k : ℕ} → (2pk≤n : 2 + k ≤ n) → CardinalNesting A n → CardinalAddress k → Tree B k → M (CardinalNesting A n)
    insertLeafAt (s≤s (s≤s (z≤n {n}))) cn ca msk = 
      tailDerivative {n = suc n} z≤n cn ca 
      >>= (λ { (seq , ∂) → η (coe! (cardinalTreeAssoc (s≤s (z≤n {n}))) (plugCardinal ∂ (node (seqLeaf {_} {1} {n}) (pt seq)))) })
    insertLeafAt {A} (s≤s (s≤s (s≤s {k} {n} k≤n))) cn (tl ▶ hd) msk = 
      tailDerivative {n = suc (suc n)} {k = k} (≤-suc (≤-suc k≤n)) cn tl 
      >>= (λ { (seq , ∂) → seekTo (transport P p seq) hd 
      >>= (λ { (fcs , cntxt) → exciseWithMask fcs msk 
      >>= (λ { (cut , cutSh) → η (coe! (cardinalTreeAssoc (≤-suc (≤-suc k≤n))) 
                                       (plugCardinal ∂ (transport! P p (cntxt ↓ node (node (seqLeaf {_} {2 + k} {Δ k≤n}) cut) cutSh)))) 
             }) }) })

      where p : Δ (≤-suc (≤-suc k≤n)) == suc (suc (Δ k≤n))
            p = Δ-lem (≤-suc k≤n) ∙ (ap suc (Δ-lem k≤n))

            P : ℕ → Set
            P m = TreeSeq (Nesting A (suc (suc (suc n)))) (suc k) m 

            Q : ℕ → Set
            Q m = CardinalTree (Nesting A (suc (suc (suc n)))) m 


    extrudeLoopAt : {A : Set} → {n : ℕ} → CardinalNesting A (suc n) → CardinalAddress n → A → M (CardinalNesting A (suc n))
    extrudeLoopAt {n = zero} cn ca a = η (pt (node (box a leaf) cn))
    extrudeLoopAt {n = suc n} cn ca a = 
      poke cn ca >>= (λ { (tr , ∂) → η (plugCardinal ∂ (node (box a leaf) (node tr (const (proj₁ (proj₂ ∂)) leaf)))) })

    extrudeDropAt : {A : Set} → {n : ℕ} → CardinalNesting A (suc (suc n)) → CardinalAddress n → A → M (CardinalNesting A (suc (suc n)))
    extrudeDropAt {n = zero} cn ca a = η (pt (node (node (dot a) leaf) cn))
    extrudeDropAt {n = suc n} cn ca a = 
      poke cn ca >>= (λ { (tr , ∂) → η (plugCardinal ∂ (node (node (dot a) leaf) (node tr (const (proj₁ (proj₂ ∂)) leaf)))) })

    padWithDropLeaf : {A : Set} → {n k : ℕ} → (3pk≤n : 3 + k ≤ n) → CardinalNesting A n → CardinalAddress k → M (CardinalNesting A n)
    padWithDropLeaf {A} (s≤s (s≤s (s≤s (z≤n {n})))) cn ca = 
      tailDerivative {n = suc (suc n)} {k = 0} z≤n cn ca 
      >>= (λ { (seq , ∂) → η (coe! (cardinalTreeAssoc {Nesting A (3 + n)} {2 + n} {2} (s≤s (s≤s (z≤n {n})))) 
          (plugCardinal {TreeSeq (Nesting A (3 + n)) 1 (2 + n)} {0} ∂ (node (node (seqLeaf {_} {2} {n}) leaf) (pt seq)))) })
    padWithDropLeaf {A} (s≤s (s≤s (s≤s (s≤s {k} {n} k≤n)))) cn ca = 
      tailDerivative {A = Nesting A (4 + n)} {n = suc (suc (suc n))} {k = suc k} (≤-suc (≤-suc (s≤s k≤n))) cn ca 
      >>= (λ { (seq , ∂) → η (coe! (cardinalTreeAssoc {Nesting A (4 + n)} {3 + n} {1 + k}  (≤-suc (≤-suc (s≤s k≤n)))) 
               (plugCardinal {TreeSeq (Nesting A (4 + n)) (2 + k) (Δ (≤-suc (≤-suc k≤n)))} {1 + k} ∂ (transport! P p (node (node (seqLeaf {_} {3 + k} {Δ k≤n}) (leaf {n = 1 + k})) 
                                                    (node (transport P p seq) (const (transport Q p (proj₁ (proj₂ ∂))) leaf)))))) })

      where p : Δ (≤-suc (≤-suc k≤n)) == suc (suc (Δ k≤n))
            p = Δ-lem (≤-suc k≤n) ∙ (ap suc (Δ-lem k≤n))

            P : ℕ → Set
            P m = TreeSeq (Nesting A (4 + n)) (suc (suc k)) m 

            Q : ℕ → Set
            Q m = Tree (Tree (P m) (suc k)) k 


    extrudeDispatch : {A : ℕ → Set} → {B : Set} → {n k : ℕ} → CardinalNesting (A n) n → CardinalAddress k → 
                      A k → A (1 + k) → Tree B k → CardinalDimFlag n k → M (CardinalNesting (A n) n)
    extrudeDispatch cn ca a₀ a₁ msk (dimLt sn≤k)       = η cn
    extrudeDispatch cn ca a₀ a₁ msk dimEq              = extrudeAt cn ca msk a₀
    extrudeDispatch cn ca a₀ a₁ msk dimSucc            = insertFillerAt cn ca msk a₁
    extrudeDispatch cn ca a₀ a₁ msk (dimDblSucc 2pk≤n) = insertLeafAt 2pk≤n cn ca msk

    dropDispatch : {A : ℕ → Set} → {n k : ℕ} → CardinalNesting (A n) n → CardinalAddress k → 
                   A (1 + k) → A (2 + k) → CardinalDimFlag n (1 + k) → M (CardinalNesting (A n) n)
    dropDispatch cn ca a₀ a₁ (dimLt sn≤sk)      = η cn
    dropDispatch cn ca a₀ a₁ dimEq              = extrudeLoopAt cn ca a₀
    dropDispatch cn ca a₀ a₁ dimSucc            = extrudeDropAt cn ca a₁
    dropDispatch cn ca a₀ a₁ (dimDblSucc 3pk≤n) = padWithDropLeaf 3pk≤n cn ca

    extrudeSelection : {A : ℕ → Set} → {n k : ℕ} → Cardinal A n → CardinalAddress k → 
                       A k → A (1 + k) → (A k → Bool) → k ≤ n → M (Cardinal A n)
    extrudeSelection c ca a₀ a₁ p k≤n = 
      getSelection (getAt _ k≤n c) ca p 
      >>= (λ msk → traverseSuite {{monadIsApp isMonad}} c (λ m cn → extrudeDispatch cn ca a₀ a₁ msk (getFlag m _)))

    dropAtAddress : {A : ℕ → Set} → {n k : ℕ} → Cardinal A n → CardinalAddress k → A (1 + k) → A (2 + k) → (k≤n : k ≤ n) → M (Cardinal A n)
    dropAtAddress {A} c ca a₀ a₁ k≤n = traverseSuite {{monadIsApp isMonad}} c (λ m cn → dropDispatch {A = A} cn ca a₀ a₁ (getFlag m _))

  -- doRootExtrusion : {n : ℕ} → {A : Set} → (k : ℕ) → (k≤n : k ≤ n) → A → A → Cardinal (suc n) A → Maybe (Cardinal (suc n) A)
  -- doRootExtrusion {n} {A} k k≤n a₀ a₁ c = rootTree {k} {A} (getAt k k≤n (tail c)) >>= (λ tr → doExtrude a₀ a₁ tr (rootAddr k) c)

  -- -- A bit weird, since the upper information passed to extend gets thrown away
  -- doTopRootExtrusion : {n : ℕ} → {A : Set} → A → A → Cardinal n A → Maybe (Cardinal (suc n) A)
  -- doTopRootExtrusion {n} a₀ a₁ c = doRootExtrusion n ≤-refl a₀ a₁ (extend a₁ c)


