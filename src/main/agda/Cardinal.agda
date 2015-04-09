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

  Cardinal : ℕ → Set → Set
  Cardinal n A = Suite (λ k → CardinalNesting A k) (suc n)

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

  data Polarity (A : Set) : Set where
    pos : Polarity A
    neg : Polarity A
    neutral : A → Polarity A

  -- extend : {n : ℕ} → {A : Set} → A → Cardinal n A → Cardinal (suc n) A
  -- extend {n} a (tl ▶ hd) = tl ▶ hd ▶ (map-cardinal-tree {n} (extendNesting a) hd)

  -- toComplex : {n : ℕ} → {A : Set} → Cardinal n A → Complex (Polarity A) n
  -- toComplex {zero} (∥ ▶ (pt cnst)) = ∥ ▶ (int pos (pt (mapNesting neutral cnst)))
  -- toComplex {suc n} {A} (tl ▶ hd) = toComplex tl ▶ int pos (node (ext neg) shell)
  --   where shell : Tree n (Tree (suc n) (Nesting (suc n) (Polarity A)))
  --         shell = toShell {n} (map-cardinal-nesting {suc n} neutral hd)

  -- seqLeaf : {n k : ℕ} → {A : Set} → TreeSeq (suc n) k A
  -- seqLeaf {k = zero} = leaf
  -- seqLeaf {k = suc k} = leaf

  getSelection : {M : Set → Set} → ⦃ isE : MonadError M ⦄ → {A : Set} → {n : ℕ} → 
                 CardinalNesting A (suc n) → CardinalAddress (suc n) → (A → Bool) →
                 M (Tree (Nesting A (suc n)) (suc n))
  getSelection cn (tl ▶ hd) p = 
    poke cn tl 
    >>= (λ { (tr , ∂) → seekTo tr hd 
    >>= (λ { (fcs , cntxt) → η (takeWhile fcs (λ nst → p (baseValue nst))) }) })
    where open MonadError ⦃ ... ⦄

  -- extrudeSelection
  -- extrudeNestingAt : {n : ℕ} → {A B : Set} → A → CardinalNesting (suc n) A → CardinalAddress (suc n) → Tree (suc n) B → Maybe (CardinalNesting (suc n) A)
  -- extrudeNestingAt a cn (ca ▶ addr) msk = 
  --   poke cn ca 
  --   >>= (λ { (∂ , tr) → extrudeNesting a addr tr msk 
  --   >>= (λ res → just (plug-cardinal ∂ res)) })

  -- encloseAt : {n : ℕ} → {A B : Set} → A → Address (suc n) → Tree (suc n) (Tree (suc (suc n)) A) → Tree (suc n) B → Maybe (Tree (suc n) (Tree (suc (suc n)) A))
  -- encloseAt a addr tr msk = 
  --   seekTo addr tr 
  --   >>= (λ { (fcs , cntxt) → excise fcs msk 
  --   >>= (λ { (cut , cutSh) → just (cntxt ↓ node (node a cut) cutSh) }) })

  -- padWithLeaf : {n k : ℕ} → {A B : Set} → Address (suc n) → TreeSeq (suc n) (suc (suc k)) A → Tree (suc n) B → Maybe (TreeSeq (suc n) (suc (suc k)) A)
  -- padWithLeaf {n} {k} addr seq msk = 
  --   seekTo addr seq 
  --   >>= (λ { (fcs , cntxt) → excise fcs msk 
  --   >>= (λ { (cut , cutSh) → just (cntxt ↓ node (node (seqLeaf {2 + n} {k}) cut) cutSh) }) })


  -- -- Here are the three dropping routines.  Notice how completely uniform they are.  Should do something
  -- -- similar for the extrusion routines ....
  -- extrudeLoopAt : {n : ℕ} → {A : Set} → A → CardinalNesting (suc n) A → CardinalAddress n → Maybe (CardinalNesting (suc n) A)
  -- extrudeLoopAt {zero} a cn ca = just (pt (node (int a leaf) cn))
  -- extrudeLoopAt {suc n} a cn ca = 
  --   poke cn ca >>= (λ { (∂ , tr) → just (plug-cardinal ∂ (node (int a leaf) (node tr (const leaf (proj₁ (proj₂ ∂)))))) })

  -- extrudeDropAt : {n : ℕ} → {A : Set} → A → CardinalNesting (suc (suc n)) A → CardinalAddress n → Maybe (CardinalNesting (suc (suc n)) A)
  -- extrudeDropAt {zero} a cn ca = just (pt (node (node (ext a) leaf) cn))
  -- extrudeDropAt {suc n} a cn ca = 
  --   poke cn ca >>= (λ { (∂ , tr) → just (plug-cardinal ∂ (node (node (ext a) leaf) (node tr (const leaf (proj₁ (proj₂ ∂)))))) })

  -- -- cardinalTreeAssoc : {n k : ℕ} → {A : Set} → (k≤n : k ≤ n) → CardinalTree (suc n) A == CardinalTree k (TreeSeq (suc k) (Δ k≤n) A)
  -- -- plug-cardinal : {n : ℕ} → {A : Set} → CardinalDerivative n A → A → CardinalTree n A
  -- -- tailDeriv : {n k : ℕ} → {A : Set} → (k≤n : k ≤ n) → CardinalTree (suc n) A → CardinalAddress k → 
  -- --             Maybe (CardinalDerivative k (TreeSeq (suc k) (Δ k≤n) A) × (TreeSeq (suc k) (Δ k≤n) A))

  -- padWithDropLeaf : {n k : ℕ} → {A : Set} → (3pk≤n : 3 + k ≤ n) → CardinalNesting n A → CardinalAddress k → Maybe (CardinalNesting n A)
  -- padWithDropLeaf {A = A} (s≤s (s≤s (s≤s (z≤n {n})))) cn ca = 
  --   tailDeriv {suc (suc n)} {0} z≤n cn ca >>= (λ { (∂ , seq) → 
  --     just (coe! (cardinalTreeAssoc {2 + n} {2} {Nesting (3 + n) A} (s≤s (s≤s (z≤n {n})))) 
  --          (plug-cardinal {0} {TreeSeq 1 (2 + n) (Nesting (3 + n) A)} ∂ (node (node (seqLeaf {2} {n}) leaf) (pt seq)))) })
  -- padWithDropLeaf {A = A} (s≤s (s≤s (s≤s (s≤s {k} {n} k≤n)))) cn ca = 
  --   tailDeriv {suc (suc (suc n))} {suc k} {Nesting (4 + n) A} (≤-suc (≤-suc (s≤s k≤n))) cn ca 
  --   >>= (λ { (∂ , seq) → 
  --     just (coe! (cardinalTreeAssoc {3 + n} {1 + k} {Nesting (4 + n) A} (≤-suc (≤-suc (s≤s k≤n)))) 
  --          (plug-cardinal {1 + k} {TreeSeq (2 + k) (Δ (≤-suc (≤-suc k≤n))) (Nesting (4 + n) A)} ∂ (transport! P p (node (node (seqLeaf {3 + k} {Δ k≤n}) (leaf {n = 1 + k})) 
  --                                                 (node (transport P p seq) (const leaf (transport Q p (proj₁ (proj₂ ∂))))))))) })

  --   where p : Δ (≤-suc (≤-suc k≤n)) == suc (suc (Δ k≤n))
  --         p = Δ-lem (≤-suc k≤n) ∙ (ap suc (Δ-lem k≤n))

  --         P : ℕ → Set
  --         P m = TreeSeq (suc (suc k)) m (Nesting (4 + n) A)
  
  --         Q : ℕ → Set
  --         Q m = Tree k (Tree (suc k) (P m))

  -- doTail : {n k : ℕ} → {A B : Set} → (k≤n : k ≤ n) → CardinalNesting (2 + n) A → CardinalAddress k → Tree k B → Maybe (CardinalNesting (2 + n) A)
  -- doTail (z≤n {n}) cn ca msk = 
  --   tailDeriv {suc n} {0} z≤n cn ca 
  --   >>= (λ { (∂ , seq) → just (coe! (cardinalTreeAssoc (s≤s (z≤n {n}))) (plug-cardinal ∂ (node (seqLeaf {1} {n}) (pt seq)))) })
  -- doTail {A = A} (s≤s {k} {n} k≤n) cn (ca ▶ addr) msk = 
  --   tailDeriv {suc (suc n)} {k} (≤-suc (≤-suc k≤n)) cn ca 
  --   >>= (λ { (∂ , seq) → padWithLeaf {k = Δ k≤n} addr (transport P p seq) msk
  --   >>= (λ res → just (coe! (cardinalTreeAssoc (≤-suc (≤-suc k≤n))) (plug-cardinal ∂ (transport! P p res)))) })

  --   where p : Δ (≤-suc (≤-suc k≤n)) == suc (suc (Δ k≤n))
  --         p = Δ-lem (≤-suc k≤n) ∙ (ap suc (Δ-lem k≤n))

  --         P : ℕ → Set
  --         P m = TreeSeq (suc k) m (Nesting (suc (suc (suc n))) A)

  --         Q : ℕ → Set
  --         Q m = CardinalTree m (Nesting (suc (suc (suc n))) A)

  -- doFiller : {n : ℕ} → {A B : Set} → A → CardinalNesting (suc n) A → CardinalAddress n → Tree n B → Maybe (CardinalNesting (suc n) A)
  -- doFiller {zero} a cn (∥ ▶ hdAddr) msk = just (pt (node (ext a) cn))
  -- doFiller {suc n} {A} a cn (ca ▶ hdAddr) msk = 
  --   tailDeriv {n} {n} ≤-refl cn ca 
  --   >>= (λ { (∂ , seq) → encloseAt (ext a) hdAddr (transport P (Δ-refl-lem {n}) seq) msk
  --   >>= (λ res → just (coe! (ap (λ A₁ → CardinalTree n A₁) (ap P (! (Δ-refl-lem {n})))) (plug-cardinal ∂ (transport! P (Δ-refl-lem {n}) res)))) })

  --   where P : ℕ → Set
  --         P m = TreeSeq (suc n) m (Tree (suc (suc n)) (Nesting (suc (suc n)) A))
  
  -- data CardinalDimFlag : ℕ → ℕ → Set where
  --   dimEq : {k : ℕ} → CardinalDimFlag k k 
  --   dimSucc : {k : ℕ} → CardinalDimFlag (suc k) k
  --   dimLt : {n k : ℕ} → suc n ≤ k → CardinalDimFlag n k
  --   dimDblSucc : {n k : ℕ} → suc (suc k) ≤ n → CardinalDimFlag n k

  -- getFlag : (n : ℕ) → (k : ℕ) → CardinalDimFlag n k
  -- getFlag zero zero = dimEq
  -- getFlag zero (suc k) = dimLt (s≤s z≤n)
  -- getFlag (suc zero) zero = dimSucc
  -- getFlag (suc (suc n)) zero = dimDblSucc (s≤s (s≤s z≤n))
  -- getFlag (suc n) (suc k) with getFlag n k
  -- getFlag (suc n) (suc .n) | dimEq = dimEq
  -- getFlag (suc .(suc k)) (suc k) | dimSucc = dimSucc
  -- getFlag (suc n) (suc k) | dimLt sn≤k = dimLt (s≤s sn≤k)
  -- getFlag (suc n) (suc k) | dimDblSucc ssk≤n = dimDblSucc (s≤s ssk≤n)

  -- extrudeDispatch : {n k : ℕ} → {A B : Set} → A → A → Tree k B → CardinalAddress k → CardinalDimFlag n k → CardinalNesting n A → Maybe (CardinalNesting n A)
  -- extrudeDispatch {k = zero} a₀ a₁ msk ca dimEq cn = just (pt (int a₀ cn))
  -- extrudeDispatch {k = suc k} a₀ a₁ msk ca dimEq cn = extrudeNestingAt a₀ cn ca msk
  -- extrudeDispatch a₀ a₁ msk ca dimSucc cn = doFiller a₁ cn ca msk
  -- extrudeDispatch a₀ a₁ msk ca (dimLt sn≤k) cn = just cn
  -- extrudeDispatch a₀ a₁ msk ca (dimDblSucc (s≤s (s≤s k≤n))) cn = doTail k≤n cn ca msk

  -- dropDispatch : {n k : ℕ} → {A : Set} → A → A → CardinalAddress k → CardinalDimFlag n (suc k) → CardinalNesting n A → Maybe (CardinalNesting n A)
  -- dropDispatch a₀ a₁ ca dimEq cn = {!!} -- extrudeLoopAt a₀ cn ca
  -- dropDispatch a₀ a₁ ca dimSucc cn = extrudeDropAt a₁ cn ca
  -- dropDispatch a₀ a₁ ca (dimLt sn≤sk) cn = just cn
  -- dropDispatch a₀ a₁ ca (dimDblSucc 3pk≤n) cn = padWithDropLeaf 3pk≤n cn ca

  -- traverseCardinal : {n : ℕ} → {A : Set} → ((m : ℕ) → CardinalNesting m A → Maybe (CardinalNesting m A)) → Cardinal n A → Maybe (Cardinal n A)
  -- traverseCardinal {zero} f (∥ ▶ hd) = f 0 hd >>= (λ newHd → just (∥ ▶ newHd))
  -- traverseCardinal {suc n} f (c ▶ hd) = traverseCardinal {n} f c >>= (λ newTl → f (suc n) hd >>= (λ newHd → just (newTl ▶ newHd)))

  -- doExtrude : {n k : ℕ} → {A B : Set} → A → A → Tree k B → CardinalAddress k → Cardinal n A → Maybe (Cardinal n A)
  -- doExtrude {k = k} a₀ a₁ msk ca c = traverseCardinal (λ m → extrudeDispatch a₀ a₁ msk ca (getFlag m k)) c

  -- doDrop : {n k : ℕ} → {A : Set} → A → A → CardinalAddress k → Cardinal n A → Maybe (Cardinal n A)
  -- doDrop {k = k} a₀ a₁ ca c = traverseCardinal (λ m → dropDispatch a₀ a₁ ca (getFlag m (suc k))) c

  -- doRootExtrusion : {n : ℕ} → {A : Set} → (k : ℕ) → (k≤n : k ≤ n) → A → A → Cardinal (suc n) A → Maybe (Cardinal (suc n) A)
  -- doRootExtrusion {n} {A} k k≤n a₀ a₁ c = rootTree {k} {A} (getAt k k≤n (tail c)) >>= (λ tr → doExtrude a₀ a₁ tr (rootAddr k) c)

  -- -- A bit weird, since the upper information passed to extend gets thrown away
  -- doTopRootExtrusion : {n : ℕ} → {A : Set} → A → A → Cardinal n A → Maybe (Cardinal (suc n) A)
  -- doTopRootExtrusion {n} a₀ a₁ c = doRootExtrusion n ≤-refl a₀ a₁ (extend a₁ c)

  -- extrudeSelection : {n : ℕ} → {A : Set} → (k : ℕ) → (k≤n : k ≤ n) → A → A → (A → Bool) → CardinalAddress (suc k) → Cardinal (suc n) A → Maybe (Cardinal (suc n) A)
  -- extrudeSelection k k≤n a₀ a₁ p ca c = 
  --   findSelectionMask (getAt (suc k) (s≤s k≤n) c) ca p 
  --   >>= (λ msk → doExtrude a₀ a₁ msk ca c)

