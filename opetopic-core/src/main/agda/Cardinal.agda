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

  cardinalAddressComplete : {n : ℕ} → CardinalAddress n → Address (suc n)
  cardinalAddressComplete {zero} (∥ ▶ tt) = tt ∷ []
  cardinalAddressComplete {suc n} (tl ▶ hd) = cardinalAddressComplete tl ∷ hd ∷ [] 

  CardinalDerivative : Set → ℕ → Set
  CardinalDerivative A zero = ⊤
  CardinalDerivative A (suc n) = CardinalDerivative (Tree A (suc n)) n × Derivative A (suc n)

  CardinalNesting : Set → ℕ → Set
  CardinalNesting A n = CardinalTree (Nesting A n) n 

  Cardinal : (ℕ → Set) → ℕ → Set
  Cardinal A n = Suite (λ k → CardinalNesting (A k) k) (suc n)

  open Monad errorM

  poke : {A : Set} → {n : ℕ} → CardinalTree A n → CardinalAddress n → Error (A × CardinalDerivative A n)
  poke {n = zero} (pt a) ca = succeed (a , tt)
  poke {n = suc n} ct (tl ▶ hd) = 
    poke {n = n} ct tl 
    >>= (λ { (tr , ∂) → seekTo tr hd 
                        >>= (λ { (leaf , cntxt) → fail "Error in poke" ; 
                                 (node a sh , cntxt) → η (a , ∂ , (sh , cntxt)) }) })

  tailDerivative : {A : Set} → {n k : ℕ} → (k≤n : k ≤ n) → CardinalTree A (suc n) → CardinalAddress k → 
                   Error ((TreeSeq A (suc k) (Δ k≤n)) × CardinalDerivative (TreeSeq A (suc k) (Δ k≤n)) k)
  tailDerivative k≤n ct ca = poke (coe (cardinalTreeAssoc k≤n) ct) ca
                   
  plugCardinal : {A : Set} → {n : ℕ} → CardinalDerivative A n → A → CardinalTree A n
  plugCardinal {n = zero} cd a = pt a
  plugCardinal {n = suc n} (cd , ∂) a = plugCardinal cd (∂ ← a)

  traverseCardinalTree : {G : Set → Set} → ⦃ isA : Applicative G ⦄ → {A B : Set} → {n : ℕ} → CardinalTree A n → (A → G B) → G (CardinalTree B n)
  traverseCardinalTree {n = zero} (pt a) f = pure pt ⊛ f a
    where open Applicative ⦃ ... ⦄
  traverseCardinalTree {n = suc n} ct f = 
    traverseCardinalTree {n = n} ct (λ tr → traverseTree tr f)

  mapCardinalTree : {A B : Set} → {n : ℕ} → CardinalTree A n → (A → B) → CardinalTree B n
  mapCardinalTree {n = n} = traverseCardinalTree ⦃ idA ⦄ {n = n}

  traverseCardinalTreeWithAddr : {G : Set → Set} → ⦃ isA : Applicative G ⦄ → {A B : Set} → {n : ℕ} → 
                                 CardinalTree A n → (A → CardinalAddress n → G B) → G (CardinalTree B n)
  traverseCardinalTreeWithAddr {n = zero} (pt a) f = pure pt ⊛ f a (∥ ▶ tt)
    where open Applicative ⦃ ... ⦄ 
  traverseCardinalTreeWithAddr {n = suc n} ct f = 
    traverseCardinalTreeWithAddr {n = n} ct (λ tr ca → 
      traverseWithAddress tr (λ a addr → f a (ca ▶ addr))
    )

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

  data Polarity (A : Set) : Set where
    pos : Polarity A
    neg : Polarity A
    neutral : A → Polarity A

  completeWith : {A : Set} → {n : ℕ} → CardinalTree A n → A → Tree A n
  completeWith {n = zero} ct a = ct
  completeWith {n = suc n} ct a = node a (completeWith {n = n} ct leaf)

  toShell : {A : Set} → {n : ℕ} → CardinalTree A (suc n) → Tree (Tree A (suc n)) n
  toShell ct = completeWith ct leaf

  toComplex : {A : ℕ → Set} → {n : ℕ} → Cardinal A n → Complex (λ k → Polarity (A k)) n
  toComplex {n = zero} (∥ ▶ pt (nst)) = ∥ ▶ (box pos (pt (mapNesting nst neutral)))
  toComplex {A} {n = suc n} (tl ▶ hd) = toComplex tl ▶ box pos (node (dot neg) shell)
    where shell : Tree (Tree (Nesting (Polarity (A (suc n))) (suc n)) (suc n)) n
          shell = toShell {n = n} (mapCardinalTree {n = suc n} hd (λ nst → mapNesting nst neutral))

  completeToComplex : {A : ℕ → Set} → {n : ℕ} → Cardinal A n → Suite (λ k → (A k × A k)) (suc n) → Complex A n
  completeToComplex {n = zero} (∥ ▶ hd) (∥ ▶ (_ , a₊)) = ∥ ▶ (box a₊ hd)
  completeToComplex {n = suc n} (tl ▶ hd) (ps ▶ (a₋ , a₊)) = 
    completeToComplex tl ps ▶ box a₊ (node (dot a₋) (toShell {n = n} hd))

  complexToCardinal₀ : {A : ℕ → Set} → {n : ℕ} → Complex A n → (Cardinal A n × Derivative (Nesting (A (suc n)) (suc n)) (suc n))
  complexToCardinal₀ {n = zero} (∥ ▶ hd) = (∥ ▶ (pt hd)) , (pt leaf) , []
  complexToCardinal₀ {n = suc n} (tl ▶ hd) with complexToCardinal₀ tl
  complexToCardinal₀ {A = A} {n = suc n} (tl ▶ hd) | (c₀ , (sh , _)) = nextCardinal , const (toTree (head tl)) leaf , []

    where nextCardinal : Cardinal A (suc n)
          nextCardinal = c₀ ▶ mapCardinalTree {A = Nesting (A n) n} 
                                {B = Tree (Nesting (A (suc n)) (suc n)) (suc n)} {n} (head c₀) 
                                  (λ nst → node hd sh)

  complexToCardinal : {A : ℕ → Set} → {n : ℕ} → Complex A n → Cardinal A n
  complexToCardinal c = proj₁ (complexToCardinal₀ c)

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

  getSelection : {A : Set} → {n : ℕ} → CardinalNesting A n → CardinalAddress n → (A → Bool) → Error (Tree (Nesting A n) n)
  getSelection {n = zero} (pt nst) ca p = if p (baseValue nst) then succeed (pt (nst)) else fail "Nothing selected"
  getSelection {n = suc n} cn (tl ▶ hd) p = 
    poke cn tl 
    >>= (λ { (tr , ∂) → seekTo tr hd 
    >>= (λ { (fcs , cntxt) → succeed (takeWhile fcs (λ nst → p (baseValue nst))) }) })

  sproutAt : {A : Set} → {n : ℕ} → CardinalNesting A n → CardinalAddress (suc n) → A → Error (CardinalNesting A n)
  sproutAt {n = zero} (pt nst) (tl ▶ hd) a = 
    seekToNesting nst hd 
    >>= (λ { (obj a₁ , cs) → succeed (pt (closeNesting cs (box a₁ (pt (obj a))))) 
           ; (box _ _ , cntxt) → fail "Cannot sprout from a box" })
  sproutAt {n = suc n} cn (tl ▶ hd) a = 
    poke cn tl 
    >>= (λ { (nst , ∂) → seekToNesting nst hd 
    >>= (λ { (dot a₁ , cs) → let sh = maybeRec (headMaybe cs) (const (proj₁ (proj₂ ∂)) leaf) (λ c → const (proj₁ (proj₂ c)) leaf) in 
                  succeed (plugCardinal ∂ (closeNesting cs (box a₁ (node (dot a) sh))))  
           ; (box _ _ , cs) → fail "Cannot sprout from a box" }) })

  sproutFillerAt : {A : Set} → {n : ℕ} → CardinalNesting A (suc n) → CardinalAddress (suc n) → A → Error (CardinalNesting A (suc n))
  sproutFillerAt {n = zero} cn (tl ▶ hd) a = 
    poke cn tl 
    >>= (λ { (tr , ∂) → seekTo tr hd 
    >>= (λ { (leaf , cs) → succeed (plugCardinal ∂ (cs ↓ node (dot a) (pt leaf))) 
           ; (node _ _ , cs) → fail "Expected a leaf here ..." }) })
  sproutFillerAt {n = suc n} cn (tl ▶ hd) a = 
    poke {A = Tree (Nesting _ (2 + n)) (2 + n)} {n = suc n} cn tl 
    >>= (λ { (tr , ∂) → seekTo tr hd 
    >>= (λ { (leaf , cs) → let sh = maybeRec (headMaybe cs) (const (proj₁ (proj₂ ∂)) leaf) (λ c → const (proj₁ (proj₂ c)) leaf) in 
                   succeed (plugCardinal ∂ (cs ↓ node (dot a) (node leaf sh))) 
           ; (node _ _ , cs) → fail "Exprected a leaf here ..." }) })

  sproutLeafAt : {A : Set} → {n k : ℕ} → (2pk≤n : 2 + k ≤ n) → CardinalNesting A n → CardinalAddress (suc k) → Error (CardinalNesting A n)
  sproutLeafAt {A} (s≤s (s≤s (z≤n {n}))) cn (tl ▶ hd) = 
    tailDerivative {A = Nesting A (2 + n)} {n = suc n} {k = 0} (z≤n {suc n}) cn tl
    >>= (λ { (seq , ∂) → seekTo seq hd 
    >>= (λ { (leaf , cs) → succeed (coe! (cardinalTreeAssoc (s≤s (z≤n {n}))) (plugCardinal ∂ (cs ↓ node (seqLeaf {_} {1} {n}) (pt leaf)))) 
           ; (node _ _ , cs) → fail "Expected leaf here" }) })
  sproutLeafAt {A} (s≤s (s≤s (s≤s {k} {n} k≤n))) cn (tl ▶ hd) = 
    tailDerivative {A = Nesting A (3 + n)} {n = suc (suc n)} {k = suc k} (s≤s (≤-suc k≤n)) cn tl 
    >>= (λ { (seq , ∂) → seekTo (transport P (Δ-lem k≤n) seq) hd 
    >>= (λ { (leaf , cs) → let sh = maybeRec (headMaybe cs) (const (proj₁ (proj₂ ∂)) leaf) (λ c → const (proj₁ (proj₂ c)) leaf) in 
                   succeed (coe! (cardinalTreeAssoc (s≤s (≤-suc k≤n))) 
                                   (plugCardinal ∂ (transport! P (Δ-lem k≤n) (cs ↓ node (seqLeaf {_} {2 + k} {Δ k≤n}) (node leaf sh) )))) 
           ; (node _ _ , cs) → fail "Exprected leaf here" }) })

    where P : ℕ → Set
          P m = TreeSeq (Nesting A (suc (suc (suc n)))) (suc (suc k)) m 

  extrudeAt : {A B : Set} → {n : ℕ} → CardinalNesting A n → CardinalAddress n → Tree B n → A → Error (CardinalNesting A n)
  extrudeAt {n = zero} cn ca msk a = succeed (pt (box a cn))
  extrudeAt {n = suc n} cn (tl ▶ hd) msk a = 
    poke cn tl 
    >>= (λ { (tr , ∂) → seekTo tr hd 
    >>= (λ { (fcs , cntxt) → exciseWithMask fcs msk 
    >>= (λ { (cn , verts) → succeed (plugCardinal ∂ (cntxt ↓ node (box a cn) verts)) }) }) })

  extrudeFillerAt : {A B : Set} → {n : ℕ} → CardinalNesting A (suc n) → CardinalAddress n → Tree B n → A → Error (CardinalNesting A (suc n))
  extrudeFillerAt {n = zero} cn (∥ ▶ hd) msk a = succeed (pt (node (dot a) cn))
  extrudeFillerAt {n = suc n} cn (tl ▶ hd) msk a = 
    poke cn tl 
    >>= (λ { (tr , ∂) → seekTo tr hd 
    >>= (λ { (fcs , cntxt) → exciseWithMask fcs msk 
    >>= (λ { (cut , cutSh) → succeed (plugCardinal ∂ (cntxt ↓ node (node (dot a) cut) cutSh)) }) }) })

  extrudeLeafAt : {A B : Set} → {n k : ℕ} → (2pk≤n : 2 + k ≤ n) → CardinalNesting A n → CardinalAddress k → Tree B k → Error (CardinalNesting A n)
  extrudeLeafAt {A} {B} (s≤s (s≤s (z≤n {n}))) cn ca msk = 
    tailDerivative {A = Nesting A (2 + n)} {n = suc n} {k = 0} (z≤n {suc n}) cn ca 
    >>= (λ { (seq , ∂) → succeed (coe! (cardinalTreeAssoc (s≤s (z≤n {n}))) (plugCardinal ∂ (node (seqLeaf {_} {1} {n}) (pt seq)))) })
  extrudeLeafAt {A} (s≤s (s≤s (s≤s {k} {n} k≤n))) cn (tl ▶ hd) msk = 
    tailDerivative {A = Nesting A (3 + n)} {n = suc (suc n)} {k = k} (≤-suc (≤-suc k≤n)) cn tl 
    >>= (λ { (seq , ∂) → seekTo (transport P p seq) hd 
    >>= (λ { (fcs , cntxt) → exciseWithMask fcs msk 
    >>= (λ { (cut , cutSh) → succeed (coe! (cardinalTreeAssoc (≤-suc (≤-suc k≤n))) 
                                     (plugCardinal ∂ (transport! P p (cntxt ↓ node (node (seqLeaf {_} {2 + k} {Δ k≤n}) cut) cutSh)))) 
           }) }) })

    where p : Δ (≤-suc (≤-suc k≤n)) == suc (suc (Δ k≤n))
          p = Δ-lem (≤-suc k≤n) ∙ (ap suc (Δ-lem k≤n))

          P : ℕ → Set
          P m = TreeSeq (Nesting A (suc (suc (suc n)))) (suc k) m 

          Q : ℕ → Set
          Q m = CardinalTree (Nesting A (suc (suc (suc n)))) m 


  extrudeLoopAt : {A : Set} → {n : ℕ} → CardinalNesting A (suc n) → CardinalAddress n → A → Error (CardinalNesting A (suc n))
  extrudeLoopAt {n = zero} cn ca a = succeed (pt (node (box a leaf) cn))
  extrudeLoopAt {n = suc n} cn ca a = 
    poke cn ca >>= (λ { (tr , ∂) → succeed (plugCardinal ∂ (node (box a leaf) (node tr (const (proj₁ (proj₂ ∂)) leaf)))) })

  extrudeDropAt : {A : Set} → {n : ℕ} → CardinalNesting A (suc (suc n)) → CardinalAddress n → A → Error (CardinalNesting A (suc (suc n)))
  extrudeDropAt {n = zero} cn ca a = succeed (pt (node (node (dot a) leaf) cn))
  extrudeDropAt {n = suc n} cn ca a = 
    poke cn ca >>= (λ { (tr , ∂) → succeed (plugCardinal ∂ (node (node (dot a) leaf) (node tr (const (proj₁ (proj₂ ∂)) leaf)))) })

  extrudeDropLeafAt : {A : Set} → {n k : ℕ} → (3pk≤n : 3 + k ≤ n) → CardinalNesting A n → CardinalAddress k → Error (CardinalNesting A n)
  extrudeDropLeafAt {A} (s≤s (s≤s (s≤s (z≤n {n})))) cn ca = 
    tailDerivative {n = suc (suc n)} {k = 0} z≤n cn ca 
    >>= (λ { (seq , ∂) → succeed (coe! (cardinalTreeAssoc {Nesting A (3 + n)} {2 + n} {2} (s≤s (s≤s (z≤n {n})))) 
        (plugCardinal {TreeSeq (Nesting A (3 + n)) 1 (2 + n)} {0} ∂ (node (node (seqLeaf {_} {2} {n}) leaf) (pt seq)))) })
  extrudeDropLeafAt {A} (s≤s (s≤s (s≤s (s≤s {k} {n} k≤n)))) cn ca = 
    tailDerivative {A = Nesting A (4 + n)} {n = suc (suc (suc n))} {k = suc k} (≤-suc (≤-suc (s≤s k≤n))) cn ca 
    >>= (λ { (seq , ∂) → succeed (coe! (cardinalTreeAssoc {Nesting A (4 + n)} {3 + n} {1 + k}  (≤-suc (≤-suc (s≤s k≤n)))) 
             (plugCardinal {TreeSeq (Nesting A (4 + n)) (2 + k) (Δ (≤-suc (≤-suc k≤n)))} {1 + k} ∂ (transport! P p (node (node (seqLeaf {_} {3 + k} {Δ k≤n}) (leaf {n = 1 + k})) 
                                                  (node (transport P p seq) (const (transport Q p (proj₁ (proj₂ ∂))) leaf)))))) })

    where p : Δ (≤-suc (≤-suc k≤n)) == suc (suc (Δ k≤n))
          p = Δ-lem (≤-suc k≤n) ∙ (ap suc (Δ-lem k≤n))

          P : ℕ → Set
          P m = TreeSeq (Nesting A (4 + n)) (suc (suc k)) m 

          Q : ℕ → Set
          Q m = Tree (Tree (P m) (suc k)) k 


  extrudeDispatch : {A : ℕ → Set} → {B : Set} → {n k : ℕ} → CardinalNesting (A n) n → CardinalAddress k → 
                    A k → A (1 + k) → Tree B k → CardinalDimFlag n k → Error (CardinalNesting (A n) n)
  extrudeDispatch cn ca a₀ a₁ msk (dimLt sn≤k)       = succeed cn
  extrudeDispatch cn ca a₀ a₁ msk dimEq              = extrudeAt cn ca msk a₀
  extrudeDispatch cn ca a₀ a₁ msk dimSucc            = extrudeFillerAt cn ca msk a₁
  extrudeDispatch cn ca a₀ a₁ msk (dimDblSucc 2pk≤n) = extrudeLeafAt 2pk≤n cn ca msk

  dropDispatch : {A : ℕ → Set} → {n k : ℕ} → CardinalNesting (A n) n → CardinalAddress k → 
                 A (1 + k) → A (2 + k) → CardinalDimFlag n (1 + k) → Error (CardinalNesting (A n) n)
  dropDispatch cn ca a₀ a₁ (dimLt sn≤sk)      = succeed cn
  dropDispatch cn ca a₀ a₁ dimEq              = extrudeLoopAt cn ca a₀
  dropDispatch cn ca a₀ a₁ dimSucc            = extrudeDropAt cn ca a₁
  dropDispatch cn ca a₀ a₁ (dimDblSucc 3pk≤n) = extrudeDropLeafAt 3pk≤n cn ca

  sproutDispatch : {A : ℕ → Set} → {n k : ℕ} → CardinalNesting (A n) n → CardinalAddress (suc k) → 
                   A k → A (suc k) → CardinalDimFlag n k → Error (CardinalNesting (A n) n)
  sproutDispatch cn ca a₀ a₁ (dimLt sn≤k) = succeed cn
  sproutDispatch cn ca a₀ a₁ dimEq = sproutAt cn ca a₀
  sproutDispatch cn ca a₀ a₁ dimSucc = sproutFillerAt cn ca a₁ 
  sproutDispatch cn ca a₀ a₁ (dimDblSucc 2pk≤n) = sproutLeafAt 2pk≤n cn ca

  extrudeSelection : {A : ℕ → Set} → {n k : ℕ} → Cardinal A n → CardinalAddress k → 
                     A k → A (1 + k) → (A k → Bool) → k ≤ n → Error (Cardinal A n)
  extrudeSelection c ca a₀ a₁ p k≤n = 
    getSelection (getAt _ k≤n c) ca p 
    >>= (λ msk → traverseSuite ⦃ monadIsApp errorM ⦄ c (λ m cn → extrudeDispatch cn ca a₀ a₁ msk (getFlag m _)))

  sproutAtAddress : {A : ℕ → Set} → {n k : ℕ} → Cardinal A n → CardinalAddress (suc k) → A k → A (1 + k) → (k≤n : k ≤ n) → Error (Cardinal A n)
  sproutAtAddress {A} {k = k} c ca a₀ a₁ k≤n = --traverseSuite ⦃ monadIsApp errorM ⦄ c (λ m cn → sproutDispatch {A = A} cn ca a₀ a₁ (getFlag m _))
    let cn : CardinalTree (Nesting (A k) k) k
        cn = getAt _ k≤n c
    in {!!}
    -- poke cn (tail ca) 
    --    >>= (λ { (nst , ∂) → fromTree (toTree nst) 
    --    >>= (λ addrNst → seekToNesting addrNst (head ca) 
    --    >>= (λ { (fcs , cs) → succeed (baseValue fcs) -- A hack to pattern match the guy below
    --    >>= (λ { (inj₁ _) → fail "Failed to calculate shell address" 
    --           ; (inj₂ addr) → traverseSuite ⦃ monadIsApp errorM ⦄ c (λ m cn → sproutDispatch {A = A} cn ca a₀ a₁ (getFlag m _)) }) })) }) 

  dropAtAddress : {A : ℕ → Set} → {n k : ℕ} → Cardinal A n → CardinalAddress k → A (1 + k) → A (2 + k) → (k≤n : k ≤ n) → Error (Cardinal A n)
  dropAtAddress {A} c ca a₀ a₁ k≤n = traverseSuite ⦃ monadIsApp errorM ⦄ c (λ m cn → dropDispatch {A = A} cn ca a₀ a₁ (getFlag m _))

  -- doRootExtrusion : {n : ℕ} → {A : Set} → (k : ℕ) → (k≤n : k ≤ n) → A → A → Cardinal (suc n) A → Maybe (Cardinal (suc n) A)
  -- doRootExtrusion {n} {A} k k≤n a₀ a₁ c = rootTree {k} {A} (getAt k k≤n (tail c)) >>= (λ tr → doExtrude a₀ a₁ tr (rootAddr k) c)

  -- -- A bit weird, since the upper information passed to extend gets thrown away
  -- doTopRootExtrusion : {n : ℕ} → {A : Set} → A → A → Cardinal n A → Maybe (Cardinal (suc n) A)
  -- doTopRootExtrusion {n} a₀ a₁ c = doRootExtrusion n ≤-refl a₀ a₁ (extend a₁ c)


