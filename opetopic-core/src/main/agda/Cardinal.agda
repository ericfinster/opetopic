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

  --
  --  Normal Extrusion Routines
  -- 

  getSelection : {A : Set} → {n : ℕ} → CardinalNesting A n → CardinalAddress n → (A → Bool) → Error (Tree (Nesting A n) n)
  getSelection {n = zero} (pt nst) ca p = if p (baseValue nst) then succeed (pt (nst)) else fail "Nothing selected"
  getSelection {n = suc n} cn (tl ▶ hd) p = 
    poke cn tl 
    >>= (λ { (tr , ∂) → seekTo tr hd 
    >>= (λ { (fcs , cntxt) → succeed (takeWhile fcs (λ nst → p (baseValue nst))) }) })

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


  --
  --  Drop extrusion routines
  --

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


  --
  --  Sprouting routines
  -- 

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

  sproutFillerAt : {A : Set} → {n : ℕ} → CardinalNesting A (suc n) → CardinalAddress (suc n) → A → Error (CardinalNesting A (suc n) × Address (suc n))
  sproutFillerAt {n = zero} cn (tl ▶ hd) a = 
    poke cn tl 
    >>= (λ { (tr , ∂) → canopyAddressExtend tr 
    >>= (λ addrNst → seekToNesting addrNst hd 
    >>= (λ { (obj addr , _) → seekTo tr hd 
                              >>= (λ { (leaf , cs) → succeed (plugCardinal ∂ (cs ↓ node (dot a) (pt leaf)) , addr) 
                                     ; (node _ _ , cs) → fail "Expected a leaf here ..." })
           ; (box _ _ , _) → fail "Could not calculate modified address" })) })
  sproutFillerAt {n = suc n} cn (tl ▶ hd) a = 
    poke {A = Tree (Nesting _ (2 + n)) (2 + n)} {n = suc n} cn tl 
    >>= (λ { (tr , ∂) → canopyAddressExtend tr 
    >>= (λ { addrNst → seekToNesting addrNst hd 
    >>= (λ { (dot addr , _) → seekTo tr hd 
                              >>= (λ { (leaf , cs) → let sh = maybeRec (headMaybe cs) (const (proj₁ (proj₂ ∂)) leaf) (λ c → const (proj₁ (proj₂ c)) leaf) 
                                                     in succeed (plugCardinal ∂ (cs ↓ node (dot a) (node leaf sh)) , addr) 
                                     ; (node _ _ , cs) → fail "Exprected a leaf here ..." })
           ; (box _ _ , _) → fail "Could not calculate modified address" }) }) })


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

  IndexedCardinal : (ℕ → Set) → ℕ → Set
  IndexedCardinal A k = CardinalNesting (A k) k

  cardinalSplit : {A : ℕ → Set} → {n k : ℕ} → (sk≤n : 1 + k ≤ n) → Cardinal A n → 
                  Suite (IndexedCardinal A) k × IndexedCardinal A k × IndexedCardinal A (1 + k) × Suite (IndexedCardinal A ↑ (2 + k)) (Δ sk≤n)
  cardinalSplit {A} {n} {k} sk≤n c = 
    let (pre₀ , post₀) = grab (Δ-≤-lem (s≤s sk≤n)) c 
        pre = transport (Suite (IndexedCardinal A)) lem₀ pre₀
        post = transport (λ m → Suite (IndexedCardinal A ↑ m) (Δ sk≤n)) lem₁ post₀
        preTail = tail pre
    in tail preTail , head preTail , head pre , post

    where lem₀ : (Δ (Δ-≤-lem (s≤s sk≤n))) == (2 + k)
          lem₀ = Δ-≤-lem-eq (s≤s sk≤n)

          lem₁ : Δ (≤-suc (Δ-≤-lem sk≤n)) == (2 + k)
          lem₁ = Δ-lem (Δ-≤-lem sk≤n) ∙ ap suc (Δ-≤-lem-eq sk≤n)

  cardinalJoin : {A : ℕ → Set} → {n k : ℕ} → (sk≤n : 1 + k ≤ n) → 
                 Suite (IndexedCardinal A) k × IndexedCardinal A k × IndexedCardinal A (1 + k) × Suite (IndexedCardinal A ↑ (2 + k)) (Δ sk≤n) → 
                 Cardinal A n
  cardinalJoin {A} {n} {k} sk≤n (c₀ , c₁ , c₂ , c₃) = 
    transport (Suite (IndexedCardinal A)) lem (smash (c₀ ▶ c₁ ▶ c₂) c₃)

    where lem : (2 + (k + Δ sk≤n)) == suc n
          lem = 2 + (k + Δ sk≤n)            =⟨ idp ⟩ 
                1 + ((1 + k) + Δ sk≤n)      =⟨ +-sym {1 + k} {Δ sk≤n} |in-ctx (λ k → 1 + k) ⟩ 
                1 + (Δ sk≤n + (1 + k))      =⟨ Δ-+-lem sk≤n |in-ctx (λ k → 1 + k) ⟩ 
                1 + n ∎

  extrudeSelection : {A : ℕ → Set} → {n k : ℕ} → Cardinal A n → CardinalAddress k → 
                        A k → A (1 + k) → (A k → Bool) → (1 + k ≤ n) → Error (Cardinal A n)
  extrudeSelection {A} {n} {k} c ca a₀ a₁ p sk≤n with cardinalSplit sk≤n c
  extrudeSelection {A} {n} {k} c ca a₀ a₁ p sk≤n | (c₀ , c₁ , c₂ , c₃) = 
    getSelection c₁ ca p 
    >>= (λ msk → extrudeAt c₁ ca msk a₀   -- The first two steps can be done simultaneously now ...
    >>= (λ c₁' → extrudeFillerAt c₂ ca msk a₁ 
    >>= (λ c₂' → traverseSuite ⦃ monadIsApp errorM ⦄ c₃ (λ m cn → extrudeLeafAt (s≤s (s≤s +-≤-lem)) cn ca msk)
    >>= (λ c₃' → succeed (cardinalJoin sk≤n (c₀ , c₁' , c₂' , c₃'))))))

  sproutAtAddress : {A : ℕ → Set} → {n k : ℕ} → Cardinal A n → CardinalAddress (suc k) → A k → A (1 + k) → (sk≤n : 1 + k ≤ n) → Error (Cardinal A n)
  sproutAtAddress {A} {n} {k} c ca a₀ a₁ sk≤n with cardinalSplit sk≤n c
  sproutAtAddress {A} {n} {k} c ca a₀ a₁ sk≤n | (c₀ , c₁ , c₂ , c₃) = 
    sproutAt c₁ ca a₀ 
    >>= (λ c₁' → sproutFillerAt c₂ ca a₁ 
    >>= (λ { (c₂' , addr) → traverseSuite {{monadIsApp errorM}} c₃ (λ m cn → sproutLeafAt (s≤s (s≤s +-≤-lem)) cn (tail ca ▶ addr)) 
    >>= (λ c₃' → succeed (cardinalJoin sk≤n (c₀ , c₁' , c₂' , c₃'))) }))  

  dropAtAddress : {A : ℕ → Set} → {n k : ℕ} → Cardinal A n → CardinalAddress k → A (1 + k) → A (2 + k) → (2 + k ≤ n) → Error (Cardinal A n)
  dropAtAddress {A} {n} {k} c ca a₀ a₁ ssk≤n with cardinalSplit ssk≤n c
  dropAtAddress {A} {n} {k} c ca a₀ a₁ ssk≤n | (c₀ , c₁ , c₂ , c₃) = 
    extrudeLoopAt c₁ ca a₀ 
    >>= (λ c₁' → extrudeDropAt c₂ ca a₁ 
    >>= (λ c₂' → traverseSuite {{monadIsApp errorM}} c₃ (λ m cn → extrudeDropLeafAt (s≤s (s≤s (s≤s +-≤-lem))) cn ca) 
    >>= (λ c₃' → succeed (cardinalJoin ssk≤n (c₀ , c₁' , c₂' , c₃')))))


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

  -- doRootExtrusion : {n : ℕ} → {A : Set} → (k : ℕ) → (k≤n : k ≤ n) → A → A → Cardinal (suc n) A → Maybe (Cardinal (suc n) A)
  -- doRootExtrusion {n} {A} k k≤n a₀ a₁ c = rootTree {k} {A} (getAt k k≤n (tail c)) >>= (λ tr → doExtrude a₀ a₁ tr (rootAddr k) c)

  -- -- A bit weird, since the upper information passed to extend gets thrown away
  -- doTopRootExtrusion : {n : ℕ} → {A : Set} → A → A → Cardinal n A → Maybe (Cardinal (suc n) A)
  -- doTopRootExtrusion {n} a₀ a₁ c = doRootExtrusion n ≤-refl a₀ a₁ (extend a₁ c)


