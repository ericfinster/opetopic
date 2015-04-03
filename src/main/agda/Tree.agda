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


  traverseTree : {A B : Set} → {G : Set → Set} → {n : ℕ} → Applicative G → Tree A n → (A → G B) → G (Tree B n)
  traverseTree apG (pt a) f = let open Applicative apG in pure pt ⊛ f a
  traverseTree apG leaf f = let open Applicative apG in pure leaf
  traverseTree apG (node a sh) f = let open Applicative apG in 
    pure node ⊛ f a ⊛ traverseTree apG sh (λ b → traverseTree apG b f)

  traverseWithAddress : {A B : Set} → {G : Set → Set} → {n : ℕ} → Applicative G → Tree A n → (A → Address n → G B) → G (Tree B n)
  traverseWithAddress {G = G} apG tr f = traverseWithAddress₀ tr (rootAddr _) f

    where open Applicative apG

          traverseWithAddress₀ : {A B : Set} → {n : ℕ} → Tree A n → Address n → (A → Address n → G B) → G (Tree B n)
          traverseWithAddress₀ (pt a) base f = pure pt ⊛ f a base
          traverseWithAddress₀ leaf base f = pure leaf
          traverseWithAddress₀ (node a sh) base f = 
            pure node ⊛ f a base ⊛ traverseWithAddress apG sh (λ b d → traverseWithAddress₀ b (d ∷ base) f)

  mapTree : {A B : Set} → {n : ℕ} → Tree A n → (A → B) → Tree B n
  mapTree tr f = traverseTree idA tr f

  mapWithAddress : {A B : Set} → {n : ℕ} → Tree A n → (A → Address n → B) → Tree B n
  mapWithAddress tr f = traverseWithAddress idA tr f

  const : {A B : Set} → {n : ℕ} → Tree A n → B → Tree B n
  const tr b = mapTree tr (λ _ → b)

  unzip : {n : ℕ} → {A B : Set} → Tree (A × B) n → (Tree A n × Tree B n)
  unzip (pt (a , b)) = (pt a , pt b)
  unzip leaf = leaf , leaf
  unzip (node (a , b) shAB) with unzip (mapTree shAB unzip)
  unzip (node (a , b) shAB) | (shA , shB) = (node a shA , node b shB)

  traverseWithLocalData : {A B C : Set} → {G : Set → Set} → {n : ℕ} → Applicative G → 
                          Tree A n → (A → Address n → Derivative B n → G C) → G (Tree C n)
  traverseWithLocalData {G = G} apG tr f = traverseWithLocalData₀ tr (rootAddr _) f

    where open Applicative apG

          traverseWithLocalData₀ : {A B C : Set} → {n : ℕ} → Tree A n → Address n → (A → Address n → Derivative B n → G C) → G (Tree C n)
          traverseWithLocalData₀ (pt a) base f = pure pt ⊛ f a base tt
          traverseWithLocalData₀ leaf base f = pure leaf
          traverseWithLocalData₀ (node a sh) base f = 
            pure node ⊛ f a base (const sh leaf , []) ⊛ traverseWithAddress apG sh (λ b d → traverseWithLocalData₀ b (d ∷ base) f)
  

  module Matching {M : Set → Set} (isError : MonadError M) where

    open MonadError isError
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

  open Matching

  join : {A : Set} → {M : Set → Set} → {n : ℕ} → MonadError M → Tree (Tree A n) n → M (Tree A n)

  module GraftRec {A B : Set} {M : Set → Set} (isE : MonadError M) where

    open MonadError isE

    apM : Applicative M
    apM = monadIsApp isMonad

    module PositiveDim {n : ℕ} (ν-rec : (A → Tree B (suc n) → M B)) (λ-rec : (Address (suc n) → M B)) where

      unzipAndJoin : Tree (Tree B (suc n) × Tree (Address (suc n)) n) n → M (Tree (Tree B (suc n)) n × Tree (Address (suc n)) n)
      unzipAndJoin zt with unzip zt
      unzipAndJoin zt | (bSh , adrJnSh) = 
        join isE adrJnSh 
        >>= (λ adTr → η (bSh , adTr))

      unzipJoinAndAppend : Tree (Tree B (suc n) × Tree (Address (suc n)) n) n → M B → M (Tree B (suc n) × Tree (Address (suc n)) n)
      unzipJoinAndAppend zt mb = 
        unzipAndJoin zt >>= (λ { (bSh , adTr) → 
        mb >>= (λ b → η (node b bSh , adTr)) })

      horizontalPass : Address (suc n) → Tree (Tree A (suc (suc n))) (suc n) → Derivative (Address (suc n)) n → M (Tree B (suc n) × Tree (Address (suc n)) n)
      horizontalPass base leaf ∂ = η (leaf , ∂ ← base)
      horizontalPass base (node leaf hsh) ∂ = 
        traverseWithLocalData apM hsh (λ hbr d ∂₀ → horizontalPass (d ∷ base) hbr ∂₀) 
        >>= (λ res → unzipJoinAndAppend res (λ-rec base))
      horizontalPass base (node (node a vsh) hsh) ∂ = 
        horizontalPass base vsh ∂ 
        >>= (λ { (bTr , adTr₀) → matchWithDerivative isE {C = Address (suc n)} horizontalPass adTr₀ hsh 
        >>= (λ res → unzipJoinAndAppend res (ν-rec a bTr)) })

      initHorizontal : A → Tree (Tree (Tree A (suc (suc n))) (suc n)) n → M (B × Tree (Address (suc n)) n) → M (B × Tree (Address (suc n)) n)
      initHorizontal a hsh m = 
        m >>= (λ { (b₀ , adTr₀) → matchWithDerivative isE {C = Address (suc n)} horizontalPass adTr₀ hsh 
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


  join = {!!}


  --   graftRecCont : {n : ℕ} → {A B : Set} → (A → Tree (suc n) B → Maybe B) → (Address (suc n) → Maybe B) →
  --                  Tree n (Address (suc n) × Derivative n (Address (suc n)) × Tree (suc n) (Tree (suc (suc n)) A)) → Maybe (Tree n (Tree (suc n) B) × Tree n (Address (suc n)))
  --   graftRecCont {n} ndRec lfRec hsh = 
  --     traverseTree maybeA (graftRecHoriz ndRec lfRec) hsh 
  --     >>= (λ trRes → let (bSh , adrJnSh) = unzip trRes in join adrJnSh 
  --     >>= (λ adSh → just (bSh , adSh))) 


  --   graftRecStart : {n : ℕ} → {A B : Set} → (A → Tree (suc n) B → Maybe B) → (Address (suc n) → Maybe B) →
  --                  Tree (suc (suc n)) A → A → Tree n (Tree (suc n) (Tree (suc (suc n)) A)) → Maybe (B × Tree n (Address (suc n)))
  --   graftRecStart ndRec lfRec leaf a hsh                    = graftRecChain ndRec lfRec a hsh (lfRec [] >>= (λ b → just (b , mapWithAddress (λ dir _ → dir ∷ []) hsh)))
  --   graftRecStart ndRec lfRec (node a₀ leaf) a hsh          = graftRecChain ndRec lfRec a hsh (ndRec a leaf >>= (λ b → just (b , mapWithAddress (λ _ _ → []) hsh)))
  --   graftRecStart ndRec lfRec (node a₀ (node v hsh₀)) a hsh = graftRecChain ndRec lfRec a hsh (graftRecStart ndRec lfRec v a₀ hsh₀)


  --   graftRec {zero} ndRec lfRec leaf = lfRec []
  --   graftRec {zero} ndRec lfRec (node hd (pt tl)) = 
  --     graftRec ndRec lfRec tl >>= (λ b → ndRec hd (pt b))
  --   graftRec {suc n} ndRec lfRec leaf = lfRec []
  --   graftRec {suc n} ndRec lfRec (node a leaf) = ndRec a leaf
  --   graftRec {suc n} ndRec lfRec (node a (node v hsh)) = 
  --     graftRecStart ndRec lfRec v a hsh >>= (λ { (b , _) → just b })


  --   graft : {n : ℕ} → {A : Set} → Tree (suc n) A → Tree n (Tree (suc n) A) → Maybe (Tree (suc n) A)
  --   graft {n} tr brs = graftRec (λ a sh → just (node a sh)) (λ addr → brs valueAt addr) tr

  -- zip : {n : ℕ} → {A B : Set} → Tree n A → Tree n B → Tree n (A × B)
  -- zip {zero} (pt a) (pt b) = pt (a , b)
  -- zip {suc n} leaf leaf = leaf
  -- zip {suc n} leaf (node a shB) = leaf
  -- zip {suc n} (node a shA) leaf = leaf
  -- zip {suc n} (node a shA) (node b shB) = 
  --   node (a , b) (mapTree (uncurry zip) (zip shA shB))

  -- treeRecWithPrefix : {n : ℕ} → {A B : Set} → 
  --                     (Address (suc n) → A → Tree n B → B) → 
  --                     (Address (suc n) → B) → Address (suc n) → Tree (suc n) A → B

  -- treeRecWithPrefix ndRec lfRec ds leaf = lfRec ds
  -- treeRecWithPrefix ndRec lfRec ds (node a sh) = 
  --   ndRec ds a (mapWithAddress (λ d b → treeRecWithPrefix ndRec lfRec (d ∷ ds) b) sh)

  -- treeRecWithAddr : {n : ℕ} → {A B : Set} → 
  --                   (Address (suc n) → A → Tree n B → B) → 
  --                   (Address (suc n) → B) → Tree (suc n) A → B
  -- treeRecWithAddr ndRec lfRec tr = treeRecWithPrefix ndRec lfRec [] tr

  -- mutual

  --   Derivative : (n : ℕ) → Set → Set
  --   Derivative zero A = ⊤
  --   Derivative (suc n) A = Tree n (Tree (suc n) A) × Context (suc n) A

  --   Context : (n : ℕ) → Set → Set
  --   Context zero A = ⊤
  --   Context (suc n) A = List (A × Derivative n (Tree (suc n) A))

  --   Zipper : (n : ℕ) → Set → Set
  --   Zipper n A = Tree n A × Context n A
  
  --   _←_ : {n : ℕ} → {A : Set} → Derivative n A → A → Tree n A
  --   _←_ {zero} tt a = pt a
  --   _←_ {suc n} (sh , context) a = context ↓ node a sh

  --   _↓_ : {n : ℕ} → {A : Set} → Context n A → Tree n A → Tree n A
  --   _↓_ {zero} tt t = t
  --   _↓_ {suc n} [] t = t
  --   _↓_ {suc n} ((a , d) ∷ c) t = c ↓ node a (d ← t)

  --   visit : {n : ℕ} → {A : Set} → Direction n → Zipper n A → Maybe (Zipper n A)
  --   visit {zero} () z
  --   visit {suc zero} [] (leaf , c) = nothing
  --   visit {suc zero} [] (node head (pt tail) , c) = just (tail , (head , tt) ∷ c)
  --   visit {suc zero} (() ∷ d) z
  --   visit {suc (suc n)} d (leaf , c) = nothing
  --   visit {suc (suc n)} d (node a sh , c) = 
  --     seek d (sh , []) 
  --     >>= (λ { (leaf , c₀) → nothing ; 
  --              (node tr hsh , c₀) → just (tr , (a , hsh , c₀) ∷ c) })

  --   seek : {n : ℕ} → {A : Set} → Address n → Zipper n A → Maybe (Zipper n A)
  --   seek [] z = just z
  --   seek (d ∷ ds) z = seek ds z >>= visit d 

  -- parent : {n : ℕ} → {A : Set} → Zipper n A → Maybe (Zipper n A)
  -- parent {zero} z = nothing
  -- parent {suc n} (fcs , []) = nothing
  -- parent {suc n} (fcs , (a , ∂) ∷ cs) = just (node a (∂ ← fcs) , cs)

  -- parentWhich : {n : ℕ} → {A : Set} → (A → Bool) → Zipper n A → Maybe (Zipper n A)
  -- parentWhich {zero} p z = nothing
  -- parentWhich {suc n} p (fcs , []) = nothing
  -- parentWhich {suc n} p (fcs , (a , ∂) ∷ cs) = 
  --   let parent = (node a (∂ ← fcs) , cs) in
  --     if p a then 
  --       just parent
  --     else 
  --       parentWhich p parent

  -- seekTo : {n : ℕ} → {A : Set} → Address n → Tree n A → Maybe (Zipper n A)
  -- seekTo {zero} addr tr = seek addr (tr , tt)
  -- seekTo {suc n} addr tr = seek addr (tr , [])

  -- globDeriv : (n : ℕ) → (B : Set) → Derivative n B
  -- globDeriv zero B = tt
  -- globDeriv (suc n) B = (globDeriv n (Tree (suc n) B) ← leaf) , []

  -- rootValue : {n : ℕ} → {A : Set} → Tree n A → Maybe A
  -- rootValue {zero} (pt a) = just a
  -- rootValue {suc n} leaf = nothing
  -- rootValue {suc n} (node a sh) = just a

  -- _valueAt_ : {n : ℕ} → {A : Set} → Tree n A → Address n → Maybe A
  -- tr valueAt addr = seekTo addr tr >>= (λ z → rootValue (proj₁ z))

  -- zipWithDeriv : {n : ℕ} → {A B : Set} → Tree n A → Tree n (Derivative n B × A)
  -- zipWithDeriv (pt a) = pt (tt , a)
  -- zipWithDeriv leaf = leaf
  -- zipWithDeriv (node a sh) = node ((const leaf sh , []) , a) (mapTree zipWithDeriv sh)

  -- extentsData : {n : ℕ} → {A : Set} → Tree n A → Tree n (Address n × Derivative n (Address (suc n)) × A)
  -- extentsData {zero} (pt a) = pt ([] , tt , a)
  -- extentsData {suc n} t = 
  --   treeRecWithAddr 
  --     (λ addr a sh → node (addr , (const leaf sh , []) , a) sh)
  --     (λ addr → leaf) t

  -- localData : {n : ℕ} → {A B : Set} → Tree n A → Tree n (Address n × Derivative n B × A)
  -- localData {zero} (pt a) = pt ([] , tt , a)
  -- localData {suc n} tr = treeRecWithAddr (λ addr a sh → node (addr , (const leaf sh , []) , a) sh) (λ addr → leaf) tr

  -- ∇ : {n : ℕ} → {A : Set} → Tree n A → Tree n (Tree n A)
  -- ∇ {n} {A} tr = mapTree (λ { (_ , ∂ , a) → ∂ ← a }) (localData {B = A} tr)

  -- corollaSetup : {n : ℕ} → {A : Set} → Tree n A → Tree n (Derivative n (Address n) × A)
  -- corollaSetup {zero} (pt a) = pt (tt , a)
  -- corollaSetup {suc n} leaf  = leaf 
  -- corollaSetup {suc n} (node a sh) = 
  --   node ((mapTree (λ _ → leaf ) sh , []) , a) 
  --        (mapTree corollaSetup sh)

  --   join : {n : ℕ} → {A : Set} → Tree n (Tree n A) → Maybe (Tree n A)
  --   join {zero} (pt (pt a)) = just (pt a)
  --   join {suc n} leaf = just leaf
  --   join {suc n} (node tr tsh) = traverseTree maybeA join tsh >>= graft tr

  --   shellExtents : {n : ℕ} → {A : Set} → Tree n (Tree (suc n) A) → Maybe (Tree n (Address (suc n)))
  --   shellExtents {n} {A} sh = shellExtents₀ [] sh

  --     where shellExtents₀ : Address (suc n) → Tree n (Tree (suc n) A) → Maybe (Tree n (Address (suc n)))
  --           shellExtents₀ ds sh₀ = traverseTree maybeA 
  --                                    (λ { (d , ∂ , leaf) → just (∂ ← (d ∷ ds)) ; 
  --                                         (d , _ , node _ s) → shellExtents₀ (d ∷ ds) s }) 
  --                                    (extentsData sh) >>= join

  --   flattenWithPrefix : {n : ℕ} → {A : Set} → Address (suc n) → Maybe (Derivative n (Address (suc n))) → Tree (suc n) A → Maybe (Tree n (Address (suc n)))
  --   flattenWithPrefix {n} ds nothing leaf = just (globDeriv n (Address (suc n)) ← ds) 
  --   flattenWithPrefix {n} ds (just ∂) leaf = just (∂ ← ds)
  --   flattenWithPrefix {n} ds _ (node a sh) = 
  --     traverseTree maybeA (λ { (d , ∂ , v) → flattenWithPrefix (d ∷ ds) (just ∂) v }) (localData {B = Address (suc n)} sh) 
  --     >>= join

  --   flatten : {n : ℕ} → {A : Set} → Tree (suc n) A → Maybe (Tree n (Address (suc n)))
  --   flatten tr = flattenWithPrefix [] nothing tr

  -- dejoin : {n : ℕ} → {A : Set} → Tree n A → (A → Bool) → Maybe (Tree n (A ⊎ Tree n A))
  -- dejoin {zero} (pt a) p = just (if p a then pt (inj₂ (pt a)) else pt (inj₁ a))
  -- dejoin {suc n} leaf p = just leaf
  -- dejoin {suc n} {A} (node a tr) p = 
  --   traverseTree maybeA (λ t → dejoin t p) tr 
  --   >>= (λ sh → 
  --     if p a then 
  --       (let innerShell : Tree n (Tree (suc n) A)
  --            innerShell = mapTree (λ { (_ , leaf) → leaf 
  --                                    ; (addr , node (inj₁ a₀) t) → leaf 
  --                                    ; (addr , node (inj₂ tr₀) t) → tr₀ }) (zipWithAddress sh) 
  --            outerShell : Tree n (Tree n (Tree (suc n) (A ⊎ Tree (suc n) A)))
  --            outerShell = mapTree (λ { (∂ , leaf) → ∂ ← leaf 
  --                                    ; (∂ , node (inj₁ a₀) t) → ∂ ← node (inj₁ a₀) t 
  --                                    ; (∂ , node (inj₂ tr) t) → t }) (zipWithDeriv sh)
  --          in join outerShell >>= (λ r → just (node (inj₂ (node a innerShell)) r)))
  --     else 
  --       just (node (inj₁ a) sh))

  -- -- Okay, the idea is that we can store the cardinal address of the root of the selection.  We then would like a routine
  -- -- which traces the selection tree, generating the mask.  The result should be the tree consisting of all places where
  -- -- the selection function returns true, and as soon as it returns false, we should cut.  Let's try this:
  
  -- takeWhile : {n : ℕ} → {A : Set} → Tree (suc n) A → (A → Bool) → Tree (suc n) A
  -- takeWhile leaf p = leaf
  -- takeWhile (node a sh) p = if p a then node a (mapTree (λ t → takeWhile t p) sh) else leaf

  -- -- Great.  That was easy.  Now, can we apply this to a cardinal?

  -- exciseDeriv : {n : ℕ} → {A B : Set} → Derivative n (Tree (suc n) A) → Tree (suc n) A → Tree (suc n) B → Maybe (Tree (suc n) A × Tree n (Tree (suc n) A))
  -- exciseDeriv ∂ tr leaf = just (leaf , ∂ ← tr)
  -- exciseDeriv ∂ leaf (node b msk) = nothing 
  -- exciseDeriv {n} {A} ∂ (node a trSh) (node _ mskSh) = 
  --   traverseTree maybeA (uncurry (uncurry exciseDeriv))
  --     (zip (zipWithDeriv {B = Tree (suc n) A} trSh) mskSh)
  --   >>= (λ zsh → let (nsh , crpJn) = unzip zsh in join crpJn >>= (λ crp → just (node a nsh , crp)))

  -- excise : {n : ℕ} → {A B : Set} → Tree (suc n) A → Tree (suc n) B → Maybe (Tree (suc n) A × Tree n (Tree (suc n) A))
  -- excise tr msk = exciseDeriv (globDeriv _ _) tr msk

  -- replace : {n : ℕ} → {A B : Set} → Tree (suc n) A → Tree (suc n) B → (Tree (suc n) A → A) → Maybe (Tree (suc n) A)
  -- replace tr msk rplfn = excise tr msk >>= (λ { (exTr , sh) → just (node (rplfn exTr) sh) })
  
  -- -- replaceAt : {n : ℕ} → {A B : Set} → Address (suc n) → Tree (suc n) A → Tree (suc n) B → (Tree (suc n) A → A) → Maybe (Tree (suc n) A)
  -- -- replaceAt addr tr msk rplfn = seekTo addr tr >>= (λ { (fcs , cntxt)  → replace fcs msk rplfn >>= (λ res → just (cntxt ↓ res)) })

