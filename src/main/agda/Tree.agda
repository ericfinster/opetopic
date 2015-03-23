--
--  Tree.agda - Higher Dimensional Trees
--
--  Eric Finster
--

{-# OPTIONS --no-termination-check #-}

open import Prelude
open import Mtl

module Tree where

  mutual

    Direction : (n : ℕ) → Set
    Direction zero = ⊥
    Direction (suc n) = Address n

    Address : (n : ℕ) → Set
    Address n = List (Direction n)

  data Tree₀ (A : Set) : ℕ → Set where
    pt : (a : A) → Tree₀ A 0
    leaf : {n : ℕ} → Tree₀ A (suc n)
    node : {n : ℕ} → (a : A) → (sh : Tree₀ (Tree₀ A (suc n)) n) → Tree₀ A (suc n)

  Tree : ℕ → Set → Set
  Tree n A = Tree₀ A n

  mapWithPrefix : {n : ℕ} → {A B : Set} → (Address n → A → B) → Address n → Tree n A → Tree n B
  mapWithPrefix {zero} f p (pt a) = pt (f p a)
  mapWithPrefix {suc n} f p leaf = leaf
  mapWithPrefix {suc n} f p (node a sh) = node (f p a) (mapWithPrefix (λ d b → mapWithPrefix f (d ∷ p) b) [] sh)

  mapWithAddress : {n : ℕ} → {A B : Set} → (Address n → A → B) → Tree n A → Tree n B
  mapWithAddress f t = mapWithPrefix f [] t

  mapTree : {n : ℕ} → {A B : Set} → (A → B) → Tree n A → Tree n B
  mapTree {zero} f (pt a) = pt (f a)
  mapTree {suc n} f leaf = leaf
  mapTree {suc n} f (node a sh) = node (f a) (mapTree (mapTree f) sh)

  traverseWithPrefix : {n : ℕ} → {A B : Set} → {G : Set → Set} → Applicative G → (Address n → A → G B) → Address n → Tree n A → G (Tree n B)
  traverseWithPrefix {zero} apG f p (pt a) = let open Applicative apG in pure pt ⊛ f p a
  traverseWithPrefix {suc n} apG f p leaf = let open Applicative apG in pure leaf
  traverseWithPrefix {suc n} apG f p (node a sh) = let open Applicative apG in 
    pure node ⊛ f p a ⊛ traverseWithPrefix apG (λ d b → traverseWithPrefix apG f (d ∷ p) b) [] sh

  traverseWithAddress : {n : ℕ} → {A B : Set} → {G : Set → Set} → Applicative G → (Address n → A → G B) → Tree n A → G (Tree n B)
  traverseWithAddress apG f t = traverseWithPrefix apG f [] t

  traverseTree : {n : ℕ} → {A B : Set} → {G : Set → Set} → Applicative G → (A → G B) → Tree n A → G (Tree n B)
  traverseTree {zero} apG f (pt a) = let open Applicative apG in pure pt ⊛ f a
  traverseTree {suc n} apG f leaf = let open Applicative apG in pure leaf
  traverseTree {suc n} apG f (node a sh) = let open Applicative apG in 
    pure node ⊛ f a ⊛ traverseTree apG (traverseTree apG f) sh

  TreeF : (n : ℕ) → Functor (Tree n)
  TreeF n = record { fmap = mapTree {n} }

  TreeT : (n : ℕ) → Traverse (Tree n)
  TreeT n = record { isFunctor = TreeF n ; traverse = traverseTree {n} }

  open Monad maybeM hiding (fmap)

  zipWithAddress : {n : ℕ} → {A : Set} → Tree n A → Tree n (Address n × A)
  zipWithAddress = mapWithAddress (λ addr a → (addr , a))

  addrTree : {n : ℕ} → {A : Set} → Tree n A → Tree n (Address n)
  addrTree = mapWithAddress (λ addr _ → addr)

  const : {n : ℕ} → {A B : Set} → A → Tree n B → Tree n A
  const a t = mapTree (λ _ → a) t

  zip : {n : ℕ} → {A B : Set} → Tree n A → Tree n B → Tree n (A × B)
  zip {zero} (pt a) (pt b) = pt (a , b)
  zip {suc n} leaf leaf = leaf
  zip {suc n} leaf (node a shB) = leaf
  zip {suc n} (node a shA) leaf = leaf
  zip {suc n} (node a shA) (node b shB) = 
    node (a , b) (mapTree (uncurry zip) (zip shA shB))

  unzip : {n : ℕ} → {A B : Set} → Tree n (A × B) → (Tree n A × Tree n B)
  unzip {zero} (pt (a , b)) = (pt a , pt b)
  unzip {suc n} leaf = leaf , leaf
  unzip {suc n} (node (a , b) shAB) with unzip (mapTree unzip shAB)
  unzip {suc n} (node (a , b) shAB) | (shA , shB) = (node a shA , node b shB)

  zipComplete : {n : ℕ} → {A B : Set} → Tree n A → Tree n B → Maybe (Tree n (A × B))
  zipComplete {zero} (pt a) (pt b) = just (pt (a , b))
  zipComplete {suc n} leaf (node b bsh) = nothing
  zipComplete {suc n} (node a ash) leaf = nothing
  zipComplete {suc n} leaf leaf = just leaf
  zipComplete {suc n} (node a ash) (node b bsh) = 
    zipComplete ash bsh 
    >>= (λ zsh → traverseTree maybeA (uncurry zipComplete) zsh 
    >>= (λ sh → just (node (a , b) sh)))

  treeRecWithPrefix : {n : ℕ} → {A B : Set} → 
                      (Address (suc n) → A → Tree n B → B) → 
                      (Address (suc n) → B) → Address (suc n) → Tree (suc n) A → B

  treeRecWithPrefix ndRec lfRec ds leaf = lfRec ds
  treeRecWithPrefix ndRec lfRec ds (node a sh) = 
    ndRec ds a (mapWithAddress (λ d b → treeRecWithPrefix ndRec lfRec (d ∷ ds) b) sh)

  treeRecWithAddr : {n : ℕ} → {A B : Set} → 
                    (Address (suc n) → A → Tree n B → B) → 
                    (Address (suc n) → B) → Tree (suc n) A → B
  treeRecWithAddr ndRec lfRec tr = treeRecWithPrefix ndRec lfRec [] tr

  mutual

    Derivative : (n : ℕ) → Set → Set
    Derivative zero A = ⊤
    Derivative (suc n) A = Tree n (Tree (suc n) A) × Context (suc n) A

    Context : (n : ℕ) → Set → Set
    Context zero A = ⊤
    Context (suc n) A = List (A × Derivative n (Tree (suc n) A))

    Zipper : (n : ℕ) → Set → Set
    Zipper n A = Tree n A × Context n A
  
    _←_ : {n : ℕ} → {A : Set} → Derivative n A → A → Tree n A
    _←_ {zero} tt a = pt a
    _←_ {suc n} (sh , context) a = context ↓ node a sh

    _↓_ : {n : ℕ} → {A : Set} → Context n A → Tree n A → Tree n A
    _↓_ {zero} tt t = t
    _↓_ {suc n} [] t = t
    _↓_ {suc n} ((a , d) ∷ c) t = c ↓ node a (d ← t)

    visit : {n : ℕ} → {A : Set} → Direction n → Zipper n A → Maybe (Zipper n A)
    visit {zero} () z
    visit {suc zero} [] (leaf , c) = nothing
    visit {suc zero} [] (node head (pt tail) , c) = just (tail , (head , tt) ∷ c)
    visit {suc zero} (() ∷ d) z
    visit {suc (suc n)} d (leaf , c) = nothing
    visit {suc (suc n)} d (node a sh , c) = 
      seek d (sh , []) 
      >>= (λ { (leaf , c₀) → nothing ; 
               (node tr hsh , c₀) → just (tr , (a , hsh , c₀) ∷ c) })

    seek : {n : ℕ} → {A : Set} → Address n → Zipper n A → Maybe (Zipper n A)
    seek [] z = just z
    seek (d ∷ ds) z = seek ds z >>= visit d 

  seekTo : {n : ℕ} → {A : Set} → Address n → Tree n A → Maybe (Zipper n A)
  seekTo {zero} addr tr = seek addr (tr , tt)
  seekTo {suc n} addr tr = seek addr (tr , [])

  globDeriv : (n : ℕ) → (B : Set) → Derivative n B
  globDeriv zero B = tt
  globDeriv (suc n) B = (globDeriv n (Tree (suc n) B) ← leaf) , []

  rootValue : {n : ℕ} → {A : Set} → Tree n A → Maybe A
  rootValue {zero} (pt a) = just a
  rootValue {suc n} leaf = nothing
  rootValue {suc n} (node a sh) = just a

  _valueAt_ : {n : ℕ} → {A : Set} → Tree n A → Address n → Maybe A
  tr valueAt addr = seekTo addr tr >>= (λ z → rootValue (proj₁ z))

  zipWithDeriv : {n : ℕ} → {A B : Set} → Tree n A → Tree n (Derivative n B × A)
  zipWithDeriv (pt a) = pt (tt , a)
  zipWithDeriv leaf = leaf
  zipWithDeriv (node a sh) = node ((const leaf sh , []) , a) (mapTree zipWithDeriv sh)

  extentsData : {n : ℕ} → {A : Set} → Tree n A → Tree n (Address n × Derivative n (Address (suc n)) × A)
  extentsData {zero} (pt a) = pt ([] , tt , a)
  extentsData {suc n} t = 
    treeRecWithAddr 
      (λ addr a sh → node (addr , (const leaf sh , []) , a) sh)
      (λ addr → leaf) t

  localData : {n : ℕ} → {A B : Set} → Tree n A → Tree n (Address n × Derivative n B × A)
  localData {zero} (pt a) = pt ([] , tt , a)
  localData {suc n} tr = treeRecWithAddr (λ addr a sh → node (addr , (const leaf sh , []) , a) sh) (λ addr → leaf) tr

  ∇ : {n : ℕ} → {A : Set} → Tree n A → Tree n (Tree n A)
  ∇ {n} {A} tr = mapTree (λ { (_ , ∂ , a) → ∂ ← a }) (localData {B = A} tr)

  corollaSetup : {n : ℕ} → {A : Set} → Tree n A → Tree n (Derivative n (Address n) × A)
  corollaSetup {zero} (pt a) = pt (tt , a)
  corollaSetup {suc n} leaf  = leaf 
  corollaSetup {suc n} (node a sh) = 
    node ((mapTree (λ _ → leaf ) sh , []) , a) 
         (mapTree corollaSetup sh)

  mutual

    graftRecHoriz : {n : ℕ} → {A B : Set} → (A → Tree (suc n) B → Maybe B) → (Address (suc n) → Maybe B) →
                    Address (suc n) × Derivative n (Address (suc n)) × Tree (suc n) (Tree (suc (suc n)) A) → Maybe (Tree (suc n) B × Tree n (Address (suc n)))
    graftRecHoriz {n} ndRec lfRec (hAddr , ∂ , leaf) = just (leaf , (∂ ← hAddr))
    graftRecHoriz {n} ndRec lfRec (hAddr , ∂ , (node leaf hsh)) = 
      graftRecCont ndRec lfRec (mapTree (λ { (dir , deriv , vsh) → (dir ∷ hAddr , deriv , vsh) }) (localData {B = Address (suc n)} hsh)) 
      >>= (λ { (bSh , adTr) → lfRec hAddr >>= (λ b → just (node b bSh , adTr)) })
    graftRecHoriz {n} ndRec lfRec (hAddr , ∂ , (node (node a vsh) hsh)) = 
      graftRecHoriz ndRec lfRec (hAddr , ∂ , vsh)
      >>= (λ { (bTr , adTr₀) → zipComplete adTr₀ (zipWithDeriv {B = Address (suc n)} hsh) 
                               >>= (λ zt → graftRecCont ndRec lfRec zt
                               >>= (λ { (bSh , adTr) → ndRec a bTr >>= (λ b → just (node b bSh , adTr)) }) )
             })


    graftRecCont : {n : ℕ} → {A B : Set} → (A → Tree (suc n) B → Maybe B) → (Address (suc n) → Maybe B) →
                   Tree n (Address (suc n) × Derivative n (Address (suc n)) × Tree (suc n) (Tree (suc (suc n)) A)) → Maybe (Tree n (Tree (suc n) B) × Tree n (Address (suc n)))
    graftRecCont {n} ndRec lfRec hsh = 
      traverseTree maybeA (graftRecHoriz ndRec lfRec) hsh 
      >>= (λ trRes → let (bSh , adrJnSh) = unzip trRes in join adrJnSh 
      >>= (λ adSh → just (bSh , adSh))) 

    graftRecChain : {n : ℕ} → {A B : Set} → (A → Tree (suc n) B → Maybe B) → (Address (suc n) → Maybe B) →
                    A → Tree n (Tree (suc n) (Tree (suc (suc n)) A)) → Maybe (B × Tree n (Address (suc n))) → Maybe (B × Tree n (Address (suc n)))
    graftRecChain {n} ndRec lfRec a hsh m = 
      m >>= (λ { (b₀ , adTr₀) → zipComplete adTr₀ (zipWithDeriv {B = Address (suc n)} hsh) 
        >>= (λ zt → graftRecCont ndRec lfRec zt
        >>= (λ { (bSh , adTr) → ndRec a (node b₀ bSh) 
        >>= (λ b → just (b , adTr)) }) ) })

    graftRecStart : {n : ℕ} → {A B : Set} → (A → Tree (suc n) B → Maybe B) → (Address (suc n) → Maybe B) →
                   Tree (suc (suc n)) A → A → Tree n (Tree (suc n) (Tree (suc (suc n)) A)) → Maybe (B × Tree n (Address (suc n)))
    graftRecStart ndRec lfRec leaf a hsh = 
      graftRecChain ndRec lfRec a hsh (lfRec [] >>= (λ b → just (b , mapWithAddress (λ dir _ → dir ∷ []) hsh)))
    graftRecStart ndRec lfRec (node a₀ leaf) a hsh = 
      graftRecChain ndRec lfRec a hsh (ndRec a leaf >>= (λ b → just (b , mapWithAddress (λ dir _ → dir ∷ []) hsh)))
    graftRecStart ndRec lfRec (node a₀ (node v hsh₀)) a hsh = 
      graftRecChain ndRec lfRec a hsh (graftRecStart ndRec lfRec v a₀ hsh₀)

    graftRec : {n : ℕ} → {A B : Set} → (A → Tree n B → Maybe B) → (Address n → Maybe B) →
               Tree (suc n) A → Maybe B
    graftRec {zero} ndRec lfRec leaf = lfRec []
    graftRec {zero} ndRec lfRec (node hd (pt tl)) = 
      graftRec ndRec lfRec tl >>= (λ b → ndRec hd (pt b))
    graftRec {suc n} ndRec lfRec leaf = lfRec []
    graftRec {suc n} ndRec lfRec (node a leaf) = ndRec a leaf
    graftRec {suc n} ndRec lfRec (node a (node v hsh)) = 
      graftRecStart ndRec lfRec v a hsh >>= (λ { (b , _) → just b })

    graft : {n : ℕ} → {A : Set} → Tree (suc n) A → Tree n (Tree (suc n) A) → Maybe (Tree (suc n) A)
    graft {n} tr brs = graftRec (λ a sh → just (node a sh)) (λ addr → brs valueAt addr) tr

    join : {n : ℕ} → {A : Set} → Tree n (Tree n A) → Maybe (Tree n A)
    join {zero} (pt (pt a)) = just (pt a)
    join {suc n} leaf = just leaf
    join {suc n} (node tr tsh) = traverseTree maybeA join tsh >>= graft tr

    shellExtents : {n : ℕ} → {A : Set} → Tree n (Tree (suc n) A) → Maybe (Tree n (Address (suc n)))
    shellExtents {n} {A} sh = shellExtents₀ [] sh

      where shellExtents₀ : Address (suc n) → Tree n (Tree (suc n) A) → Maybe (Tree n (Address (suc n)))
            shellExtents₀ ds sh₀ = traverseTree maybeA 
                                     (λ { (d , ∂ , leaf) → just (∂ ← (d ∷ ds)) ; 
                                          (d , _ , node _ s) → shellExtents₀ (d ∷ ds) s }) 
                                     (extentsData sh) >>= join

    flattenWithPrefix : {n : ℕ} → {A : Set} → Address (suc n) → Maybe (Derivative n (Address (suc n))) → Tree (suc n) A → Maybe (Tree n (Address (suc n)))
    flattenWithPrefix {n} ds nothing leaf = just (globDeriv n (Address (suc n)) ← ds) 
    flattenWithPrefix {n} ds (just ∂) leaf = just (∂ ← ds)
    flattenWithPrefix {n} ds _ (node a sh) = 
      traverseTree maybeA (λ { (d , ∂ , v) → flattenWithPrefix (d ∷ ds) (just ∂) v }) (localData {B = Address (suc n)} sh) 
      >>= join

    flatten : {n : ℕ} → {A : Set} → Tree (suc n) A → Maybe (Tree n (Address (suc n)))
    flatten tr = flattenWithPrefix [] nothing tr

  dejoin : {n : ℕ} → {A : Set} → Tree n A → (A → Bool) → Maybe (Tree n (A ⊎ Tree n A))
  dejoin {zero} (pt a) p = just (if p a then pt (inj₂ (pt a)) else pt (inj₁ a))
  dejoin {suc n} leaf p = just leaf
  dejoin {suc n} {A} (node a tr) p = 
    traverseTree maybeA (λ t → dejoin t p) tr 
    >>= (λ sh → 
      if p a then 
        (let innerShell : Tree n (Tree (suc n) A)
             innerShell = mapTree (λ { (_ , leaf) → leaf 
                                     ; (addr , node (inj₁ a₀) t) → leaf 
                                     ; (addr , node (inj₂ tr₀) t) → tr₀ }) (zipWithAddress sh) 
             outerShell : Tree n (Tree n (Tree (suc n) (A ⊎ Tree (suc n) A)))
             outerShell = mapTree (λ { (∂ , leaf) → ∂ ← leaf 
                                     ; (∂ , node (inj₁ a₀) t) → ∂ ← node (inj₁ a₀) t 
                                     ; (∂ , node (inj₂ tr) t) → t }) (zipWithDeriv sh)
           in join outerShell >>= (λ r → just (node (inj₂ (node a innerShell)) r)))
      else 
        just (node (inj₁ a) sh))

  -- Okay, the idea is that we can store the cardinal address of the root of the selection.  We then would like a routine
  -- which traces the selection tree, generating the mask.  The result should be the tree consisting of all places where
  -- the selection function returns true, and as soon as it returns false, we should cut.  Let's try this:
  
  takeWhile : {n : ℕ} → {A : Set} → Tree (suc n) A → (A → Bool) → Tree (suc n) A
  takeWhile leaf p = leaf
  takeWhile (node a sh) p = if p a then node a (mapTree (λ t → takeWhile t p) sh) else leaf

  -- Great.  That was easy.  Now, can we apply this to a cardinal?

  exciseDeriv : {n : ℕ} → {A B : Set} → Derivative n (Tree (suc n) A) → Tree (suc n) A → Tree (suc n) B → Maybe (Tree (suc n) A × Tree n (Tree (suc n) A))
  exciseDeriv ∂ leaf leaf = just (leaf , ∂ ← leaf)
  exciseDeriv ∂ leaf (node a sil) = nothing 
  exciseDeriv ∂ (node a tr) leaf = just (leaf , ∂ ← leaf)
  exciseDeriv {n} {A} ∂ (node a trSh) (node _ silSh) = 
    traverseTree maybeA (uncurry (uncurry exciseDeriv))
      (zip (zipWithDeriv {B = Tree (suc n) A} trSh) silSh)
    >>= (λ zsh → let (nsh , crpJn) = unzip zsh in join crpJn >>= (λ crp → just (node a nsh , crp)))

  excise : {n : ℕ} → {A B : Set} → Tree (suc n) A → Tree (suc n) B → Maybe (Tree (suc n) A × Tree n (Tree (suc n) A))
  excise tr sil = exciseDeriv (globDeriv _ _) tr sil

  replace : {n : ℕ} → {A B : Set} → Tree (suc n) A → Tree (suc n) B → (Tree (suc n) A → A) → Maybe (Tree (suc n) A)
  replace tr sil rplfn = excise tr sil >>= (λ { (exTr , sh) → just (node (rplfn exTr) sh) })
  
  -- replaceAt : {n : ℕ} → {A B : Set} → Address (suc n) → Tree (suc n) A → Tree (suc n) B → (Tree (suc n) A → A) → Maybe (Tree (suc n) A)
  -- replaceAt addr tr sil rplfn = seekTo addr tr >>= (λ { (fcs , cntxt)  → replace fcs sil rplfn >>= (λ res → just (cntxt ↓ res)) })

