--
--  Mtl.agda - Monad Transformers
--
--  Eric Finster
--

open import Prelude

module Mtl where

  record Functor (F : Set → Set) : Set₁ where
    field
      fmap : {X Y : Set} → (X → Y) → (F X → F Y)

  record Applicative (F : Set → Set) : Set₁ where

    infixl 4 _⊛_

    field
      isFunctor : Functor F

      pure : ∀ {A} → A → F A
      _⊛_  : ∀ {A B} → F (A → B) → F A → F B

    open Functor isFunctor public

  record Traverse (F : Set → Set) : Set₁ where

    field
      isFunctor : Functor F
  
      traverse : ∀ {A B} → {G : Set → Set} → Applicative G → (f : A → G B) → F A → G (F B)

    open Functor isFunctor public

    sequence : ∀ {A} → {G : Set → Set} → Applicative G → F (G A) → G (F A)
    sequence isA fga = traverse isA (λ x → x) fga
   
  record Comonad (F : Set → Set) : Set₁ where

    field
  
      isFunctor : Functor F

      ε : {X : Set} → F X → X
      δ : {X : Set} → F X → F (F X)

    open Functor isFunctor public

  record Monad (F : Set → Set) : Set₁ where

    field 

      isFunctor : Functor F

      η : {X : Set} → X → F X
      μ : {X : Set} → F (F X) → (F X)

    open Functor isFunctor public

    infixl 4 _>>=_

    _>>=_ : {A B : Set} → F A → (A → F B) → F B
    m >>= f = μ (fmap f m)
  
  monad-pure : {F : Set → Set} → Monad F → {A : Set} → A → F A
  monad-pure isM a = η a
    where open Monad isM

  monad-ap : {F : Set → Set} → Monad F → {A B : Set} → F (A → B) → F A → F B
  monad-ap isM ff fa = fa >>= (λ a → ff >>= (λ f → η (f a)))
    where open Monad isM

  monadIsApp : {F : Set → Set} → Monad F → Applicative F
  monadIsApp {F} isM = record { isFunctor = isFunctor ; pure = monad-pure isM ; _⊛_ = monad-ap isM }
    where open Monad isM 

  record MonadReader (F : Set → Set) (E : Set) : Set₁ where
    field
  
      isMonad : Monad F

      ask : F E
      local : {A : Set} → (E → E) → F A → F A

    open Monad isMonad public

    scope : {A : Set} → E → F A → F A
    scope e = local (λ e → e)

  record MonadState (F : Set → Set) (S : Set) : Set₁ where
    field

      isMonad : Monad F
  
      get : F S
      put : S → F ⊤

    open Monad isMonad public

  record MonadError (F : Set → Set) : Set₁ where
    field

      isMonad : Monad F

      failWith : {A : Set} → String → F A
    
    open Monad isMonad public

  --
  -- Identity
  --

  Id : Set → Set
  Id A = A

  map-id : {A B : Set} → (f : A → B) → Id A → Id B
  map-id f a = f a

  η-id : {A : Set} → A → Id A
  η-id a = a

  μ-id : {A : Set} → Id (Id A) → Id A
  μ-id a = a

  traverse-id : {A B : Set} → {G : Set → Set} → Applicative G → (f : A → G B) → Id A → G (Id B)
  traverse-id apG f a = f a
    where open Applicative apG

  idF : Functor Id
  idF = record { fmap = map-id }

  idM : Monad Id
  idM = record { isFunctor = idF ; η = η-id ; μ = μ-id }

  idT : Traverse Id
  idT = record { isFunctor = idF ; traverse = traverse-id }

  idA : Applicative Id
  idA = monadIsApp idM

  --
  -- Boolean Monoid for detecting conditions in a traverse
  -- 

  ConstBool : Set → Set
  ConstBool A = Bool

  map-bool : {A B : Set} → (f : A → B) → ConstBool A → ConstBool B
  map-bool f b = b

  boolF : Functor ConstBool
  boolF = record { fmap = map-bool }

  pure-bool : {A : Set} → A → ConstBool A
  pure-bool a = false

  ap-bool : {A B : Set} → ConstBool (A → B) → ConstBool A → ConstBool B
  ap-bool bf ba = bf ∨ ba

  boolApp : Applicative ConstBool
  boolApp = record { isFunctor = boolF ; pure = pure-bool ; _⊛_ = λ {A} {B} → ap-bool {A} {B} }

  some : {F : Set → Set} → {A : Set} → Traverse F → F A → (A → Bool) → Bool
  some {F} {A} isT fa p = traverse {A} {A} {ConstBool} boolApp (λ a → p a) fa
    where open Traverse isT

  --
  -- Maybe
  -- 

  map-maybe : {A B : Set} → (A → B) → (Maybe A → Maybe B)
  map-maybe f (just x) = just (f x)
  map-maybe f nothing = nothing

  η-maybe : {A : Set} → A → Maybe A
  η-maybe a = just a

  μ-maybe : {A : Set} → Maybe (Maybe A) → Maybe A
  μ-maybe (just (just x)) = just x
  μ-maybe (just nothing) = nothing
  μ-maybe nothing = nothing

  maybeF : Functor Maybe
  maybeF = record { fmap = map-maybe }
  
  maybeM : Monad Maybe
  maybeM = record { isFunctor = maybeF ; η = η-maybe ; μ = μ-maybe }

  maybeA : Applicative Maybe
  maybeA = monadIsApp maybeM

  --
  --  Maybe Transformer
  --

  data MaybeT (F : Set → Set) (A : Set) : Set where
    toM : F (Maybe A) → MaybeT F A

  map-maybeT : {F : Set → Set} → (isF : Functor F) → {A B : Set} → (A → B) → (MaybeT F A → MaybeT F B)
  map-maybeT isF f (toM fma) = toM (fmap (map-maybe f) fma)
    where open Functor isF

  η-maybeT : {F : Set → Set} → (isM : Monad F) → {A : Set} → A → MaybeT F A
  η-maybeT isM a = toM (fmap η-maybe (η a))
    where open Monad isM

  μ-maybeT : {F : Set → Set} → (isM : Monad F) → {A : Set} → MaybeT F (MaybeT F A) → MaybeT F A
  μ-maybeT {F} isM (toM fma) = toM (μ (fmap unwrap fma))
    where open Monad isM
          unwrap : {A : Set} → Maybe (MaybeT F A) → F (Maybe A)
          unwrap (just (toM fma₀)) = fma₀
          unwrap nothing = η nothing

  maybeT-F : {F : Set → Set} → (isF : Functor F) → Functor (MaybeT F)
  maybeT-F isF = record { fmap = map-maybeT isF }

  maybeT-M : {F : Set → Set} → (isM : Monad F) → Monad (MaybeT F)
  maybeT-M isM = record { isFunctor = maybeT-F isFunctor ; η = η-maybeT isM ; μ = μ-maybeT isM }
    where open Monad isM

  --
  --  Either
  --

  map-either : {E A B : Set} → (f : A → B) → E ⊎ A → E ⊎ B
  map-either f (inj₁ e) = inj₁ e
  map-either f (inj₂ a) = inj₂ (f a)

  η-either : {E A : Set} → A → E ⊎ A
  η-either a = inj₂ a

  μ-either : {E A : Set} → E ⊎ (E ⊎ A) → E ⊎ A
  μ-either (inj₁ e) = inj₁ e
  μ-either (inj₂ (inj₁ e)) = inj₁ e
  μ-either (inj₂ (inj₂ a)) = inj₂ a

  eitherF : {E : Set} → Functor (λ A → E ⊎ A)
  eitherF = record { fmap = map-either }

  eitherM : {E : Set} → Monad (λ A → E ⊎ A)
  eitherM = record { isFunctor = eitherF ; η = η-either ; μ = μ-either }

  Error : Set → Set
  Error A = String ⊎ A

  errorF : Functor Error
  errorF = eitherF

  errorM : Monad Error
  errorM = eitherM

  succeed : {A : Set} → A → Error A
  succeed a = inj₂ a

  fail : {A : Set} → String → Error A
  fail s = inj₁ s

  --
  --  State Transformer
  --

  data StateT (F : Set → Set) (S : Set) (A : Set) : Set where
    toS : (S → F (S × A)) → StateT F S A
  
  run-stateT : {F : Set → Set} → {S A : Set} → (isF : Functor F) → (s : S) → (m : StateT F S A) → F (S × A)
  run-stateT isF s (toS m) = m s

  eval-stateT : {F : Set → Set} → {S A : Set} → (isF : Functor F) → (s : S) → (m : StateT F S A) → F S
  eval-stateT isF s sm = fmap proj₁ (run-stateT isF s sm)
    where open Functor isF

  exec-stateT : {F : Set → Set} → {S A : Set} → (isF : Functor F) → (s : S) → (m : StateT F S A) → F A
  exec-stateT isF s sm = fmap proj₂ (run-stateT isF s sm)
    where open Functor isF

  get-stateT : {F : Set → Set} → {S : Set} → (isM : Monad F) → StateT F S S
  get-stateT isM = toS (λ s → η (s , s))
    where open Monad isM

  put-stateT : {F : Set → Set} → {S : Set} → (isM : Monad F) → S → StateT F S ⊤
  put-stateT isM s = toS (λ s₀ → η (s , tt))
    where open Monad isM

  map-stateT : {F : Set → Set} → {S : Set} → (isF : Functor F) → {A B : Set} → (f : A → B) → StateT F S A → StateT F S B
  map-stateT isF f (toS m) = toS (λ s → fmap (λ { (s₀ , a) → (s₀ , f a) }) (m s))
    where open Functor isF

  η-stateT : {F : Set → Set} → {S : Set} → (isM : Monad F) → {A : Set} → A → StateT F S A
  η-stateT isM a = toS (λ s → η (s , a))
    where open Monad isM

  μ-stateT : {F : Set → Set} → {S : Set} → (isM : Monad F) → {A : Set} → StateT F S (StateT F S A) → StateT F S A
  μ-stateT isM {A} (toS m) = toS (λ s → μ (fmap (λ { (s₀ , sm) → run-stateT isFunctor s₀ sm }) (m s)))
    where open Monad isM

  stateTF : {F : Set → Set} {S : Set} → (isF : Functor F) → Functor (StateT F S)
  stateTF isF = record { fmap = map-stateT isF }
  
  stateTM : {F : Set → Set} {S : Set} → (isM : Monad F) → Monad (StateT F S)
  stateTM isM = record { isFunctor = stateTF isFunctor ; η = η-stateT isM ; μ = μ-stateT isM }
    where open Monad isM

  stateTS : {F : Set → Set} {S : Set} → (isM : Monad F) → MonadState (StateT F S) S
  stateTS isM = record { isMonad = stateTM isM ; get = get-stateT isM ; put = put-stateT isM }

  --
  --  Reader Transformer
  --

  data ReaderT (F : Set → Set) (E : Set) (A : Set) : Set where
    toR : (E → F A) → ReaderT F E A

  run-readerT : {F : Set → Set} → {E A : Set} → (isF : Functor F) → (e : E) → (m : ReaderT F E A) → F A
  run-readerT isF s (toR m) = m s

  ask-readerT : {F : Set → Set} → {E : Set} → (isM : Monad F) → ReaderT F E E
  ask-readerT isM = toR (λ e → η e)
    where open Monad isM

  local-readerT : {F : Set → Set} → {E : Set} → (isM : Monad F) → {A : Set} → (E → E) → ReaderT F E A → ReaderT F E A
  local-readerT isM f (toR m) = toR (λ e → m (f e))

  map-readerT : {F : Set → Set} → {E : Set} → (isF : Functor F) → {A B : Set} → (f : A → B) → ReaderT F E A → ReaderT F E B
  map-readerT isF f (toR m) = toR (λ e → fmap f (m e))
    where open Functor isF

  η-readerT : {F : Set → Set} → {E : Set} → (isM : Monad F) → {A : Set} → A → ReaderT F E A
  η-readerT isM a = toR (λ e → η a)
    where open Monad isM

  μ-readerT : {F : Set → Set} → {E : Set} → (isM : Monad F) → {A : Set} → ReaderT F E (ReaderT F E A) → ReaderT F E A
  μ-readerT isM (toR m) = toR (λ e → μ (fmap (run-readerT isFunctor e) (m e)))
    where open Monad isM

  readerTF : {F : Set → Set} → {E : Set} → (isF : Functor F) → Functor (ReaderT F E)
  readerTF isF = record { fmap = map-readerT isF }

  readerTM : {F : Set → Set} → {E : Set} → (isM : Monad F) → Monad (ReaderT F E)
  readerTM isM = record { isFunctor = readerTF isFunctor ; η = η-readerT isM ; μ = μ-readerT isM }
    where open Monad isM

  readerTR : {F : Set → Set} → {E : Set} → (isM : Monad F) → MonadReader (ReaderT F E) E
  readerTR isM = record { isMonad = readerTM isM ; ask = ask-readerT isM ; local = local-readerT isM }

  --
  -- Continuations
  -- 

  Cont : Set → Set → Set
  Cont R A = (A → R) → R

  map-cont : {A B R : Set} → (f : A → B) → Cont R A → Cont R B
  map-cont f m = λ k → m (λ a → k (f a))

  η-cont : {A R : Set} → A → Cont R A
  η-cont a = λ k → k a

  μ-cont : {A R : Set} → Cont R (Cont R A) → Cont R A
  μ-cont m = λ k → m (λ m₀ → m₀ k)



  
