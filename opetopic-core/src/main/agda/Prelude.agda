--
--  Prelude.agda - Basic definitions
--
--  Eric Finster
--

module Prelude where

  infixr 5 _∨_ _∷_ _++_
  infixr 1 _⊎_
  infixr 4 _,_ _≤_
  infixr 2 _×_

  data ℕ : Set where
    zero : ℕ
    suc  : (n : ℕ) → ℕ

  {-# BUILTIN NATURAL ℕ #-}

  _+_ : ℕ → ℕ → ℕ
  zero  + n = n
  suc m + n = suc (m + n)

  {-# BUILTIN NATPLUS _+_ #-}

  _∸_ : ℕ → ℕ → ℕ
  m     ∸ zero  = m
  zero  ∸ suc n = zero
  suc m ∸ suc n = m ∸ n

  {-# BUILTIN NATMINUS _∸_ #-}

  data _≤_ : ℕ → ℕ → Set where
    z≤n : ∀ {n}                 → zero ≤ n
    s≤s : ∀ {m n} (m≤n : m ≤ n) → suc m ≤ suc n

  Δ : ∀ {m n} → m ≤ n → ℕ
  Δ {.zero} {n} z≤n = n
  Δ (s≤s m≤n) = Δ m≤n

  record ⊤ : Set where
    constructor tt

  data Bool : Set where
    true  : Bool
    false : Bool

  {-# BUILTIN BOOL  Bool  #-}
  {-# BUILTIN TRUE  true  #-}
  {-# BUILTIN FALSE false #-}

  {-# COMPILED_DATA Bool Bool True False #-}

  _∨_ : Bool → Bool → Bool
  true  ∨ b = true
  false ∨ b = b

  if_then_else_ : {A : Set} → Bool → A → A → A
  if true  then t else f = t
  if false then t else f = f

  data Maybe (A : Set) : Set where
    just    : (x : A) → Maybe A
    nothing : Maybe A

  isNothing : {A : Set} → Maybe A → Bool
  isNothing (just _) = false
  isNothing nothing = true

  maybeRec : {A B : Set} → Maybe A → B → (A → B) → B
  maybeRec nothing b f = b
  maybeRec (just a) b f = f a

  {-# IMPORT Data.FFI #-}
  {-# COMPILED_DATA Maybe Data.FFI.AgdaMaybe Just Nothing #-}

  data _⊎_ (A : Set) (B : Set) : Set where
    inj₁ : (x : A) → A ⊎ B
    inj₂ : (y : B) → A ⊎ B

  {-# IMPORT Data.FFI #-}
  {-# COMPILED_DATA _⊎_ Data.FFI.AgdaEither Left Right #-}

  postulate
    String : Set

  {-# BUILTIN STRING String #-}
  {-# COMPILED_TYPE String String #-}

  record Σ (A : Set) (B : A → Set) : Set where
    constructor _,_
    field
      proj₁ : A
      proj₂ : B proj₁

  open Σ public

  Σ-syntax : ∀ (A : Set) → (A → Set) → Set 
  Σ-syntax = Σ

  syntax Σ-syntax A (λ x → B) = Σ[ x ∈ A ] B

  _×_ : ∀ (A : Set) (B : Set) → Set
  A × B = Σ[ x ∈ A ] B

  uncurry : {A : Set} → {B : A → Set} → {C : Σ A B → Set} →
            ((x : A) → (y : B x) → C (x , y)) →
            ((p : Σ A B) → C p)
  uncurry f (x , y) = f x y

  data List (A : Set) : Set where
    []  : List A
    _∷_ : (x : A) (xs : List A) → List A

  {-# BUILTIN LIST List #-}
  {-# BUILTIN NIL  []   #-}
  {-# BUILTIN CONS _∷_  #-}

  {-# IMPORT Data.FFI #-}
  {-# COMPILED_DATA List Data.FFI.AgdaList [] (:) #-}

  headMaybe : {A : Set} → List A → Maybe A
  headMaybe [] = nothing
  headMaybe (a ∷ _) = just a

  [_] : ∀ {A : Set} → A → List A
  [ x ] = x ∷ []

  _++_ : ∀ {A : Set} → List A → List A → List A
  []       ++ ys = ys
  (x ∷ xs) ++ ys = x ∷ (xs ++ ys)

  mapList : {A B : Set} → (A → B) → List A → List B
  mapList f [] = []
  mapList f (a ∷ as) = f a ∷ mapList f as

  data ⊥ : Set where

  {-# IMPORT Data.FFI #-}
  {-# COMPILED_DATA ⊥ Data.FFI.AgdaEmpty #-}

  data _==_ {i} {A : Set i} (a : A) : A → Set i where
    idp : a == a

  {-# BUILTIN EQUALITY _==_ #-}
  {-# BUILTIN REFL  idp #-}

  infix  2 _∎
  infixr 2 _=⟨_⟩_

  _=⟨_⟩_ : ∀ {i} {A : Set i} (x : A) {y z : A} → x == y → y == z → x == z
  _ =⟨ idp ⟩ idp = idp

  _∎ : ∀ {i} {A : Set i} (x : A) → x == x
  _ ∎ = idp

  syntax ap f p = p |in-ctx f

  fiber : {A B : Set} → (f : A → B) → B → Set
  fiber f b = Σ[ a ∈ _ ] f a == b

  record _≃_ (A B : Set) : Set where

    field

      f : A → B
      g : B → A

      η : (a : A) → a == g (f a)
      ε : (b : B) → f (g b) == b

  id-equiv : (A : Set) → A ≃ A
  id-equiv A = record { 
                 f = λ a → a ; 
                 g = λ a → a ; 
                 η = λ a → idp ; 
                 ε = λ a → idp 
               }

  ap : ∀ {i j} {A : Set i} {B : Set j} (f : A → B) {x y : A}
    → (x == y → f x == f y)
  ap f idp = idp

  coe : ∀ {i} {A B : Set i} (p : A == B) → A → B
  coe idp x = x

  coe! : ∀ {i} {A B : Set i} (p : A == B) → B → A
  coe! idp x = x

  transport : ∀ {i j} {A : Set i} (B : A → Set j) {x y : A} (p : x == y)
    → (B x → B y)
  transport B p = coe (ap B p)

  transport! : ∀ {i j} {A : Set i} (B : A → Set j) {x y : A} (p : x == y)
    → (B y → B x)
  transport! B p = coe! (ap B p)

  infixr 8 _∙_

  _∙_ : ∀ {i} → {A : Set i} → {x y z : A}
    → (x == y → y == z → x == z)
  idp ∙ q = q
  
  ! : ∀ {i} → {A : Set i} → {x y : A} → (x == y → y == x)
  ! idp = idp

  ≤-suc : ∀ {m n} → m ≤ n → m ≤ suc n
  ≤-suc z≤n = z≤n
  ≤-suc (s≤s m≤n) = s≤s (≤-suc m≤n)

  ≤-refl : ∀ {n} → n ≤ n
  ≤-refl {zero} = z≤n
  ≤-refl {suc n} = s≤s ≤-refl

  ≤-suc-lem : {m n : ℕ} → suc m ≤ n → m ≤ n
  ≤-suc-lem (s≤s sm≤n) = ≤-suc sm≤n

  Δ-lem : ∀ {m n} → (m≤n : m ≤ n) → Δ (≤-suc m≤n) == suc (Δ m≤n)
  Δ-lem z≤n = idp
  Δ-lem (s≤s m≤n) = Δ-lem m≤n

  Δ-refl-lem : ∀ {n} → Δ (≤-refl {n}) == 0
  Δ-refl-lem {zero} = idp
  Δ-refl-lem {suc n} = Δ-refl-lem {n}

  Δ-suc-lem : ∀ {m n} → (sm≤n : suc m ≤ n) → suc (Δ sm≤n) == Δ (≤-suc-lem sm≤n)
  Δ-suc-lem (s≤s sm≤n) = ! (Δ-lem sm≤n)

  Δ-≤-lem : ∀ {m n} → (m≤n : m ≤ n) → (Δ m≤n) ≤ n
  Δ-≤-lem z≤n = ≤-refl
  Δ-≤-lem (s≤s m≤n) = ≤-suc (Δ-≤-lem m≤n)

  Δ-≤-lem-eq : ∀ {m n} → (m≤n : m ≤ n) → Δ (Δ-≤-lem m≤n) == m
  Δ-≤-lem-eq {n = n} z≤n = Δ-refl-lem {n}
  Δ-≤-lem-eq (s≤s m≤n) = Δ-lem (Δ-≤-lem m≤n) ∙ ap suc (Δ-≤-lem-eq m≤n)

  +-suc : ∀ {m n} → (m + (suc n)) == suc (m + n) 
  +-suc {zero} {n} = idp
  +-suc {suc m} {n} = ap suc (+-suc {m} {n})

  +-unit-r : ∀ {n} → n == (n + zero)
  +-unit-r {zero} = idp
  +-unit-r {suc n} = ap suc +-unit-r

  +-sym : ∀ {n m} → (n + m) == (m + n)
  +-sym {zero} {m} = +-unit-r
  +-sym {suc n} {m} = ap suc (+-sym {n = n}) ∙ ! (+-suc {m} {n})

  +-≤-lem : ∀ {n m} → n ≤ n + m
  +-≤-lem {zero} = z≤n
  +-≤-lem {suc n} = s≤s +-≤-lem

  Δ-+-lem : ∀ {m n} → (m≤n : m ≤ n) → (Δ m≤n + m) == n
  Δ-+-lem z≤n = ! +-unit-r
  Δ-+-lem (s≤s {m} {n} m≤n) = +-suc {Δ m≤n} {m} ∙ ap suc (Δ-+-lem m≤n)
