--
--  Suite.agda - Indexed sequences
--
--  Eric Finster
--

open import Prelude
open import Mtl
open import Nesting

module Suite where

  infixl 4 _▶_

  open Monad maybeM hiding (fmap ; η ; μ)

  data IncSeq (P : ℕ → Set) : ℕ → ℕ → Set where
    ∥ : ∀ {n} → IncSeq P n 0 
    _▶_ : ∀ {n k} → IncSeq P n k → P (n + k) → IncSeq P n (suc k)

  data DecSeq (P : ℕ → Set) : ℕ → ℕ → Set where
    ∣ : ∀ {n} → DecSeq P n 0
    _◀_ : ∀ {n k} → P n → DecSeq P (suc n) k → DecSeq P n (suc k) 

  DimSeq : (P : ℕ → Set) → ℕ → Set
  DimSeq P n = IncSeq P zero (suc n) 

  head : {n : ℕ} → {P : ℕ → Set} → DimSeq P n → P n
  head (ds ▶ d) = d
  
  tail : {n : ℕ} → {P : ℕ → Set} → DimSeq P (suc n) → DimSeq P n
  tail (ds ▶ x) = ds

  data SeqZip (P : ℕ → Set) (n m k : ℕ) : Set where
    sz : (sk≤m : (suc k ≤ m)) → (ls : IncSeq P n k) → (fcs : P (n + k)) → 
         (rs : DecSeq P (suc (n + k)) (Δ sk≤m)) → SeqZip P n m k

  seqZipFocus : {P : ℕ → Set} → {n m k : ℕ} → SeqZip P n m k → P (n + k)
  seqZipFocus (sz sk≤m ls fcs rs) = fcs

  module _ (P : ℕ → Set) (n m : ℕ) where

    move-left : {k : ℕ} → SeqZip P n m (suc k) → SeqZip P n m k
    move-left {k} (sz sk≤m (ls ▶ l) fcs rs) = 
      let left-lemma : _
          left-lemma = DecSeq P (n + suc k) (suc (Δ sk≤m))              =⟨ +-suc {n} {k} |in-ctx (λ l → DecSeq P l (suc (Δ sk≤m))) ⟩
                       DecSeq P (suc (n + k)) (suc (Δ sk≤m))            =⟨ Δ-suc-lem sk≤m |in-ctx (λ l → DecSeq P (suc (n + k)) l) ⟩
                       DecSeq P (suc (n + k)) (Δ (≤-suc-lem sk≤m)) ∎
      in
        sz (≤-suc-lem sk≤m) ls l (coe left-lemma (fcs ◀ rs)) 

    -- move-right : {P : ℕ → Set} → {n m : ℕ} → SeqZip P n m → Maybe (SeqZip P n m)
    -- move-right (sz k₀ .0 ev zig fcs ∣) = nothing
    -- move-right (sz k₀ .(suc k₁) ev ls fcs (_◀_ .{_} {k₁} r rs)) = 
    --   just (sz (suc k₀) k₁ (ap suc (! (+-suc {k₀} {k₁})) ∙ ev) (ls ▶ fcs) r rs)

    incToZip : IncSeq P n (suc m) → SeqZip P n (suc m) m
    incToZip (ls ▶ l) = 
      let to-zip-lemma : _
          to-zip-lemma = DecSeq P (suc (n + m)) 0                    =⟨ ! (Δ-refl-lem {suc m}) |in-ctx (λ l → DecSeq P (suc (n + m)) l) ⟩ 
                         DecSeq P (suc (n + m)) (Δ (≤-refl {suc m})) ∎
      in
        sz (≤-refl {suc m}) ls l (coe to-zip-lemma ∣) 

    seek-left : {k : ℕ} → (l : ℕ) → (l≤k : l ≤ k) → SeqZip P n m k → SeqZip P n m (Δ l≤k)
    seek-left {k} .0 z≤n zip = zip
    seek-left .{suc k} ._ (s≤s {l} {k} l≤k) zip = 
      let rec₀ : SeqZip P n m (Δ (≤-suc l≤k))
          rec₀ = seek-left {suc k} l (≤-suc l≤k) zip

          rec₁ : SeqZip P n m (suc (Δ l≤k))
          rec₁ = transport (λ i → SeqZip P n m i) (Δ-lem l≤k) rec₀
      in move-left rec₁


  openAt : (P : ℕ → Set) → (n m k : ℕ) → (k≤m : k ≤ m) → IncSeq P n (suc m) → SeqZip P n (suc m) k
  openAt P n m k k≤m seq = let atEnd : SeqZip P n (suc m) (Δ (Δ-≤-lem k≤m))
                               atEnd = seek-left P n (suc m) (Δ k≤m) (Δ-≤-lem k≤m) (incToZip P n m seq)
                             in transport (λ l → SeqZip P n (suc m) l) (Δ-≤-lem-eq k≤m) atEnd

  getAt : {P : ℕ → Set} → (n k : ℕ) → (k≤n : k ≤ n) → DimSeq P n → P k
  getAt {P} n k k≤n ds = seqZipFocus (openAt P 0 n k k≤n ds)

  traverseDecSeq : {n k : ℕ} → {P Q : ℕ → Set} → (f : (m : ℕ) → (n ≤ m) → P m → Maybe (Q m)) → DecSeq P n k → Maybe (DecSeq Q n k)
  traverseDecSeq f ∣ = just ∣
  traverseDecSeq {n} .{_} {P} {Q} f (hd ◀ seq) = 
    traverseDecSeq fₛ seq 
    >>= (λ seq₀ → f n ≤-refl hd 
    >>= (λ hd₀ → just (hd₀ ◀ seq₀)))

    where fₛ : (m : ℕ) → (suc n ≤ m) → P m → Maybe (Q m)
          fₛ m sn≤m p = f m (≤-suc-lem sn≤m) p

