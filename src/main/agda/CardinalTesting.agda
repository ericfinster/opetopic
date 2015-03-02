--
--  CardinalTesting.agda - Testing cardinal implementation
--

{-# OPTIONS --no-termination-check #-}

open import Prelude
open import Mtl
open import Tree
open import Nesting
open import Suite
open import Complex
open import Cardinal

module CardinalTesting where

  open Monad maybeM hiding (fmap ; η ; μ)

  fred-card4 : CardinalNesting 4 ℕ
  fred-card4 = (pt (node (node (node (node (ext 27) (node leaf (node (node leaf (node leaf (node leaf (pt (node (node leaf (node (node leaf leaf) (pt (node leaf (pt leaf))))) (pt (node leaf (pt leaf)))))))) (node (node leaf (node leaf (pt leaf))) (pt (node leaf (pt (node (node (node leaf (node leaf (node leaf (pt leaf)))) (node (node leaf (node leaf (pt leaf))) (pt leaf))) (pt leaf))))))))) (node leaf (node (node leaf (node leaf (pt leaf))) (pt (node (node leaf (node (node leaf leaf) (pt (node leaf (pt leaf))))) (pt (node (node leaf (node (node leaf (node leaf (pt leaf))) (pt leaf))) (pt leaf)))))))) (node leaf (pt (node leaf (pt (node leaf (pt leaf))))))) (pt leaf)))

  fred-card3 : CardinalNesting 3 ℕ
  fred-card3 = (pt (node (node (node (int 26 (node (ext 25) (node (node (ext 24) (node leaf (node leaf (pt (node (node leaf (node (node leaf leaf) (pt (node leaf (pt leaf))))) (pt (node leaf (pt leaf)))))))) (node (node leaf (node leaf (pt leaf))) (pt (node leaf (pt (node (node (node (ext 23) (node leaf (node leaf (pt leaf)))) (node (node leaf (node leaf (pt leaf))) (pt leaf))) (pt leaf))))))))) (node leaf (node (node leaf (node leaf (pt leaf))) (pt (node (node leaf (node (node leaf leaf) (pt (node leaf (pt leaf))))) (pt (node (node leaf (node (node leaf (node leaf (pt leaf))) (pt leaf))) (pt leaf)))))))) (node leaf (pt (node leaf (pt (node leaf (pt leaf))))))) (pt leaf)))

  fred-card2 : CardinalNesting 2 ℕ
  fred-card2 = (pt (node (node (int 22 (node (int 21 (node (ext 18) (node leaf (pt (node (node (ext 17) (node (node (ext 16) leaf) (pt (node leaf (pt leaf))))) (pt (node leaf (pt leaf)))))))) (node (node (ext 19) (node leaf (pt leaf))) (pt (node leaf (pt (node (node (int 20 (node (ext 15) (node leaf (pt leaf)))) (node (node (ext 14) (node leaf (pt leaf))) (pt leaf))) (pt leaf)))))))) (node leaf (pt (node leaf (pt (node leaf (pt leaf))))))) (pt leaf)))

  fred-card1 : CardinalNesting 1 ℕ
  fred-card1 = (pt (node (int 13 (node (int 12 (node (ext 7) (pt leaf))) (pt (node (int 11 (node (int 10 (leaf)) (pt (node (ext 6) (pt leaf))))) (pt (node (int 9 (node (int 8 (node (ext 5) (pt leaf))) (pt leaf))) (pt leaf))))))) (pt leaf)))

  fred-card0 : CardinalNesting 0 ℕ
  fred-card0 = (pt (int 4 (pt (int 3 (pt (int 2 (pt (obj 1))))))))

  fred-card : Cardinal 4 ℕ
  fred-card = ∥ ▶ fred-card0 ▶ fred-card1 ▶ fred-card2 ▶ fred-card3 ▶ fred-card4

  --
  -- A 3-glob
  --

  three-glob3 : CardinalNesting 3 ℕ
  three-glob3 = (pt (node (node (node (ext 7) (node leaf (node leaf (pt leaf)))) (node leaf (pt leaf))) (pt leaf)))

  three-glob2 : CardinalNesting 2 ℕ
  three-glob2 = (pt (node (node (int 6 (node (ext 5) (node leaf (pt leaf)))) (node leaf (pt leaf))) (pt leaf)))

  three-glob1 : CardinalNesting 1 ℕ
  three-glob1 = (pt (node (int 4 (node (ext 3) (pt leaf))) (pt leaf)))

  three-glob0 : CardinalNesting 0 ℕ
  three-glob0 = (pt (int 2 (pt (obj 1))))

  --
  -- Extended in dimension 0
  --
  
  three-glob-ext3 : CardinalNesting 3 ℕ
  three-glob-ext3 = (pt (node leaf (pt (node (node (node (ext 7) (node leaf (node leaf (pt leaf)))) (node leaf (pt leaf))) (pt leaf)))))

  three-glob-ext2 : CardinalNesting 2 ℕ
  three-glob-ext2 = (pt (node leaf (pt (node (node (int 6 (node (ext 5) (node leaf (pt leaf)))) (node leaf (pt leaf))) (pt leaf)))))

  three-glob-ext1 : CardinalNesting 1 ℕ
  three-glob-ext1 = (pt (node (ext 9) (pt (node (int 4 (node (ext 3) (pt leaf))) (pt leaf)))))

  three-glob-ext0 : CardinalNesting 0 ℕ
  three-glob-ext0 = (pt (int 8 (pt (int 2 (pt (obj 1))))))

  doThreeGlob2Ext : Maybe (CardinalNesting 2 ℕ)
  doThreeGlob2Ext = 
    tailDeriv {1} {0} z≤n three-glob2 (∥ ▶ []) 
    >>= (λ { (∂ , seq) → flatten seq 
    >>= (λ flt → just (plug-cardinal ∂ (node leaf (pt seq)) )) })

  --
  -- Closing the tree in dim 1
  --

  triangle3 : CardinalNesting 3 ℕ
  triangle3 = (pt (node (node leaf (node leaf (pt (node (node (node (ext 7) (node leaf (node leaf (pt leaf)))) (node leaf (pt leaf))) (pt leaf))))) (pt leaf)))

  triangle2 : CardinalNesting 2 ℕ
  triangle2 = (pt (node (node (ext 11) (node leaf (pt (node (node (int 6 (node (ext 5) (node leaf (pt leaf)))) (node leaf (pt leaf))) (pt leaf))))) (pt leaf)))

  triangle1 : CardinalNesting 1 ℕ
  triangle1 = (pt (node (int 10 (node (ext 9) (pt (node (int 4 (node (ext 3) (pt leaf))) (pt leaf))))) (pt leaf)))

  triangle0 : CardinalNesting 0 ℕ
  triangle0 = (pt (int 8 (pt (int 2 (pt (obj 1))))))

  -- Hmm.  So you're going to need a more complicated example in order to understand this.  It looks like you need to put those higher guys inside a corolla of
  -- some kind, but I don't really see what .....
  
  --
  --  A more complicated example
  --

  batman4 : CardinalNesting 4 ℕ
  batman4 = pt
            (node
             (node
              (node
               (node (ext 29)
                (node leaf (node leaf (node leaf (pt (node leaf (pt leaf)))))))
               (node leaf (node leaf (pt (node leaf (pt leaf))))))
              (node leaf
               (pt
                (node
                 (node leaf (node leaf (pt (node (node leaf leaf) (pt leaf)))))
                 (pt leaf)))))
             (pt
              (node
               (node
                (node
                 (node (ext 28)
                  (node leaf
                   (node leaf (node (node leaf (node leaf (pt leaf))) (pt leaf)))))
                 (node (node leaf (node leaf (node leaf (pt leaf))))
                  (node (node leaf (node leaf (pt leaf))) (pt leaf))))
                (node leaf (pt leaf)))
               (pt (node leaf (pt leaf))))))

  batman3 : CardinalNesting 3 ℕ
  batman3 = pt
            (node
             (node
              (node
               (int 27
                (node (ext 26) (node leaf (node leaf (pt (node leaf (pt leaf)))))))
               (node leaf (node leaf (pt (node leaf (pt leaf))))))
              (node leaf
               (pt
                (node
                 (node leaf (node leaf (pt (node (node leaf leaf) (pt leaf)))))
                 (pt leaf)))))
             (pt
              (node
               (node
                (node
                 (int 25
                  (node (ext 24)
                   (node leaf (node (node leaf (node leaf (pt leaf))) (pt leaf)))))
                 (node (node (ext 23) (node leaf (node leaf (pt leaf))))
                  (node (node leaf (node leaf (pt leaf))) (pt leaf))))
                (node leaf (pt leaf)))
               (pt (node leaf (pt leaf))))))

  batman2 : CardinalNesting 2 ℕ
  batman2 = pt
            (node
             (node
              (int 22 (node (ext 21) (node leaf (pt (node leaf (pt leaf))))))
              (node leaf
               (pt
                (node
                 (node (ext 20)
                  (node leaf (pt (node (node (ext 19) leaf) (pt leaf)))))
                 (pt leaf)))))
             (pt
              (node
               (node
                (int 18
                 (node (int 17 (node (ext 16) (node leaf (pt leaf))))
                  (node (node (ext 15) (node leaf (pt leaf))) (pt leaf))))
                (node leaf (pt leaf)))
               (pt (node leaf (pt leaf))))))

  batman1 : CardinalNesting 1 ℕ
  batman1 = pt
            (node
             (int 14
              (node (ext 11)
               (pt
                (node (int 13 (node (ext 10) (pt (node (int 12 leaf) (pt leaf)))))
                 (pt leaf)))))
             (pt
              (node (int 9 (node (int 8 (node (ext 7) (pt leaf))) (pt leaf)))
               (pt (node (ext 6) (pt leaf))))))

  batman0 : CardinalNesting 0 ℕ
  batman0 = (pt (int 5 (pt (int 4 (pt (int 3 (pt (int 2 (pt (obj 1))))))))))

  batman : Cardinal 4 ℕ
  batman = ∥ ▶ batman0 ▶ batman1 ▶ batman2 ▶ batman3 ▶ batman4

  batman2-tree : Tree 2 (Address 2)
  batman2-tree = node [] -- 14
                   (node leaf
                    (pt
                     (node
                      (node (([] ∷ []) ∷ []) -- 13
                       (node leaf
                        (pt (node (node (([] ∷ []) ∷ ([] ∷ []) ∷ []) leaf) (pt leaf))))) -- 12
                      (pt leaf))))


  batman-init-addr : CardinalAddress 1
  batman-init-addr = ∥ ▶ [] ▶ []

  batman-test-addr : CardinalAddress 2
  batman-test-addr = ∥ ▶ [] ▶ [] ▶ ([] ∷ []) ∷ []

  batman-hd-addr : Address 2
  batman-hd-addr = ([] ∷ []) ∷ []

  -- blorp : Maybe (Tree 3 (Nesting 3 ℕ))
  -- blorp = tailSeq (≤-refl {2}) batman3 batman-test-addr

  -- bloop : Maybe (Tree 2 (Nesting 2 ℕ))
  -- bloop = tailSeq (≤-refl {1}) batman2 batman-init-addr 

  -- bleep : Maybe (Nesting 2 ℕ)
  -- bleep = poke batman2 batman-test-addr >>= (λ res → just (proj₂ res))

  -- blonk : Maybe (Tree 2 (Nesting 2 ℕ))
  -- blonk = tailSeq (≤-refl {1}) batman2 batman-init-addr >>= (λ res → seekTo batman-hd-addr res >>= (λ z → just (proj₁ z)))

  -- bonzo : Maybe (TreeSeq 2 1 (Nesting 3 ℕ))
  -- bonzo = tailSeq {2} {1} (s≤s z≤n) batman3 batman-init-addr >>= (λ res → seekTo batman-hd-addr res >>= (λ z → just (proj₁ z)))

  -- blomp : Maybe (TreeSeq 2 2 (Nesting 4 ℕ))
  -- blomp = tailSeq {3} {1} (s≤s z≤n) batman4 batman-init-addr >>= (λ res → seekTo batman-hd-addr res >>= (λ z → just (proj₁ z)))

  msk : Tree 2 (Nesting 2 ℕ)
  msk = node (ext 20) (node leaf (pt (node (node (ext 19) leaf) (pt leaf))))


  --
  --  And its extrusion
  --

  superman4 : CardinalNesting 4 ℕ
  superman4 = pt
              (node
               (node
                (node
                 (node (ext 29)
                  (node leaf (node leaf (node leaf (pt (node leaf (pt leaf)))))))
                 (node leaf (node leaf (pt (node leaf (pt leaf))))))
                (node leaf
                 (pt
                  (node
                   (node
                    (node leaf
                     (node leaf (node leaf (pt (node (node leaf leaf) (pt leaf))))))
                    (node leaf (pt leaf)))
                   (pt leaf)))))
               (pt
                (node
                 (node
                  (node
                   (node (ext 28)
                    (node leaf
                     (node leaf (node (node leaf (node leaf (pt leaf))) (pt leaf)))))
                   (node (node leaf (node leaf (node leaf (pt leaf))))
                    (node (node leaf (node leaf (pt leaf))) (pt leaf))))
                  (node leaf (pt leaf)))
                 (pt (node leaf (pt leaf))))))

  superman3 : CardinalNesting 3 ℕ
  superman3 = pt
              (node
               (node
                (node
                 (int 27
                  (node (ext 26) (node leaf (node leaf (pt (node leaf (pt leaf)))))))
                 (node leaf (node leaf (pt (node leaf (pt leaf))))))
                (node leaf
                 (pt
                  (node
                   (node
                    (node (ext 31)
                     (node leaf (node leaf (pt (node (node leaf leaf) (pt leaf))))))
                    (node leaf (pt leaf)))
                   (pt leaf)))))
               (pt
                (node
                 (node
                  (node
                   (int 25
                    (node (ext 24)
                     (node leaf (node (node leaf (node leaf (pt leaf))) (pt leaf)))))
                   (node (node (ext 23) (node leaf (node leaf (pt leaf))))
                    (node (node leaf (node leaf (pt leaf))) (pt leaf))))
                  (node leaf (pt leaf)))
                 (pt (node leaf (pt leaf))))))

  superman2 : CardinalNesting 2 ℕ
  superman2 = pt
              (node
               (node
                (int 22 (node (ext 21) (node leaf (pt (node leaf (pt leaf))))))
                (node leaf
                 (pt
                  (node
                   (node
                    (int 30
                     (node (ext 20)
                      (node leaf (pt (node (node (ext 19) leaf) (pt leaf))))))
                    (node leaf (pt leaf)))
                   (pt leaf)))))
               (pt
                (node
                 (node
                  (int 18
                   (node (int 17 (node (ext 16) (node leaf (pt leaf))))
                    (node (node (ext 15) (node leaf (pt leaf))) (pt leaf))))
                  (node leaf (pt leaf)))
                 (pt (node leaf (pt leaf))))))

  superman1 : CardinalNesting 1 ℕ
  superman1 = pt
              (node
               (int 14
                (node (ext 11)
                 (pt
                  (node (int 13 (node (ext 10) (pt (node (int 12 leaf) (pt leaf)))))
                   (pt leaf)))))
               (pt
                (node (int 9 (node (int 8 (node (ext 7) (pt leaf))) (pt leaf)))
                 (pt (node (ext 6) (pt leaf))))))

  superman0 : CardinalNesting 0 ℕ
  superman0 = (pt (int 5 (pt (int 4 (pt (int 3 (pt (int 2 (pt (obj 1))))))))))

  superman : Cardinal 4 ℕ
  superman = ∥ ▶ superman0 ▶ superman1 ▶ superman2 ▶ superman3 ▶ superman4

  extrusionTest : Maybe (Cardinal 4 ℕ)
  extrusionTest = doExtrude 30 31 msk batman-test-addr batman

  extrusionVerify : just (superman) == extrusionTest
  extrusionVerify = idp

  --
  --  Starting with some low dimensional stuff
  --

  sbs0 : Cardinal 0 ℕ
  sbs0 = objectCardinal 0

  sbs1 : Maybe (Cardinal 1 ℕ)
  sbs1 = doTopRootExtrusion 1 2 sbs0

  sbs2 : Maybe (Cardinal 1 ℕ)
  sbs2 = sbs1 >>= λ c → doRootExtrusion 0 z≤n 3 4 c

  sbs3 : Maybe (Cardinal 2 ℕ)
  sbs3 = sbs2 >>= (λ c → doTopRootExtrusion 5 6 c)

  sbs4 : Maybe (Cardinal 2 ℕ)
  sbs4 = sbs3 >>= (λ c → doRootExtrusion 1 ≤-refl 7 8 c)

  complex4 : Maybe (Complex (Polarity ℕ) 2)
  complex4 = sbs4 >>= (λ c → just (toComplex c))

