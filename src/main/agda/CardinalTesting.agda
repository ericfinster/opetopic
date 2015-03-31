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

  -- The following shows that as we pass into higher
  -- cardinals which are empty, the type changes a bit.
  -- What to do about this???

  empty-card-0 : CardinalNesting 0 ℕ
  empty-card-0 = (pt (obj 0))

  empty-card-1 : CardinalNesting 1 ℕ
  empty-card-1 = (pt leaf)

  empty-card-2 : Tree 1 (Tree 2 ℕ)
  empty-card-2 = (node leaf (pt leaf))

  empty-card-3 : Tree 2 (Tree 3 ℕ)
  empty-card-3 = (node leaf (node leaf (pt leaf)))

  empty-card-4 : Tree 3 (Tree 4 ℕ)
  empty-card-4 = (node leaf (node leaf (node leaf (pt leaf))))

  empty-card-5 : Tree 4 (Tree 5 ℕ)
  empty-card-5 = (node leaf (node leaf (node leaf (node leaf (pt leaf)))))

  --
  --  Four glob
  --

  four-glob-4 : CardinalNesting 4 ℕ
  four-glob-4 = (pt (node (node (node (node (ext 8) (node leaf (node leaf (node leaf (pt leaf))))) (node leaf (node leaf (pt leaf)))) (node leaf (pt leaf))) (pt leaf)))

  four-glob-3 : CardinalNesting 3 ℕ
  four-glob-3 = (pt (node (node (node (int 7 (node (ext 6) (node leaf (node leaf (pt leaf))))) (node leaf (node leaf (pt leaf)))) (node leaf (pt leaf))) (pt leaf)))

  four-glob-2 : CardinalNesting 2 ℕ
  four-glob-2 = (pt (node (node (int 5 (node (ext 4) (node leaf (pt leaf)))) (node leaf (pt leaf))) (pt leaf)))

  four-glob-1 : CardinalNesting 1 ℕ
  four-glob-1 = (pt (node (int 3 (node (ext 2) (pt leaf))) (pt leaf)))

  four-glob-0 : CardinalNesting 0 ℕ
  four-glob-0 = (pt (int 1 (pt (obj 0))))

  --
  --  Drop in dimension 2
  --

  drop-ext-4 : CardinalNesting 4 ℕ
  drop-ext-4 = (pt (node (node (node leaf leaf) (node (node (node (node (ext 8) (node leaf (node leaf (node leaf (pt leaf))))) (node leaf (node leaf (pt leaf)))) (node leaf (pt leaf))) (pt leaf))) (pt leaf)))

  drop-ext-3 : CardinalNesting 3 ℕ
  drop-ext-3 = (pt (node (node (node (ext 10) leaf) (node (node (node (int 7 (node (ext 6) (node leaf (node leaf (pt leaf))))) (node leaf (node leaf (pt leaf)))) (node leaf (pt leaf))) (pt leaf))) (pt leaf)))

  drop-ext-2 : CardinalNesting 2 ℕ
  drop-ext-2 = (pt (node (node (int 9 (leaf)) (node (node (int 5 (node (ext 4) (node leaf (pt leaf)))) (node leaf (pt leaf))) (pt leaf))) (pt leaf)))

  drop-ext-1 : CardinalNesting 1 ℕ
  drop-ext-1 = (pt (node (int 3 (node (ext 2) (pt leaf))) (pt leaf)))

  drop-ext-0 : CardinalNesting 0 ℕ
  drop-ext-0 = (pt (int 1 (pt (obj 0))))

  --
  -- Addressing
  --

  test : Maybe (CardinalDerivative 1 (Tree 2 (Nesting 2 ℕ)) × (Tree 2 (Nesting 2 ℕ)))
  test = poke four-glob-2 (∥ ▶ [] ▶ [])

  test2 : Maybe (CardinalNesting 2 ℕ)
  test2 = test >>= (λ { (∂ , tr) → just (plug-cardinal ∂ (node (int 9 leaf) (node tr (pt leaf)))) })

  test3 : just drop-ext-2 == test2
  test3 = idp

  test4 : Maybe (CardinalDerivative 0 (Tree 1 (Tree 2 (Nesting 2 ℕ))) × (Tree 1 (Tree 2 (Nesting 2 ℕ))))
  test4 = poke four-glob-2 (∥ ▶ [])

  test5 : Maybe (CardinalNesting 2 ℕ)
  test5 = test4 >>= (λ { (∂ , tr) → just (plug-cardinal ∂ (node (node (int 9 leaf) tr) (pt leaf))) })

  test6 : just drop-ext-2 == test5
  test6 = idp

  -- (pt (node (node (int 5 (node (ext 4) (node leaf (pt leaf)))) (node leaf (pt leaf))) (pt leaf)))
  -- (pt (node (node (int 9 (leaf)) (node (node (int 5 (node (ext 4) (node leaf (pt leaf)))) (node leaf (pt leaf))) (pt leaf))) (pt leaf)))
  --                                      (node (int 5 (node (ext 4) (node leaf (pt leaf)))) (node leaf (pt leaf)))




  --
  --  A Suitably Complex Example
  --

  batman-5 : CardinalNesting 5 ℕ
  batman-5 = (pt (node (node (node (node (node (ext 16) (node leaf (node leaf (node leaf (node (node leaf (node leaf (pt leaf))) (pt (node (node leaf (node leaf (pt leaf))) (pt leaf)))))))) (node leaf (node leaf (node (node leaf (node leaf (pt leaf))) (pt (node (node leaf (node leaf (pt leaf))) (pt leaf))))))) (node leaf (node (node leaf (node leaf (pt leaf))) (pt (node (node leaf (node leaf (pt leaf))) (pt leaf)))))) (node leaf (pt (node leaf (pt leaf))))) (pt leaf)))

  batman-4 : CardinalNesting 4 ℕ
  batman-4 = (pt (node (node (node (node (int 15 (node (ext 14) (node leaf (node leaf (node (node leaf (node leaf (pt leaf))) (pt (node (node leaf (node leaf (pt leaf))) (pt leaf)))))))) (node leaf (node leaf (node (node leaf (node leaf (pt leaf))) (pt (node (node leaf (node leaf (pt leaf))) (pt leaf))))))) (node leaf (node (node leaf (node leaf (pt leaf))) (pt (node (node leaf (node leaf (pt leaf))) (pt leaf)))))) (node leaf (pt (node leaf (pt leaf))))) (pt leaf)))

  batman-3 : CardinalNesting 3 ℕ
  batman-3 = (pt (node (node (node (int 13 (node (ext 12) (node leaf (node (node leaf (node leaf (pt leaf))) (pt (node (node leaf (node leaf (pt leaf))) (pt leaf))))))) (node leaf (node (node leaf (node leaf (pt leaf))) (pt (node (node leaf (node leaf (pt leaf))) (pt leaf)))))) (node leaf (pt (node leaf (pt leaf))))) (pt leaf)))

  batman-2 : CardinalNesting 2 ℕ
  batman-2 = (pt (node (node (int 11 (node (ext 10) (node (node (ext 9) (node leaf (pt leaf))) (pt (node (node (ext 8) (node leaf (pt leaf))) (pt leaf)))))) (node leaf (pt (node leaf (pt leaf))))) (pt leaf)))

  batman-1 : CardinalNesting 1 ℕ
  batman-1 = (pt (node (int 7 (node (int 6 (node (ext 5) (pt leaf))) (pt (node (int 4 (node (ext 3) (pt leaf))) (pt leaf))))) (pt leaf)))

  batman-0 : CardinalNesting 0 ℕ
  batman-0 = (pt (int 2 (pt (int 1 (pt (obj 0))))))

  batman-pos : Maybe (CardinalDerivative 2 (Tree 3 (Nesting 3 ℕ)) × (Tree 3 (Nesting 3 ℕ)))
  batman-pos = poke batman-3 (∥ ▶ [] ▶ [] ▶ [])

  batman-sh : Maybe (Tree 1 (Tree 2 (Tree 3 (Nesting 3 ℕ))))
  batman-sh = batman-pos >>= (λ { (∂ , _) → just (proj₁ (proj₂ ∂)) })

  batman-test : Maybe (CardinalNesting 3 ℕ)
  batman-test = extrudeLoopAt 17 batman-3 (∥ ▶ [] ▶ [] ▶ [])

  batman-test2 : Maybe (CardinalNesting 4 ℕ)
  batman-test2 = extrudeDropAt 18 batman-4 (∥ ▶ [] ▶ [] ▶ [])

  batman-test3 : Maybe (CardinalNesting 5 ℕ)
  batman-test3 = padWithDropLeaf batman-5 (∥ ▶ [] ▶ [] ▶ [])

  --
  -- It's Extension
  --

  superman-5 : CardinalNesting 5 ℕ
  superman-5 = (pt (node (node (node (node leaf leaf) (node (node (node (node (ext 16) (node leaf (node leaf (node leaf (node (node leaf (node leaf (pt leaf))) (pt (node (node leaf (node leaf (pt leaf))) (pt leaf)))))))) (node leaf (node leaf (node (node leaf (node leaf (pt leaf))) (pt (node (node leaf (node leaf (pt leaf))) (pt leaf))))))) (node leaf (node (node leaf (node leaf (pt leaf))) (pt (node (node leaf (node leaf (pt leaf))) (pt leaf)))))) (node leaf (pt (node leaf (pt leaf)))))) (node leaf (pt (node leaf (pt leaf))))) (pt leaf)))

  superman-4 : CardinalNesting 4 ℕ
  superman-4 = (pt (node (node (node (node (ext 18) leaf) (node (node (node (int 15 (node (ext 14) (node leaf (node leaf (node (node leaf (node leaf (pt leaf))) (pt (node (node leaf (node leaf (pt leaf))) (pt leaf)))))))) (node leaf (node leaf (node (node leaf (node leaf (pt leaf))) (pt (node (node leaf (node leaf (pt leaf))) (pt leaf))))))) (node leaf (node (node leaf (node leaf (pt leaf))) (pt (node (node leaf (node leaf (pt leaf))) (pt leaf)))))) (node leaf (pt (node leaf (pt leaf)))))) (node leaf (pt (node leaf (pt leaf))))) (pt leaf)))

  superman-3 : CardinalNesting 3 ℕ
  superman-3 = (pt (node (node (node (int 17 (leaf)) (node (node (int 13 (node (ext 12) (node leaf (node (node leaf (node leaf (pt leaf))) (pt (node (node leaf (node leaf (pt leaf))) (pt leaf))))))) (node leaf (node (node leaf (node leaf (pt leaf))) (pt (node (node leaf (node leaf (pt leaf))) (pt leaf)))))) (node leaf (pt (node leaf (pt leaf)))))) (node leaf (pt (node leaf (pt leaf))))) (pt leaf)))

  superman-2 : CardinalNesting 2 ℕ
  superman-2 = (pt (node (node (int 11 (node (ext 10) (node (node (ext 9) (node leaf (pt leaf))) (pt (node (node (ext 8) (node leaf (pt leaf))) (pt leaf)))))) (node leaf (pt (node leaf (pt leaf))))) (pt leaf)))

  superman-1 : CardinalNesting 1 ℕ
  superman-1 = (pt (node (int 7 (node (int 6 (node (ext 5) (pt leaf))) (pt (node (int 4 (node (ext 3) (pt leaf))) (pt leaf))))) (pt leaf)))

  superman-0 : CardinalNesting 0 ℕ
  superman-0 = (pt (int 2 (pt (int 1 (pt (obj 0))))))

  superman-test : just superman-3 == batman-test
  superman-test = idp

  superman-test2 : just superman-4 == batman-test2
  superman-test2 = idp

  superman-test3 : just superman-5 == batman-test3
  superman-test3 = idp
