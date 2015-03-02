--
--  ComplexExamples.agda - Complex Examples
--

open import Prelude
open import Mtl
open import Tree
open import Nesting
open import Suite
open import Complex

module ComplexExamples where

  fred4 : Nesting 4 ℕ
  fred4 = ext 27

  fred3 : Nesting 3 ℕ
  fred3 = int 26 (node (ext 25) (node (node (ext 24) (node leaf (node leaf (pt (node (node leaf (node (node leaf leaf) (pt (node leaf (pt leaf))))) (pt (node leaf (pt leaf)))))))) (node (node leaf (node leaf (pt leaf))) (pt (node leaf (pt (node (node (node (ext 23) (node leaf (node leaf (pt leaf)))) (node (node leaf (node leaf (pt leaf))) (pt leaf))) (pt leaf))))))))

  fred2 : Nesting 2 ℕ
  fred2 = int 22 (node (int 21 (node (ext 18) (node leaf (pt (node (node (ext 17) (node (node (ext 16) leaf) (pt (node leaf (pt leaf))))) (pt (node leaf (pt leaf)))))))) (node (node (ext 19) (node leaf (pt leaf))) (pt (node leaf (pt (node (node (int 20 (node (ext 15) (node leaf (pt leaf)))) (node (node (ext 14) (node leaf (pt leaf))) (pt leaf))) (pt leaf)))))))

  fred1 : Nesting 1 ℕ
  fred1 = int 13 (node (int 12 (node (ext 7) (pt leaf))) (pt (node (int 11 (node (int 10 (leaf)) (pt (node (ext 6) (pt leaf))))) (pt (node (int 9 (node (int 8 (node (ext 5) (pt leaf))) (pt leaf))) (pt leaf))))))

  fred0 : Nesting 0 ℕ
  fred0 = int 4 (pt (int 3 (pt (int 2 (pt (obj 1))))))

  fred : Complex ℕ 4
  fred = ∥ ▶ fred0 ▶ fred1 ▶ fred2 ▶ fred3 ▶ fred4

  fred3Cmplx : Complex ℕ 3
  fred3Cmplx = ∥ ▶ fred0 ▶ fred1 ▶ fred2 ▶ fred3

  fred3Addrs : Nesting 3 (ℕ × Address 4)
  fred3Addrs = nestingWithAddr fred3

  -- (26 , [])
  -- (25 , [] ∷ [])
  -- (24 , ([] ∷ []) ∷ [])
  -- (23 , ((([] ∷ [] ∷ []) ∷ []) ∷ []) ∷ [])

  blorp : Maybe (ComplexZipper ℕ 3)
  blorp = seekComplex (toZipper fred3Cmplx) (((([] ∷ [] ∷ []) ∷ []) ∷ []) ∷ [])

  fred3Tree : Tree 4 ℕ
  fred3Tree = toTree fred3

  umm : Maybe (Tree 4 ℕ)
  umm = join (∇ fred3Tree)

  test : umm == (just fred3Tree)
  test = idp
  
  test2 : join (∇ (toTree fred2)) == just (toTree fred2)
  test2 = idp
