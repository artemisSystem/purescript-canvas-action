module Test.SemigroupDo where

import Prelude


bind ∷ ∀ m. Semigroup m ⇒ m → (Unit → m) → m
bind a b = a <> b unit

discard ∷ ∀ m. Semigroup m ⇒ m → (Unit → m) → m
discard a b = a <> b unit
