
Universal quantification
```haskell
id :: forall a. a -> a
id x = x
```

Existential quantification
```agda
data Σ (A : Type) (B : A → Type) : Type where
  _,_ : ∀ (x : A) → B x → Σ A B
```
