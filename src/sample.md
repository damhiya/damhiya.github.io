# Combinators

Applicative의 `ap` 연산자
```haskell
(<*>) :: Applicative f => f (a -> b) -> f a -> f b
```

S combinator
```haskell
(*) :: (a -> b -> c) -> (a -> b) -> a -> c
f * g = \x -> f x (g x)
```

Y combinator
```haskell
fix f = f (fix f)
```
