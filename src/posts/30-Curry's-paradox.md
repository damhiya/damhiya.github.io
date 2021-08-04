<!--
Date : 2021.8.1
-->

λ-calculus, turing machine 등의 주제를 논할 때에는 항상 프로그램의 정지 여부에 관한 문제가 따라 나온다.
정지하는 프로그램을 판별하는 일은 커리-하워드 대응과 결부할 때 더욱 중요한데,
종료하지 않는 프로그램을 허용하는 것은 곧 논리체계가 inconsistent 함을 의미하기 때문이다.
이 글 에서는 이런 non-termination을 유발할 수 있는 요소 중 하나인 Curry's paradox에 대해 설명한다.

## Untyped λ-calculus' Y combinator
Untyped λ-calculus의 유용성을 가장 단적으로 보여주는 예시는 바로 Y combinator이다.
\[
  Y \equiv \lambda f . (\lambda x . f (x x)) (\lambda x . f (x x))
\]

\(Y f\)와 \(f (Y f)\)를 한번 계산해보자.
\[
  \begin{array}{rl}
                    & Y f                                             \\
    \longrightarrow & (\lambda x . f (x x)) (\lambda x . f (x x))     \\
    \longrightarrow & f ((\lambda x . f (x x)) (\lambda x . f (x x))) \\
  \end{array}
\]
\[
  \begin{array}{rl}
                    & f (Y f)                                         \\
    \longrightarrow & f ((\lambda x . f (x x)) (\lambda x . f (x x))) \\
  \end{array}
\]
즉 \( Y f = f (Y f) \)를 만족한다.
(여기서 등호 기호는 β-equivalence를 뜻 한다. \(Y f\)와 \(f (Y f)\)가 β-equivalent 하긴 하지만 \(Y f\) 가 \(f (Y f)\) 로 β-reduce되는 것은 아니다.)

수학에서는 \(Y f\) 와 같이 \(x = f(x)\) 를 만족하는 \(x\)를 \(f\)의 fixed point (혹은 그냥 fixpoint) 라고 부른다.
즉 \(Y\)는 임의의 함수 \(f\)를 인자로 받아서 \(f\)의 fixpoint \(Y f\)를 결과로 주는 고차함수라고 이해할 수 있다.
이처럼 \(\operatorname{fix} f = f (\operatorname{fix} f)\)를 만족하는 \(\operatorname{fix}\)를 fixpoint combinator라고 부르며,
fixpoint combinator에는 \(Y\) 말고도 여러가지가 있다.

fixpoint combinator가 무엇인지 직관적인 이해가 잘 안된다면 아래와 같이 iterative하게 생각해 보는 것이 도움이 될지도 모르겠다.
\[
  \begin{array}{rl}
                    & Y f                                             \\
    \longrightarrow & (\lambda x . f (x x)) (\lambda x . f (x x))     \\
    \longrightarrow & f ((\lambda x . f (x x)) (\lambda x . f (x x))) \\
    \longrightarrow & f (f ((\lambda x . f (x x)) (\lambda x . f (x x)))) \\
    \longrightarrow & f (f (f ((\lambda x . f (x x)) (\lambda x . f (x x))))) \\
    \longrightarrow & f (f (f (f (\ldots)))) \\
  \end{array}
\]

fixpoint combinator가 왜 중요할까? fixpoint combinator가 있으면 재귀함수를 만들 수 있기 때문이다.
다음은 하스켈에서 fixpoint combinator를 사용해 팩토리얼 함수를 작성한 것이다.
```haskell
fix f = f (fix f)

factorial = fix $ \f n ->
  if n == 0
    then 1
    else n * f (n-1)
```
위의 코드에서는 fixpoint combinator `fix`를 하스켈이 지원하는 자기참조적 정의를 사용하여 정의했다.
\(Y\)의 정의가 자기참조를 필요로 하지 않는 것과 대조적이다.

그런데 \(Y\)를 사용하면 종료하지 않는 계산을 만들 수 있다.
\[
  I \equiv \lambda x . x
\]
라고 정의하고 \(YI\)를 normal-order를 따라 계산해보자.
\[
  \begin{array}{rl}
                   & YI    \\
    \longrightarrow& (\lambda x . I (x x)) (\lambda x . I (x x)) ) \\
    \longrightarrow& I ( (\lambda x . I (x x)) (\lambda x . I (x x)) ) \\
    \longrightarrow& (\lambda x . I (x x)) (\lambda x . I (x x)) )
  \end{array}
\]
β-reduction을 수행하던 중 중간의 식으로 되돌아 왔으므로 이 계산은 종료하지 않는다.
사실 untyped λ-calculus에서 normal-order reduction이 종료하지 않는다면 β-reduction을 어떤 순서로 실행한다 하여도 계산이 종료할 수 없다.

\(YI\)의 계산이 종료하지 않음으로 untyped λ-calculus에서 weak normalization property가 성립하지 않는다고 말할 수 있다.
사실 더 단순한 예시로 \(\Omega\)가 있다.
\[
  \begin{align*}
    \omega  \equiv & \  \lambda x . x x \\
    \Omega  \equiv & \ \omega \omega
  \end{align*}
\]
\[
  \begin{array}{rl}
                    & \Omega                              \\
    \longrightarrow & (\lambda x . x x) (\lambda x . x x) \\
    \longrightarrow & (\lambda y . y y) (\lambda x . x x) \\
    \longrightarrow & (y y) [y := \lambda x . x x]        \\
    \longrightarrow & (\lambda x . x x) (\lambda x . x x)
  \end{array}
\]

fixpoint combinator에게 타입을 붙여보면 애초에 논리적으로 말이 되지 않음을 알 수 있다.
하스켈에서 `fix`는 다음과 같은 타입을 갖는다.
```haskell
fix :: (a -> a) -> a
fix id :: a
```
Propositions as types의 관점에서 볼 때, 이는 "\(P\) 일 때 \(P\) 라면, \(P\) 이다"를 의미한다.
Tautology에서 임의의 명제를 증명해낼 수 있는 것이다. 즉, 이런 fixpoint combintor를 가지는 논리체계는 inconsistent 하다.
때문에 타입 시스템을 논리체계로서 사용하는 proof assistant 에서는 일반적인 fixpoint combinator를 허용하지 않으며, 제한된 형태로만 사용할 수 있다.

## Writing Y combinator in Haskell
Y combinator를 하스켈에서 만들어보자. 람다식을 하스켈로 번역하는 것은 그리 어렵지 않다.
```haskell
y = \f -> (\x -> f (x x)) (\x -> f (x x))
```
그런데 이 코드를 GHC로 읽어보면 타입 에러가 발생한다.
```haskell
λ> y = \f -> (\x -> f (x x)) (\x -> f (x x))

<interactive>:1:39: error:
    • Occurs check: cannot construct the infinite type: t0 ~ t0 -> t
    • In the first argument of ‘x’, namely ‘x’
      In the first argument of ‘f’, namely ‘(x x)’
      In the expression: f (x x)
    • Relevant bindings include
        x :: t0 -> t (bound at <interactive>:1:29)
        f :: t -> t (bound at <interactive>:1:6)
        y :: (t -> t) -> t (bound at <interactive>:1:1)
```
에러 메시지에 따르면 `(x x)`라는 식의 타입을 검사하던 중
infinite type을 construct해야 하는 문제가 생겼다고 한다.

`(x x)`의 타입추론 과정을 따라가 보자.

- `(x x)`의 타입을 `t`라고 가정한다. `t`는 미결정된 타입 변수이다. 간단히 `(x x) :: t`라고 쓰자.
- `(x x)`에서 사용된 변수들의 타입을 가정한다. 여기선 `x` 뿐 이므로 `x :: t0`를 가정한다.
- 편의상 `x`에 첨자를 붙여 `(x x)`를 `(x₀ x₁)`로 구분하자.
- `x₀`에 `x₁ :: t0`가 인자로 적용된 결과의 타입이 `t`이므로 `x₀`는 `t0`에서 `t`로 가는 함수이다. 즉 `x₀ :: t0 -> t`
- `x₀ :: t0` 이고 `x₀ :: t0 -> t`이므로 두 타입이 같아야 한다. 즉 `t0 ~ t0 -> t`
- constraint `t0 ~ t0 -> t`를 풀고자 `t0`를 반복적으로 치환하면 `(((...) -> t) -> t) -> t`의 무한한 타입을 얻게된다.
- 타입은 유한해야 하므로 말이 되지 않는다.

즉 무한 타입을 허용하지 않는 한, Y combinator에게 타입을 부여하는 것은 불가능 하다.
사실 untyped λ-calculus에 타입을 도입해 허용되는 항을 제한한 simply typed λ-calculus 에서는
\(Y\)가 정의되지 않을 뿐만 아니라, strong normalization property가 성립한다.
타입 검사를 통과하는 모든 항이 평가전략과 무관하게 계산이 종료한다는 뜻 이다.
타입을 붙인다는 것은 그 정도로 강한 조건이다.

정리하자면 하스켈에서 재귀적 정의를 사용해 fixpoint combinator를 만드는 것이 가능하지만, Y combinator는 타입이 맞지 않아 정의할 수 없다.
```haskell
fix f = f (fix f)                         -- ok
y = \f -> (\x -> f (x x)) (\x -> f (x x)) -- type error
```
그리고 타입에러가 발생한 이유는 `t0 ~ t0 -> t`를 만족하는 타입 `t0`가 존재하지 않기 때문이다.

그렇다면 이를 강제로 만족하게 만든다면 어떨까?
타입이 정말로 같은것은 아니지만, 다음과 같은 재귀적 타입을 사용하면 두 타입을 isomorphic하게 만드는 것은 가능하다.
```haskell
newtype T a = FoldT {unfoldT :: (T a -> a)}
```
```haskell
FoldT :: (T a -> a) -> T a
unfoldT :: T a -> (T a -> a)
-- FoldT . unfoldT = id
-- unfoldT . FoldT = id
```
`FoldT`와 `unfoldT`는 각각 constructor/destructor 이므로 서로 합성하면 identity function이 된다.
즉 `T a` 와 `T a -> a`는 isomorphic 하다. `a`가 `t`, `T a`가 `t0`의 역할을 한다고 생각하면 된다.

 이를 이용해서 Y combinator를 만들어 보자.
```haskell
y :: (a -> a) -> a
y = \f -> (\x -> f (unfoldT x x)) (FoldT (\x -> f (unfoldT x x)))
```
이렇게 정의한 `y`를 전개 해보면 마찬가지로 fixpoint combinator의 정의를 만족함을 알 수 있다.
```haskell
y f = (\x -> f (unfoldT x x)) (FoldT (\x -> f (unfoldT x x)))
    = f (unfoldT (FoldT (\x -> f (unfoldT x x))) (FoldT (\x -> f (unfoldT x x))))
    = f ((\x -> f (unfoldT x x)) (FoldT (\x -> f (unfoldT x x))))
    = f (y f)
```

## Curry's paradox
Y combinator가 정의되었다는 사실은 흥미롭지만, 한편 골아픈 문제이기도 하다.
단지 `T a = T a -> a` (여기서 등호는 두 타입이 isomorphic 함을 의미한다) 를 만족하는 데이터 타입을 정의하면 fixpoint combinator를 얻을 수 있다.
fixpoint combinator에서 임의의 명제를 증명할 수 있으므로 이는 곧 타입체계가 inconsistent해 짐을 의미한다.

Curry's paradox가 말하는 바가 바로 이것이다. `FoldT`와 `unfoldT`로 부터 임의의 명제를 증명해 보자.
`y id` 로도 가능하지만 아까 소개한 \(\Omega\)를 사용하는 편이 더 간단하다.
`omega`가 \(\omega\), `bad`가 \(\Omega\)에 해당한다.
```haskell
omega :: T a -> a
omega = \x -> foldT x x

bad :: a
bad = omega (FoldT omega)
```

하스켈이 자랑하는 재귀적 타입은 inconsistency를 만들어 낼 수 밖에 없는 것일까?
그렇지는 않다. 예를들어 다음 정의는 전혀 문제가 되지 않는다.
```haskell
newtype U a = FoldU {unfoldU :: Either a (U a, U a)}

FoldU :: Either a (U a, U a) -> U a
unfoldU :: U a -> Either a (U a, U a)
```
임의로 재귀적 타입 `U`를 정의해 보았지만, 여기서는 `T`와 달리 별다른 문제점이 발생하지 않는다.
어째서 `T`의 정의만이 inconsistency를 만들어 낼까?

설명을 간단히 하기 위해 타입 변수에 임의로 `A`라는 타입을 대입해 보자.
```haskell
newtype T = FoldT {unfoldT :: T -> A}
newtype U = FoldU {unfoldU :: Either A (U, U)}
```
이렇게 놓고 보니 두 정의의 차이점은 `T -> A`와 `Either A (U, U)`부분 밖에는 없어 보인다.
이 둘에게 이름을 한번 붙여보자.
```haskell
newtype FT a = FT (a -> A)
newtype FU a = FU (Either A (a, a))
newtype T = FoldT {unfoldT :: FT T}
newtype U = FoldU {unfoldU :: FU U}
```

`FT`와 `FU`의 covariant/contravariant functor 인스턴스를 구현해보면 차이점이 명백해진다. 둘은 완전히 반대의 성질을 띤다.
```haskell
instance Contravariant FT where
  contramap :: (a -> b) -> (FT b -> FT a)
  contramap f (FT g) = FT (g . f)

instance Functor FU where
  fmap :: (a -> b) -> (FU a -> FU b)
  fmap f (FU (Left x)) = FU (Left x)
  fmap f (FU (Right (x, y))) = FU (Right (f x, f y))
```
이해를 돕기 위해 타입시그니쳐를 추가하였으나 이는 하스켈 표준 문법은 아니다.
GHC에서 코드를 실행해보고 싶다면 타입시그니쳐를 제거하거나 `InstanceSigs` 언어 확장을 켜야 한다.

일반적으로 contravariant functor를 사용해 재귀적인 타입을 정의하면 Curry's paradox가 발생할 수 있다.

## Strict positivity in proof assistant
Haskell을 사용하면서 Curry's paradox를 문제삼는 경우는 거의 없으나,
타입 시스템의 consistency가 무엇보다 중요한 증명보조기에서는 보다 현실적인 문제이다.
Coq나 Agda 등의 언어를 어느정도 해본 이라면 다음과 같은 문제를 접한적이 분명 있을 것이다.
```agda
data T : Set where
  t : (T → T) → T
```
