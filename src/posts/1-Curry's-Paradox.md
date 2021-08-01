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

그런데 \(Y\)를 사용하면 종료하지 않는 계산을 만들 수 있다.
\[
  I \equiv \lambda x . x
\]
\[
  \begin{array}{rl}
                   & YI    \\
    \longrightarrow& I(YI) \\
    \longrightarrow& YI
  \end{array}
\]

\(YI\) 는 β-reduction을 아무리 수행하여도 β normal form에 도달할 수 없다.
이는 untyped λ-calculus에서 weak normalization property가 성립하지 않음을 의미한다.
사실 더 단순한 예시로 \(\Omega\)가 있다.
\[
  \begin{array}{rrl}
    \Omega &          \equiv & (\lambda x . x x) (\lambda x . x x) \\
           & \longrightarrow & (\lambda y . y y) (\lambda x . x x) \\
           & \longrightarrow & (y y) [y := \lambda x . x x]        \\
           & \longrightarrow & (\lambda x . x x) (\lambda x . x x)
  \end{array}
\]

fixpoint combinator에게 타입을 붙여보면 애초에 논리적으로 말이 되지 않음을 알 수 있다.
하스켈에서 `fix`는 다음과 같은 타입을 갖는다.
```haskell
fix :: ∀ α. (α → α) → α
```
Propositions as types의 관점에서 볼 때, 이는 "\(P\) 일 때 \(P\) 라면, \(P\) 이다"를 의미한다.
Tautology에서 임의의 명제를 증명해 낼 수 있는 것이다.
따라서 이런 fixpoint를 가지는 논리체계는 inconsistent 하다.
