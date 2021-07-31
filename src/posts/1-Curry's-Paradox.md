<!--
Date : 2021.7.31
-->

λ-calculus, Turing machine 등의 주제를 논할 때에는 항상 프로그램의 정지 여부에 관한 문제가 따라 나온다.
정지하는 프로그램을 판별하는 일은 커리-하워드 대응과 결부할 때 더욱 중요한데,
종료하지 않는 프로그램을 허용하는 것은 곧 논리체계가 inconsistent 함을 의미하기 때문이다.
이 글 에서는 이런 non-termination을 유발할 수 있는 요소 중 하나인 Curry's paradox에 대해 설명한다.

## Untyped λ-calculus
untyped λ-calculus는 normalization property가 성립하지 않는 체계임이 잘 알려져 있다.
이는 β-reduction을 아무리 적용하여도 β-normal form을 만들어낼 수 없는 항이 존재한다는 뜻이다.
가장 단순한 예시로 \(\Omega\)가 있다.

\[
  \Omega = (\lambda x . x x) (\lambda x . x x)
\]

Y combinator
\[
  Y = \lambda f . (\lambda x . f (x x)) (\lambda x . f (x x))
\]
