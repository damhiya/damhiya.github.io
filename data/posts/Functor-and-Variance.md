# Functor and Variance

펑터(Functor)는 하스켈을 비롯한 강타입 함수형 패러다임에서 일종의 디자인 패턴으로서 흔하게 사용되는 개념이다.
함수형 프로그래밍에 대해 어느정도 지식이 있는 사람이라면 모나드를 떠올릴지도 모르겠다.
모나드 역시 펑터로부터 파생된 개념으로, 그 난해함으로 악명이 높다.
하지만 모나드의 악명 탓에 펑터를 배우기를 주저할 이유는 없다.

- 펑터는 모나드보다 **훨씬** 이해하기 쉽다.
- 프로그래밍에 모나드를 활용해본 사람은 제법 드물지만, 당신이 프로그래머라면 펑터를 이미 써 보았을 확률이 매우 높다.
  펑터를 배우는 것은 신기술 도입이라기 보다는 평소에 사용하던 대상의 구조를 자각하고 이해하는 것에 가깝다.
- 펑터는 FP 디자인 패턴일 뿐만 아니라, 타입 시스템의 이론적 배경이기도 하다. 증명보조기의 (co)inductive 타입이나 OOP의 공변성(Variance) 또한 펑터를 통해 보다 깊은 이해가 가능하다.

이 글에서는 펑터와 공변성을 여러 관점에서 설명한다.

또한 다음의 주제들은 이 글에서 설명하지 않는다.

- 하스켈 문법
- 순수함수
- 재귀함수
- 커링
- 고차함수
- 다형함수
- HKT
- 타입클래스

혹시 본 글이 잘 이해가 되지 않는다면, 하스켈 기초에 대해서 설명하는 다른 글을 먼저 읽는 것이 도움이 될 것이다.
추후 필요성이 느껴질 경우 직접 다른 글에서 다뤄 보도록 하겠다.

한편 하스켈 예시 코드가 Prelude의 정의와 충돌할 수 있다.
직접 컴파일 및 실행을 해보고 싶다면 정의의 이름들을 바꾸도록 하자.

## Functor in Haskell

### map of List
펑터의 가장 핵심이 되는 것은 바로 `map`이라고 불리는 함수이다.
다행히도 이 `map`함수는 리스트 등의 컬렉션 타입에서 자주 사용되며, 프로그래머들에게도 상당히 익숙한 편일 것이다.
우선 이 리스트에서의 `map`에 대한 이야기로 설명을 시작하겠다.

먼저 리스트에서 `map`의 동작을 한번 살펴보자.
```haskell
λ> map (\x -> x * 2) [1,2,3,4]
[2,4,6,8]

λ> map (\n -> take n "Hello") [1..5]
["H","He","Hel","Hell","Hello"]
```
아마 `map`을 처음 접하는 사람이라고 하여도 위 예시를 통해 `map`이 무엇을 하는지에 대해 어렴풋히 짐작가는 바가 있을 것이다.
이를 형식적으로 기술하자면, `map`은 다음 등식을 만족한다고 할 수 있다.
```haskell
map f [x₁, x₂, .., xₙ] = [f x₁, f x₂, .., f xₙ]
```
그리 괴상한 동작은 아니다. `map`은 그저

- 인자로 받은 함수 `f`를 두번째 인자인 리스트의 모든 원소에 **일괄적으로** 적용한다.
- 그리고 **다른 일은 하지 않는다**.

`map`이 무슨 일을 하는지 파악했다면, 정의를 보도록 하자.
```haskell
type List = []

map :: (a -> b) -> List a -> List b
map f []     = []
map f (x:xs) = f x : map f xs
```
(혼동을 줄이기 위해 타입 동의어를 선언 하였다.
본래 하스켈에서는 `List a`대신 `[a]`와 같은 표기법을 사용한다.)

`map`의 구현은 앞서 본 등식을 재귀적 표현으로 바꾼것에 불과하다.

보다 중요한 것은 타입이다.
리스트 `map`의 타입은 펑터와 공유하는 특징이 많으므로 주의깊게 살펴보도록 하자.

`map`이 인자로 받는 함수의 타입은 `f :: a -> b`로, 입력과 출력 타입이 각각 무엇이든 될 수 있다.
이 `f`를 어떤 리스트의 원소에 적용한다면 자연히 입력 리스트의 원소 타입은 `a` 이고, 출력 리스트의 원소 타입은 `b`가 될 것이다.
즉 입력 리스트의 타입은 `List a`, 출력 리스트의 타입은 `List b`가 된다.
여기서 `List`는 `Type -> Type`의 종(kind)를 가지는 HKT이다.

HKT를 잘 모르는 이에게 조금의 부연 설명을 하자면,
일반적으로 '타입'이라고 말하는 대상들, 대표적으로 `Bool`, `Int`, `Char` 등은 원소를 가진다.
타입의 원소란 `True :: Bool`과 같이 해당 타입으로 분류되는 값을 말한다.
반면에 `List`같은 것을 흔히 타입이라고 부르긴 하지만 `List`는 그 자체로는 진정한 타입이 아니며, 다른 타입이 적용 되었을 때 비로소 타입이 된다.
따라서 `List`는 원소를 가질 수 없지만, `List Bool`, `List Int`, `List (List Int)`와 같은 것은 타입이므로 `[1,2,3] :: List Int` 처럼 원소를 가질 수 있다.
(마지막 사례는 정수 리스트의 리스트를 의미한다. `[[1,2],[3],[4,5]] :: List (List Int)`)

이런 `List`와 같은 (넓은 의미의) 타입을 분류하기 위해서 종(kind)의 개념이 필요하다.
종이란 한 마디로 '타입의 타입'이다.
예를 들어 진짜 타입인 `Bool`, `Int`, `Char`는 `Type` 이라는 종을 가지며, `List`는 `Type -> Type`이라는 종을 가진다.
종은 타입 시그니쳐와 같은 표기법으로 `Bool :: Type`, `List :: Type -> Type`과 같이 쓴다.
`List`와 같이 `Type`이 아닌 다른 종을 가지는 것들을 HKT(Higher Kinded Type)이라고 부른다.

`List`의 원소가 **무엇이든 될 수 있다**는 점은 상당히 주목할만 하다.
자료구조 중에는 그것이 올바르게 작동하기 위해 원소가 될 수 있는 타입을 제한하는 것이 많다.
BST의 정의에는 원소간 동치·순서관계가 필요하다.
또 해시테이블이 정의되기 위해서는 원소들 간 동치 관계와 해시함수가 필요하다.

펑터는 이런 BST나 해시테이블과 같은 자료구조에 대해서는 정의될 수 없으며,
리스트처럼 원소의 타입에 전혀 제약을 두지 않는 경우에만 정의될 수 있다.
(사실 BST에 대한 펑터를 정의하는 것도 가능하긴 하다.
다만 이 경우 하스켈에서 일반적으로 사용하는 펑터의 정의에는 들어맞지 않으며,
매핑하는 함수 `f :: a -> b`를 monotonic function으로 제한할 필요가 있다.)

### Definition
펑터를 한마디로 요약하자면 "`map`이 가능한 대상"이라고 표현할 수 있다.
즉 앞서 본 `List :: Type -> Type`에 대해 정의된 `map`을 임의의 `f :: Type -> Type`에 대한 것으로 확장한 것이다.
(여기서 `f :: Type -> Type`은 앞서 보았던 `map`이 인자로 받는 함수 `f :: a -> b`와는 별개이다.
앞으로 혼동의 여지가 있다면 타입 시그니쳐를 통해 구분하겠다.)

하스켈에서는 펑터를 타입클래스를 사용해 정의한다.
```haskell
class Functor (f :: Type -> Type) where
  fmap :: (a -> b) -> f a -> f b
```

이 클래스 선언은 "어떤 `f :: Type -> Type`에 대해 where절 아래의 함수들이 정의된다면 `f`는 `Functor`이다." 라는 의미이다.
where절 아래에 선언된 함수는 `fmap`뿐 이므로, `Functor`란 곧 `fmap`이 잘 정의된 어떤 `f`라고 말할 수 있다.

여기서 `fmap`이 바로 리스트 `map`의 새로운 이름이다.
실제로 `fmap`의 타입 시그니쳐는 앞서 본 `map`에서 `List`가 `f`로 바뀐것 외에는 다른 점이 없다.

설명을 진행하기 전에, 한가지 짚고 넘어갈 부분이 있다.
`fmap`의 타입 `(a -> b) -> f a -> f b`는 두가지 방식으로 이해할 수 있는데,

- 함수와 매핑 대상을 인자로 받는 2인자 함수. 즉, `(a -> b, f a) -> f b`를 커링한 것.
- 평범한 함수 `a -> b`를 `f :: Type -> Type`에 대해 작용하는 함수 `f a -> f b`로 변환해주는 함수.
  `(a -> b) -> (f a -> f b)`와 같이 괄호를 사용하면 조금 더 명백해진다.

둘 모두 합당한 해석이지만, 후자의 방식으로 이해하는 편이 좋다.
이는 하스켈의 펑터 정의가 카테고리 이론에서 사용하는 보다 일반적인 정의의 특수한 경우이기 때문인데,
더 일반적으로 정의한 펑터를 다룰 때에는 후자의 방법을 사용하여야 한다.

`Functor` 클래스가 정의되었으므로 이제 우리는 적절한 `f`를 준비해서 이에 대한 펑터를 구현할 수 있다.
기초적인 예시를 몇가지 살펴보자.

```haskell
type List = []

instance Functor List where
  fmap f []     = []
  fmap f (x:xs) = f x : map f xs
```
먼저 `List`의 펑터 인스턴스이다.
리스트의 `map`은 펑터의 정의에 들어맞기 때문에 그대로 펑터 인스턴스 정의에 사용할 수 있다.

```haskell
data Maybe a = Nothing | Just a

instance Functor Maybe where
  fmap f Nothing  = Nothing
  fmap f (Just x) = Just (f x)
```
리스트보다 더욱 단순하긴 하지만, `Maybe`역시 중요한 펑터의 예시이다.
`Maybe`에 대한 `fmap`은 그저 입력이 `Just` 케이스 일 때 내부에 들어 있는 값에 `f`를 적용할 뿐이다.

```haskell
data BinTree a = Tip a | Branch a (BinTree a) (BinTree a)

instance Functor BinTree where
  fmap f (Tip x) k      = Tip (f x)
  fmap f (Branch x l r) = Branch (f x) (fmap f l) (fmap f r)
```
이진트리 역시 펑터 인스턴스를 정의할 수 있다.

다시 말하지만 펑터는 퍽 단순하기에, 구현이 어떻게 되어야 하는지 예측하기 그리 어렵지 않다.
펑터 구현의 패턴은 다음과 같이 요약할 수 있다.

- 케이스가 여러개 있는 경우, 패턴매칭을 한다.
- `a`가 등장할 경우, `f :: a -> b`를 적용해 `b`로 바꾼다.
- 재귀적인 데이터의 경우, `fmap f`를 재귀적으로 적용한다. (`List`나 `BinTree`의 경우)

`fmap`의 구현을 잘 살펴 보면, `fmap`이 입력받은 데이터의 구조를 변형시키지 않음을 알 수 있다.
`Maybe`의 경우 인자로 받은것이 `Nothing`이었다면 결과도 `Nothing`이며, `Just`를 받았다면 결과도 `Just`이다.
`BinTree`에 대한 `fmap` 역시 입력이 `Tip`이라면 `Tip`, `Branch`라면 `Branch`를 결과로 주며,
부분 트리의 위치 역시 보존된다. (왼쪽으로 치우친 이진트리를 입력으로 받았다면 결과도 왼쪽으로 치우친 이진트리일 것이다.)

그런데 다음과 같은 엉터리 구현들을 생각해볼 수 있다.
```haskell
instance Functor List where
  fmap f []     = []
  fmap f (x:xs) = f x : f x : fmap f xs

instance Functor Maybe where
  fmap f Nothing  = Nothing
  fmap f (Just x) = Nothing

instance Functor BinTree where
  fmap f (Tip x)        = Tip (f x)
  fmap f (Branch x l r) = Branch (f x) (fmap f r) (fmap f l)
```
각각 다음과 같은 이상한 점이 있다.

- `f x`가 2개로 중복되어 있다
- 입력에 무관하게 결과로 `Nothing`을 준다.
- 왼쪽과 오른쪽 하위트리가 뒤바뀌어 있다.

이것들은 분명 무엇인가 자연스럽지 않다.
왼쪽으로 치우친 이진트리를 입력으로 받았는데 결과가 오른쪽으로 치우친 이진트리인 일이 **발생해서는 안된다**.
적어도 우리가 `fmap`이라는 개념에 대해 가지는 직관에 부합하지 않는다.
하지만 무엇을 자연스럽지 않다고, 무엇을 자연스럽다고 말할 수 있을까?

다행히도 수학자들이 이 기준을 정의내린 바가 있다.
다음의 두 등식은 펑터 규칙이라 불린다.
```haskell
fmap id = id
fmap (g ∘ f) = fmap g ∘ fmap f
```
첫번째는 "항등함수의 보존", 그리고 두번째는 "(함수) 합성의 보존"이다.

먼저 올바른 리스트의 펑터 구현에서 펑터규칙이 만족됨을 확인해 보자.
```haskell
fmap id [x₁, x₂, .., xₙ]
  = [id x₁, id x₂, .., id xₙ]
  = [x₁, x₂, .., xₙ]
  = id [x₁, x₂, .., xₙ]

(fmap g ∘ fmap f) [x₁, x₂, .., xₙ]
  = fmap g [f x₁, f x₂, .., f xₙ]
  = [g (f x₁), g (f x₂), .., g (f xₙ)]
  = [(g ∘ f) x₁, (g ∘ f) x₁, .., (g ∘ f) xₙ]
  = fmap (g ∘ f) [x₁, x₂, .., xₙ]
```
글의 초반에서 소개했던 `map`이 만족하는 등식을 통해 항등함수 보존을 간단히 확인할 수 있다.
(사실 별로 엄밀한 논증은 아니다.
제대로 확인하고자 한다면 리스트에 대한 귀납법을 사용해 증명해야 한다.)

반면 잘못된 리스트 구현에서는 간단하게 반례를 찾을 수 있다.
```haskell
fmap id [1] = [1,1] ≠ id [1]
```
`1`이 두개로 복사된다면 항등함수 보존을 만족할 수 없다.

하지만 정말 두 개의 조건이 다 필요한 것일까?
예시를 하나 생각해 보자.
```haskell
data T a = T Bool a
```
즉 `Bool`과 `a`의 튜플로 이루어진 타입이다.
이 타입에 대한 올바른 펑터 인스턴스는 다음과 같다.
```haskell
instance Functor T where
  fmap f (T b x) = T b (f x)
```
`Bool`의 내용물은 그대로 두고, `a`에만 `f`를 적용하면 된다.

이번에는 잘못된 구현을 보도록 하자.
```haskell
instance Functor T where
  fmap f (T True  x) = T False (f x)
  fmap f (T False x) = T False (f x)
```
이 `fmap`은 항등함수를 보존하지 않는다.
```haskell
fmap id (T True x) = T False x ≠ id (T True x) = T True x
```
그러나 신기하게도, 합성은 보존된다.
```haskell
fmap (g ∘ f) (T b x) = T False (g (f x)) = (fmap g ∘ fmap f) (T b x)
```

또 다른 잘못된 구현을 살펴보자.
```haskell
instance Functor T where
  fmap f (T True x) = if f == id
    then (T True x)
    else (T False (f x))
  fmap f (T False x) = T False (f x)
```
이번 구현은 `f = id` 일 때만 ad-hoc한 방식으로 올바른 구현이 되며,
그렇지 않을 때에는 먼저 본 잘못된 구현과 똑같이 동작 한다.

물론 이 코드는 사실 pseudo-Haskell 코드이다.
실제 하스켈에서는 `f == id` 같은 연산으로 어떤 함수가 `id`인지 판별할 수 없지만
흔히 보는 수학의 함수에서는 이런 함수가 그다지 이상한 것이 아니므로 이런 함수가 존재한다고 가정하고 생각해보자.

이 구현에서는 항등함수 보존이 만족 된다.
```haskell
fmap id (T b x) = T b x = id (T b x)
```

하지만 합성은 보존되지 않는다.
`g ∘ f = id`, `f ≠ id`를 만족하는 함수 `f`,`g`가 존재한다고 가정해보자.
대표적으로 `f = g = not :: Bool -> Bool`이 있다.
```haskell
fmap (g ∘ f) (T True x) = fmap id (T True x) = T True x
(fmap g ∘ fmap f) (T True x) = fmap g (T False (f x)) = T False (g (f x)) = T False x
```

이것으로 완전히 납득이 되었을지는 잘 모르겠지만,
항등함수의 보존과 합성의 보존은 각각 독립적으로
펑터의 정의가 우리의 직관을 반영하게 하는데에 기여하는 중요한 법칙이다.

정리하자면 펑터의 정의는 다음과 같다. (pseudo-Haskell 이다.)
```haskell
class Functor (f :: Type -> Type) where
  fmap :: (a -> b) -> f a -> f b

  identityLaw :: fmap id = id
  compositionLaw :: fmap (g ∘ f) = fmap g ∘ fmap f
```
실제 하스켈에서 항등함수 보존이나 합성 보존같은 법칙을 표현하거나 강제할 방법은 없으나,
적어도 프로그래머는 항상 이 법칙이 만족되도록 펑터 인스턴스를 구현해야 한다.
(만족하지 않는다면 버그이다.)

<!--
## Functor and Haskell's parametricity
## Contravariant functor
## Variance
## Variance in OOP
## Functor in CT
-->
