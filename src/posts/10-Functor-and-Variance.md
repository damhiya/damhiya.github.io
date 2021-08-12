# Functor and Variance

펑터(Functor)는 하스켈을 비롯한 강타입 함수형 패러다임에서 일종의 디자인 패턴으로서 흔하게 사용되는 개념이다.
함수형 프로그래밍에 대해 어느정도 지식이 있는 사람이라면 모나드를 떠올릴지도 모르겠다.
모나드 역시 펑터로부터 파생된 개념으로, 그 난해함으로 악명이 높다.
하지만 모나드의 악명 탓에 펑터를 배우기를 주저할 이유는 없다.

- 펑터는 모나드보다 **훨씬** 이해하기 쉽다.
- 프로그래밍에 모나드를 활용해본 사람은 제법 드물지만, 당신이 프로그래머라면 펑터를 이미 써 보았을 확률이 매우 높다.
  펑터를 배우는 것은 신기술 도입이라기 보다는 평소에 사용하던 대상의 구조를 자각하고 이해하는 것에 가깝다.
- 펑터는 FP 디자인 패턴일 뿐만 아니라, 타입 시스템의 이론적 배경이기도 하다. 증명언어의 (co)inductive 타입이나 OOP의 공변성(Variance) 또한 펑터를 통해 보다 깊은 이해가 가능하다.

이 글에서는 펑터와 공변성을 여러 관점에서 설명한다.
Currying등 FP의 기초적인 지식에 대해서는 별도로 설명을 하지 않는다.
추후 필요성이 느껴질 경우 다른 글에서 다뤄 보도록 하겠다.

## Functor in Haskell
펑터를 한마디로 요약하자면 "`map`이 가능한 대상"이라고 표현할 수 있다.
여기서 `map`은 흔히 리스트 등의 컬렉션 타입에서 사용하는 함수 `map`을 의미한다.
설마 Python이나 JavaScript의 `map`도 써보지 않았다고는 말하지 않겠지.

먼저 리스트에서 `map`의 동작을 한번 살펴보자.
```haskell
λ> map (\x -> x * 2) [1,2,3,4]
[2,4,6,8]

λ> map (\n -> take n "Hello") [1..5]
["H","He","Hel","Hell","Hello"]
```
아마 `map`을 처음 접하는 사람이라고 하여도 위 예시를 통해 `map`의 동작을 충분히 예측할 수 있을것이다.
구체적으로 얘기하자면 `map`은 다음 등식을 만족한다.
```haskell
map f [x₀, x₁, .., xₙ] = [f x₀, f x₁, .., f xₙ]
```
이보다 쉬울 수 없다! `map`은 그저

- 인자로 받은 함수 `f`를 두번째 인자인 리스트의 모든 원소에 **일괄적으로** 적용한다.
- 그리고 **다른 일은 하지 않는다**.

`map`의 특징을 설명하는데에 타입이 필수적인 것은 아니나,
리스트의 `map`을 펑터로 확장시키기 위해선 타입이 필요하다.
```haskell
map :: (a -> b) -> List a -> List b
map f []     = []
map f (x:xs) = f x : map f xs
```
(이 글의 주제는 하스켈이 아닌 펑터 이므로, 혼동을 줄이기 위해 하스켈에서의 표기를 살짝 바꾸도록 하겠다.
본래 하스켈에서는 `List a`대신 `[a]`와 같은 표기법을 사용한다.)

`map`의 구현은 앞서 본 등식을 재귀적 표현으로 바꾼것에 불과하다.

보다 중요한 것은 타입이다.
`map`은 다형적이다.
`map`이 인자로 받은 함수 `f`의 입력과 출력 타입인 `a`, `b`는 각각 무엇이든 될 수 있다.
당연히 `f`가 적용될 대상인 리스트 또한 다형적이어야 한다.
타입생성자(type constructor)인 `List`는 `Type -> Type`이라는 카인드(kind)를 가지며, 타입 `a :: Type`가 적용되면 `List a :: Type`으로 구체화 된다.
`a -> b`타입의 함수가 `List a` 타입을 가지는 리스트의 각각의 원소에 적용되면 `List b`타입을 가지는 결과가 나온다.

이 리스트에서의 `map`을 다른 타입에 대해서도 사용될 수 있도록 일반화한 개념이 하스켈의 펑터이다.
```haskell
class Functor (f :: Type -> Type) where
  map :: (a -> b) -> f a -> f b
```
(마찬가지로, 하스켈에서는 `Functor`의 `map`을 본래 `fmap`이라 부른다.)

하스켈의 `class`키워드는 타입클래스를 선언하는 것으로, 임의의 타입에 대한 인터페이스를 정의하는 기능이다.
`Functor`는 타입 `f :: Type -> Type`에 대한 인터페이스로, `f`가 기존 `List`의 역할을 대체한다.

이제 우리는 적절한 `f`를 준비해서 이에 대한 펑터를 구현할 수 있다.
당장 몇가지 예시를 살펴보자.
```haskell
data List a = [] | (:) a (List a)
data Maybe a = Nothing | Just a
data BinTree a = Tip a | Branch a (BinTree a) (BinTree a)

instance Functor List where
  map f []     = []
  map f (x:xs) = f x : map f xs

instance Functor Maybe where
  map f Nothing  = Nothing
  map f (Just x) = Just (f x)

instance Functor BinTree where
  map f (Tip x) = Tip (f x)
  map f (Branch x l r) = Branch (f x) (map f l) (map f r)
```
리스트의 `map`은 아까 보았지만, 이것이 펑터의 인터페이스에 들어맞음을 보이기 위해 한번 더 언급하였다.

다시 말하지만 펑터는 퍽 단순하기에, 구현이 어떻게 되야하는지 예측하기 그리 어렵지 않다.
그저 `a`가 등장하는 모든 곳에 인자로 받은 함수를 적용해 `b`로 치환하는 것이 전부이다.

때문에 `map`은 인자로 받은 값의 구조를 바꾸지 않는다.
인자로 받은것이 `Nothing`이었다면 결과도 `Nothing`이며, `Just`를 받았다면 결과도 `Just`이다.
왼쪽으로 치우친 이진트리를 입력으로 받았다면 결과도 왼쪽으로 치우친 이진트리일 것이다.

그런데 다음과 같은 엉터리 구현들을 생각해볼 수 있다.
```haskell
instance Functor List where
  map f []     = []
  map f (x:xs) = f x : f x : xs

instance Functor Maybe where
  map f Nothing  = Nothing
  map f (Just x) = Nothing

instance Functor BinTree where
  map f (Tip x)        = Tip (f x)
  map f (Branch x l r) = Branch (f x) (map f r) (map f l)
```
이것들은 분명 무엇인가 자연스럽지 않다.
왼쪽으로 치우친 이진트리를 입력으로 받았는데 결과가 오른쪽으로 치우친 이진트리인 일이 **발생해서는 안된다**.
적어도 우리가 `map`이라는 개념에 대해 가지는 직관에 부합하지 않는다.
하지만 무엇을 자연스럽지 않다고, 무엇을 자연스럽다고 말할 수 있을까?

다행히도 수학자들이 이 기준을 정의내린 바가 있다.
다음의 두 등식은 펑터 규칙이라 불린다.
```haskell
map id = id
map (g ∘ f) = map g ∘ map f
```
각각 "항등함수의 보존", "함수 합성의 보존" 이라고 얘기할 수 있다.

<!--
## Contravariant functor
## Variance
## Variance in OOP
## Functor in CT
-->
