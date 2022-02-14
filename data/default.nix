let
  paths = [
    ./posts/Functor-and-Variance.md
    (./posts + "Curry's-paradox.md")
    ./posts/Quantification.md
  ];
in {
  posts = map builtins.readFile paths;
}
