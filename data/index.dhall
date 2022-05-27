let Format
    : Type
    = < MarkDown | HTML >

let Post
    : Type
    = { identifier : Text, title : Text, content : Text, format : Format }

let Model
    : Type
    = { mainCss : Text, syntaxCss : Text, posts : List Post, haskellTutorial : List Post }

let model
    : Model
    = { mainCss = ./main.css as Text
      , syntaxCss = ./syntax.css as Text
      , posts =
        [ { identifier = "Functor-and-Variance"
          , title = "Functor and variance"
          , content = ./posts/Functor-and-Variance.md as Text
          , format = Format.MarkDown
          }
        , { identifier = "Curry's-paradox"
          , title = "Curry's paradox"
          , content = ./posts/Curry's-paradox.md as Text
          , format = Format.MarkDown
          }
        , { identifier = "quantification"
          , title = "Quantification"
          , content = ./posts/Quantification.md as Text
          , format = Format.MarkDown
          }
        ]
      , haskellTutorial =
        [ { identifier = "Installation"
          , title = "Installation"
          , content = ./haskellTutorial/Installation.md as Text
          , format = Format.MarkDown
          }
        ]
      }

in  model
