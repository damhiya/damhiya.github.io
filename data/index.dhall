let Format
    : Type
    = < MarkDown | HTML >

let Post
    : Type
    = { ident : Text, content : Text, format : Format }

let Model = List Post

let model
    : Model
    = [ { ident = "Functor-and-Variance"
        , content = ./posts/Functor-and-Variance.md as Text
        , format = Format.MarkDown
        }
      , { ident = "Curry's-paradox"
        , content = ./posts/Curry's-paradox.md as Text
        , format = Format.MarkDown
        }
      , { ident = "quantification"
        , content = ./posts/Quantification.md as Text
        , format = Format.MarkDown
        }
      ]

in  model
