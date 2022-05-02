let Format
    : Type
    = < MarkDown | HTML >

let Post
    : Type
    = { identifier : Text, content : Text, format : Format }

let Model = List Post

let model
    : Model
    = [ { identifier = "Functor-and-Variance"
        , content = ./posts/Functor-and-Variance.md as Text
        , format = Format.MarkDown
        }
      , { identifier = "Curry's-paradox"
        , content = ./posts/Curry's-paradox.md as Text
        , format = Format.MarkDown
        }
      , { identifier = "quantification"
        , content = ./posts/Quantification.md as Text
        , format = Format.MarkDown
        }
      ]

in  model
