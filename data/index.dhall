let Format
    : Type
    = < MarkDown | HTML >

let Post
    : Type
    = { title : Text, content : Text, format : Format }

let Model = List Post

let model
    : Model
    = [ { title = "Functor and Variance"
        , content = ./posts/Functor-and-Variance.md as Text
        , format = Format.MarkDown
        }
      , { title = "Curry's paradox"
        , content = ./posts/Curry's-paradox.md as Text
        , format = Format.MarkDown
        }
      , { title = "Quantification"
        , content = ./posts/Quantification.md as Text
        , format = Format.MarkDown
        }
      ]

in  model
