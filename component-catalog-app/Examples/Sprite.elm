module Examples.Sprite exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Example exposing (Example)
import Examples.IconExamples as IconExamples exposing (Group)
import Nri.Ui.Sprite.V1 as Sprite exposing (SpriteId)
import Nri.Ui.Svg.V1 as Svg exposing (Svg)


{-| -}
type alias State =
    IconExamples.Settings


{-| -}
type alias Msg =
    IconExamples.Msg


{-| -}
example : Example State Msg
example =
    { moduleName = "Sprite"
    , version = 1
    , label = "Bold"
    , name = "bold"
    , icon = viewSprite Sprite.bold
    , renderSvgCode = \name -> "Svg.init \"0 0 100 100\" [ Sprite.use  Sprite." ++ name ++ " ] "
    , preview = IconExamples.preview (List.map (\( a, b, c ) -> b) sprites)
    , all = all
    }
        |> IconExamples.example


all : List Group
all =
    [ ( "Rich Text Formatting", sprites )
    ]


sprites : List ( String, Svg, List a )
sprites =
    [ ( "bold", viewSprite Sprite.bold, [] )
    , ( "italic", viewSprite Sprite.italic, [] )
    , ( "underline", viewSprite Sprite.underline, [] )
    , ( "list", viewSprite Sprite.list, [] )
    , ( "link", viewSprite Sprite.link, [] )
    , ( "undo", viewSprite Sprite.undo, [] )
    , ( "redo", viewSprite Sprite.redo, [] )
    ]


viewSprite : SpriteId -> Svg
viewSprite id =
    Svg.init "0 0 100 100" [ Sprite.use id ]
