module Examples.Sprite exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Category exposing (Category(..))
import Example exposing (Example)
import Examples.IconExamples as IconExamples
import Nri.Ui.Sprite.V1 as Sprite exposing (SpriteId)
import Nri.Ui.Svg.V1 as Svg exposing (Svg)
import Svg.Styled exposing (svg)
import Svg.Styled.Attributes as Attributes


{-| -}
type alias State =
    IconExamples.Settings


{-| -}
type alias Msg =
    IconExamples.Msg


{-| -}
example : Example State Msg
example =
    { name = "Sprite"
    , version = 1
    , categories = List.singleton Icons
    , keyboardSupport = []
    , state = IconExamples.init
    , update = IconExamples.update
    , subscriptions = \_ -> Sub.none
    , preview = IconExamples.preview (List.map Tuple.second sprites)
    , view =
        \ellieLinkConfig settings ->
            [ IconExamples.viewSettings settings
            , IconExamples.view settings "Rich Text Formatting" sprites
            ]
    }


sprites : List ( String, Svg )
sprites =
    [ ( "bold", viewSprite Sprite.bold )
    , ( "italic", viewSprite Sprite.italic )
    , ( "underline", viewSprite Sprite.underline )
    , ( "list", viewSprite Sprite.list )
    , ( "link", viewSprite Sprite.link )
    , ( "undo", viewSprite Sprite.undo )
    , ( "redo", viewSprite Sprite.redo )
    ]


viewSprite : SpriteId -> Svg
viewSprite id =
    Svg.init "" [ Sprite.use id ]
