module Examples.Sprite exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Accessibility.Styled.Aria as Aria
import Category exposing (Category(..))
import Example exposing (Example)
import Examples.IconExamples as IconExamples
import KeyboardSupport exposing (Direction(..), Key(..))
import Nri.Ui.Sprite.V1 as Sprite exposing (SpriteId)
import Nri.Ui.Svg.V1 as Svg exposing (Svg)
import Svg.Styled exposing (svg)
import Svg.Styled.Attributes as Attributes


{-| -}
type alias State =
    ()


{-| -}
type alias Msg =
    ()


{-| -}
example : Example State Msg
example =
    { name = "Sprite"
    , version = 1
    , categories = List.singleton Icons
    , keyboardSupport = []
    , state = ()
    , update = \_ state -> ( state, Cmd.none )
    , subscriptions = \_ -> Sub.none
    , preview = IconExamples.preview (List.map Tuple.second sprites)
    , view = \_ -> [ IconExamples.view "Rich Text Formatting" sprites ]
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
    svg
        [ Attributes.width "100%"
        , Attributes.height "100%"
        ]
        [ Sprite.use id ]
        |> Svg.fromHtml
