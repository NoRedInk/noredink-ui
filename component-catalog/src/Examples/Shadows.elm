module Examples.Shadows exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Category exposing (Category(..))
import Css exposing (Style)
import Example exposing (Example)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Nri.Ui.Shadows.V1 as Shadows
import Nri.Ui.Text.V6 as Text
import ViewHelpers


type alias State =
    ()


{-| -}
type alias Msg =
    ()


{-| -}
example : Example State Msg
example =
    { name = "Shadows"
    , version = 1
    , categories = [ Atoms ]
    , keyboardSupport = []
    , init = ( (), Cmd.none )
    , update = \_ state -> ( state, Cmd.none )
    , subscriptions = \_ -> Sub.none
    , preview = List.map (\( _, style, _ ) -> viewPreviewShadow style) allShadows
    , about = []
    , view =
        \ellieLinkConfig _ ->
            [ List.map (\( name, style, usage ) -> ( name, viewShadow ( usage, style ) )) allShadows
                |> ViewHelpers.viewExamples
            ]
    }


allShadows : List ( String, Style, String )
allShadows =
    [ ( "Shadows.low"
      , Shadows.low
      , "Use for standard containers and similar elements like large messages."
      )
    , ( "Shadows.medium"
      , Shadows.medium
      , "Use for larger, more prominent containers like Container.Pillow and marketing site cards."
      )
    , ( "Shadows.high"
      , Shadows.high
      , "Use for “floating” elements like tooltips, popovers, and modals"
      )
    ]


viewPreviewShadow : Css.Style -> Html msg
viewPreviewShadow shadow =
    Html.div
        [ Attributes.css
            [ Css.padding2 (Css.px 8) Css.zero
            , Css.margin2 (Css.px 4) Css.zero
            , Css.borderRadius (Css.px 4)
            , Css.height (Css.px 14)
            , shadow
            ]
        ]
        []


viewShadow : ( String, Css.Style ) -> Html msg
viewShadow ( usageGuidance, shadow ) =
    Html.div
        [ Attributes.css
            [ Css.margin2 (Css.px 20) (Css.px 4)
            ]
        ]
        [ Text.smallBodyGray
            [ Text.css
                [ Css.textAlign Css.center
                , shadow
                ]
            , Text.plaintext usageGuidance
            ]
        ]
