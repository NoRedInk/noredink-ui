module Examples.DisclosureIndicator exposing (Msg, State, example, init, update)

{-|

@docs Msg, State, example, init, update

-}

import Css
import Html.Styled as Html
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import ModuleExample as ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.DisclosureIndicator.V2 as DisclosureIndicator
import Nri.Ui.Fonts.V1 as Fonts


{-| -}
type alias State =
    { largeState : Bool
    , mediumState : Bool
    }


{-| -}
example : (Msg -> msg) -> State -> ModuleExample msg
example parentMessage state =
    { name = "Nri.Ui.DisclosureIndicator.V2"
    , category = Widgets
    , content =
        [ viewExample ToggleLarge
            state.largeState
            (Css.px 17)
            (DisclosureIndicator.large [ Css.marginRight (Css.px 10) ] state.largeState)
            ("DisclosureIndicator.large [ Css.marginRight (Css.px 10) ] True"
                ++ "\nI'm a 17px caret icon."
            )
        , viewExample ToggleMedium
            state.mediumState
            (Css.px 15)
            (DisclosureIndicator.medium [ Css.paddingRight (Css.px 8) ] state.mediumState)
            ("DisclosureIndicator.medium [ Css.paddingRight (Css.px 8) ] True"
                ++ "\nI'm a 15px caret icon."
            )
        ]
            |> List.map (Html.map parentMessage)
    }


viewExample : msg -> Bool -> Css.Px -> Html.Html msg -> String -> Html.Html msg
viewExample toggle isOpen fontSize disclosureView disclosureCode =
    Html.div [ css [ Css.margin2 (Css.px 16) Css.zero ] ]
        [ Html.div []
            [ Html.button
                [ css
                    [ Css.displayFlex
                    , Css.alignItems Css.center
                    , Css.borderStyle Css.none
                    , Css.outline Css.none
                    , Fonts.baseFont
                    , Css.fontSize fontSize
                    , Css.backgroundColor Colors.white
                    ]
                , onClick toggle
                ]
                [ disclosureView
                , Html.text "Toggle me!"
                ]
            ]
        , if isOpen then
            code disclosureCode

          else
            Html.text ""
        ]


code : String -> Html.Html msg
code copy =
    Html.code
        [ css
            [ Css.fontSize (Css.px 12)
            , Css.whiteSpace Css.pre
            ]
        ]
        [ Html.text copy ]


{-| -}
init : State
init =
    { largeState = False
    , mediumState = False
    }


{-| -}
type Msg
    = ToggleLarge
    | ToggleMedium


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        ToggleLarge ->
            ( { state | largeState = not state.largeState }, Cmd.none )

        ToggleMedium ->
            ( { state | mediumState = not state.mediumState }, Cmd.none )
