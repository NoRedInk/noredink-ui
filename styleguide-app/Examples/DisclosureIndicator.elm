module Examples.DisclosureIndicator exposing (Msg, State, example, init, update)

{-|

@docs Msg, State, example, init, update

-}

import Css
import Html.Styled as Html
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import ModuleExample as ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.DisclosureIndicator.V2 as DisclosureIndicator
import Nri.Ui.Fonts.V1 as Fonts


{-| -}
type alias State =
    { mediumState : Bool
    , smallState : Bool
    }


{-| -}
example : (Msg -> msg) -> State -> ModuleExample msg
example parentMessage state =
    { name = "Nri.Ui.DisclosureIndicator.V2"
    , category = Widgets
    , content =
        [ viewExample ToggleMedium
            state.mediumState
            (Css.px 20)
            (DisclosureIndicator.view
                { isOpen = state.mediumState
                , size = Css.px 15
                , styles = [ Css.marginRight (Css.px 10) ]
                }
            )
            """
            DisclosureIndicator.view
                { isOpen = state.mediumState
                , size = Css.px 15
                , styles = [ Css.marginRight (Css.px 10) ]
                }
            """
        , viewExample ToggleSmall
            state.smallState
            (Css.px 16)
            (DisclosureIndicator.view
                { isOpen = state.smallState
                , size = Css.px 9
                , styles = [ Css.paddingRight (Css.px 8) ]
                }
            )
            """
            DisclosureIndicator.view
                { isOpen = state.smallState
                , size = Css.px 9
                , styles = [ Css.paddingRight (Css.px 8) ]
                }
            """
        ]
            |> List.map (Html.map parentMessage)
    }


viewExample : msg -> Bool -> Css.Px -> Html.Html msg -> String -> Html.Html msg
viewExample toggle isOpen fontSize disclosureView disclosureCode =
    Html.div []
        [ Html.div []
            [ Html.button
                [ css
                    [ Css.displayFlex
                    , Css.alignItems Css.center
                    , Css.borderStyle Css.none
                    , Css.outline Css.none
                    , Fonts.baseFont
                    , Css.fontSize fontSize
                    ]
                , onClick toggle
                ]
                [ disclosureView
                , Html.text "Open for code example"
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
    { mediumState = False
    , smallState = False
    }


{-| -}
type Msg
    = ToggleMedium
    | ToggleSmall


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        ToggleMedium ->
            ( { state | mediumState = not state.mediumState }, Cmd.none )

        ToggleSmall ->
            ( { state | smallState = not state.smallState }, Cmd.none )
