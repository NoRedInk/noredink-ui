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
type Msg
    = ToggleMedium Bool
    | ToggleSmall Bool


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
        [ Html.div []
            [ Html.button
                [ css
                    [ Css.borderStyle Css.none
                    , Css.outline Css.none
                    , Css.fontSize (Css.px 20)
                    ]
                , onClick (ToggleMedium (not state.mediumState))
                ]
                [ DisclosureIndicator.view
                    { isOpen = state.mediumState
                    , size = Css.px 15
                    , styles = [ Css.marginRight (Css.px 10) ]
                    }
                , Html.text "Item with detail"
                ]
            ]
        , if state.mediumState then
            code """
                    DisclosureIndicator.view
                        { isOpen = state.mediumState
                        , size = Css.px 15
                        , styles = [ Css.marginRight (Css.px 10) ]
                        }
                    """

          else
            Html.text ""
        , Html.div []
            [ Html.button
                [ css
                    [ Css.displayFlex
                    , Css.alignItems Css.center
                    , Css.borderStyle Css.none
                    , Css.outline Css.none
                    , Fonts.baseFont
                    , Css.fontSize (Css.px 16)
                    ]
                , onClick (ToggleSmall (not state.smallState))
                ]
                [ DisclosureIndicator.view
                    { isOpen = state.smallState
                    , size = Css.px 9
                    , styles = [ Css.paddingRight (Css.px 8) ]
                    }
                , Html.text "Item with detail"
                ]
            ]
        , if state.smallState then
            code """
                    DisclosureIndicator.view
                        { isOpen = state.smallState
                        , size = Css.px 9
                        , styles = [ Css.paddingRight (Css.px 8) ]
                        }
                    """

          else
            Html.text ""
        ]
            |> List.map (Html.map parentMessage)
    }


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
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        ToggleMedium mediumState ->
            ( { state | mediumState = mediumState }, Cmd.none )

        ToggleSmall mediumState ->
            ( { state | smallState = mediumState }, Cmd.none )
