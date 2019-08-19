module Examples.Fade exposing (Msg, State, example, init, update)

{-|

@docs Msg, State, example, init, update

-}

import Accessibility.Styled as Html
import Css
import Html.Styled.Attributes exposing (css)
import Html.Styled.Keyed as Keyed
import List.Zipper as Zipper exposing (Zipper(..))
import ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.Button.V8 as Button
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fade.V1 as Fade


{-| -}
type Msg
    = Previous
    | Next


{-| -}
type alias State =
    { panels : Zipper Panel
    , previous : Maybe Panel
    }


{-| -}
example : (Msg -> msg) -> State -> ModuleExample msg
example parentMessage state =
    { name = "Nri.Ui.Fade.V1"
    , category = Animations
    , content =
        [ Keyed.node "div"
            [ css
                [ Css.border3 (Css.px 3) Css.solid Colors.gray75
                , Css.padding (Css.px 20)
                , Css.width (Css.px 600)
                ]
            ]
            (case state.previous of
                Just previousPanel ->
                    [ viewPanel previousPanel Fade.fadeOut
                    , viewPanel (Zipper.current state.panels) Fade.fadeIn
                    ]

                Nothing ->
                    [ viewPanel (Zipper.current state.panels) Fade.fadeIn
                    ]
            )
        , Html.div
            [ css
                [ Css.displayFlex
                , Css.justifyContent Css.spaceBetween
                , Css.marginTop (Css.px 20)
                , Css.width (Css.px 300)
                ]
            ]
            [ navigateButton Previous "Previous"
            , navigateButton Next "Next"
            ]
        ]
            |> List.map (Html.map parentMessage)
    }


{-| -}
init : State
init =
    { panels = Zipper [] One [ Two, Three ]
    , previous = Nothing
    }


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        Previous ->
            ( { state
                | panels =
                    Zipper.previous state.panels
                        |> Maybe.withDefault (Zipper.last state.panels)
                , previous = Just (Zipper.current state.panels)
              }
            , Cmd.none
            )

        Next ->
            ( { state
                | panels =
                    Zipper.next state.panels
                        |> Maybe.withDefault (Zipper.first state.panels)
                , previous = Just (Zipper.current state.panels)
              }
            , Cmd.none
            )



-- INTERNAL


type Panel
    = One
    | Two
    | Three


viewPanel : Panel -> Css.Style -> ( String, Html.Html msg )
viewPanel panel animation =
    let
        ( color, text, key ) =
            case panel of
                One ->
                    ( Colors.red, "Panel One", "panel-1" )

                Two ->
                    ( Colors.yellow, "Panel Two", "panel-2" )

                Three ->
                    ( Colors.green, "Panel Three", "panel-3" )
    in
    ( key
    , Html.div
        [ css
            [ Css.border3 (Css.px 2) Css.dashed color
            , Css.color color
            , Css.padding (Css.px 10)
            , Css.width (Css.px 100)
            , Css.textAlign Css.center
            , animation
            ]
        ]
        [ Html.text text
        ]
    )


navigateButton : Msg -> String -> Html.Html Msg
navigateButton msg label =
    Button.button
        { onClick = msg
        , size = Button.Small
        , style = Button.Secondary
        , width = Button.WidthUnbounded
        }
        { label = label
        , state = Button.Enabled
        , icon = Nothing
        }
