module Examples.Slide exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Accessibility.Styled as Html
import Category exposing (Category(..))
import Css
import Example exposing (Example)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Keyed as Keyed
import List.Zipper as Zipper exposing (Zipper)
import Nri.Ui.Button.V8 as Button
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Slide.V1 as Slide


{-| -}
type Msg
    = TriggerAnimation Slide.AnimationDirection


{-| -}
type alias State =
    { direction : Slide.AnimationDirection
    , panels : Zipper Panel
    , previous : Maybe Panel
    }


{-| -}
example : Example State Msg
example =
    { name = "Nri.Ui.Slide.V1"
    , categories = [ Animations ]
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , view =
        \state ->
            [ Keyed.node "div"
                [ css
                    [ Slide.withSlidingContents
                    , Css.border3 (Css.px 3) Css.solid Colors.gray75
                    , Css.padding (Css.px 20)
                    , Css.width (Css.px 600)
                    ]
                ]
                (case state.previous of
                    Just previousPanel ->
                        [ viewPanel previousPanel (Slide.animateOut state.direction)
                        , viewPanel (Zipper.current state.panels) (Slide.animateIn state.direction)
                        ]

                    Nothing ->
                        [ viewPanel (Zipper.current state.panels) (Css.batch [])
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
                [ triggerAnimation Slide.FromLTR "Left-to-right"
                , triggerAnimation Slide.FromRTL "Right-to-left"
                ]
            ]
    }


{-| -}
init : State
init =
    { direction = Slide.FromRTL
    , panels = Zipper.from [] One [ Two, Three ]
    , previous = Nothing
    }


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        TriggerAnimation direction ->
            ( { state
                | direction = direction
                , panels =
                    case direction of
                        Slide.FromRTL ->
                            Zipper.next state.panels
                                |> Maybe.withDefault (Zipper.first state.panels)

                        Slide.FromLTR ->
                            Zipper.previous state.panels
                                |> Maybe.withDefault (Zipper.last state.panels)
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
                    ( Colors.redDark, "Panel One", "panel-1" )

                Two ->
                    ( Colors.ochre, "Panel Two", "panel-2" )

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


triggerAnimation : Slide.AnimationDirection -> String -> Html.Html Msg
triggerAnimation direction label =
    Button.button
        { onClick = TriggerAnimation direction
        , size = Button.Small
        , style = Button.Secondary
        , width = Button.WidthUnbounded
        }
        { label = label
        , state = Button.Enabled
        , icon = Nothing
        }
