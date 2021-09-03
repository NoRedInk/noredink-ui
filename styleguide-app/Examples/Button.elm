module Examples.Button exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Category exposing (Category(..))
import Css exposing (middle, verticalAlign)
import Debug.Control as Control exposing (Control)
import Example exposing (Example)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, id)
import KeyboardSupport exposing (Direction(..), Key(..))
import Nri.Ui.Button.V10 as Button
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.Svg.V1 as Svg exposing (Svg)
import Nri.Ui.UiIcon.V1 as UiIcon
import Set exposing (Set)


{-| -}
example : Example State Msg
example =
    { name = "Button"
    , version = 10
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , view = \state -> [ viewButtonExamples state ]
    , categories = [ Buttons ]
    , keyboardSupport = []
    }


{-| -}
type alias State =
    { debugControlsState : Control Model
    , pressedToggleButtons : Set Int
    }


{-| -}
init : State
init =
    { debugControlsState = initDebugControls
    , pressedToggleButtons = Set.singleton 1
    }


{-| -}
type ButtonType
    = Button
    | Link


{-| -}
type Msg
    = SetDebugControlsState (Control Model)
    | ShowItWorked String String
    | ToggleToggleButton Int
    | NoOp


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        SetDebugControlsState newDebugControlsState ->
            ( { state | debugControlsState = newDebugControlsState }
            , Cmd.none
            )

        ShowItWorked group message ->
            let
                _ =
                    Debug.log group message
            in
            ( state, Cmd.none )

        ToggleToggleButton id ->
            ( { state
                | pressedToggleButtons =
                    if Set.member id state.pressedToggleButtons then
                        Set.remove id state.pressedToggleButtons

                    else
                        Set.insert id state.pressedToggleButtons
              }
            , Cmd.none
            )

        NoOp ->
            ( state, Cmd.none )



-- INTERNAL


type alias Model =
    { label : String
    , icon : Maybe Svg
    , buttonType : ButtonType
    , width : Button.Attribute Msg
    , state : Button.Attribute Msg
    }


{-| -}
initDebugControls : Control Model
initDebugControls =
    Control.record Model
        |> Control.field "label" (Control.string "Label")
        |> Control.field "icon" iconChoice
        |> Control.field "button type"
            (Control.choice
                [ ( "button", Control.value Button )
                , ( "link", Control.value Link )
                ]
            )
        |> Control.field "width"
            (Control.choice
                [ ( "exactWidth 120", Control.value (Button.exactWidth 120) )
                , ( "exactWidth 70", Control.value (Button.exactWidth 70) )
                , ( "boundedWidth 100 180", Control.value (Button.boundedWidth { min = 100, max = 180 }) )
                , ( "unboundedWidth", Control.value Button.unboundedWidth )
                , ( "fillContainerWidth", Control.value Button.fillContainerWidth )
                ]
            )
        |> Control.field "state (button only)"
            (Control.choice
                [ ( "enabled", Control.value Button.enabled )
                , ( "disabled", Control.value Button.disabled )
                , ( "error", Control.value Button.error )
                , ( "unfulfilled", Control.value Button.unfulfilled )
                , ( "loading", Control.value Button.loading )
                , ( "success", Control.value Button.success )
                ]
            )


iconChoice : Control.Control (Maybe Svg)
iconChoice =
    Control.choice
        [ ( "none", Control.value Nothing )
        , ( "preview", Control.value (Just UiIcon.preview) )
        , ( "arrowLeft", Control.value (Just UiIcon.arrowLeft) )
        , ( "performance", Control.value (Just UiIcon.performance) )
        , ( "share", Control.value (Just UiIcon.share) )
        , ( "download", Control.value (Just UiIcon.download) )
        ]


viewButtonExamples : State -> Html Msg
viewButtonExamples state =
    let
        model =
            Control.currentValue state.debugControlsState
    in
    [ Control.view SetDebugControlsState state.debugControlsState
        |> fromUnstyled
    , buttons model
    , toggleButtons state.pressedToggleButtons
    , Button.link "linkExternalWithTracking"
        [ Button.unboundedWidth
        , Button.secondary
        , Button.linkExternalWithTracking
            { url = "#"
            , track = ShowItWorked "ButtonExample" "linkExternalWithTracking clicked"
            }
        ]
    ]
        |> div []


buttons : Model -> Html Msg
buttons model =
    let
        sizes =
            [ ( Button.small, "small" )
            , ( Button.medium, "medium" )
            , ( Button.large, "large" )
            ]

        styles =
            [ ( Button.primary, "primary" )
            , ( Button.secondary, "secondary" )
            , ( Button.danger, "danger" )
            , ( Button.premium, "premium" )
            ]

        exampleRow ( style, styleName ) =
            List.concat
                [ [ td
                        [ css
                            [ verticalAlign middle
                            ]
                        ]
                        [ text styleName ]
                  ]
                , List.map (Tuple.first >> exampleCell style) sizes
                ]
                |> tr []

        buttonOrLink =
            case model.buttonType of
                Link ->
                    Button.link

                Button ->
                    Button.button

        exampleCell setStyle setSize =
            buttonOrLink model.label
                [ setSize
                , setStyle
                , model.width
                , model.state
                , Button.custom [ Html.Styled.Attributes.class "styleguide-button" ]
                , Button.onClick (ShowItWorked "ButtonExample" "Button clicked!")
                , case model.icon of
                    Just icon ->
                        Button.icon icon

                    Nothing ->
                        Button.custom []
                ]
                |> List.singleton
                |> td
                    [ css
                        [ verticalAlign middle
                        , Css.width (Css.px 200)
                        ]
                    ]
    in
    List.concat
        [ [ sizes
                |> List.map (\( _, sizeName ) -> th [] [ text sizeName ])
                |> (\cells -> tr [] (th [] [] :: cells))
          ]
        , List.map exampleRow styles
        ]
        |> table []


toggleButtons : Set Int -> Html Msg
toggleButtons pressedToggleButtons =
    div []
        [ Heading.h3 [] [ text "Button toggle" ]
        , div [ css [ Css.displayFlex, Css.marginBottom (Css.px 20) ] ]
            [ Button.toggleButton
                { onDeselect = ToggleToggleButton 0
                , onSelect = ToggleToggleButton 0
                , label = "5"
                , pressed = Set.member 0 pressedToggleButtons
                }
            , Button.toggleButton
                { onDeselect = ToggleToggleButton 1
                , onSelect = ToggleToggleButton 1
                , label = "Kindergarten"
                , pressed = Set.member 1 pressedToggleButtons
                }
            ]
        ]
