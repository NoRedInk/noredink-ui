module Examples.Button exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import AtomicDesignType exposing (AtomicDesignType(..))
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


{-| -}
example : Example State Msg
example =
    { name = "Nri.Ui.Button.V10"
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , view = \state -> [ viewButtonExamples state ]
    , categories = [ Buttons ]
    , atomicDesignType = Molecule
    , keyboardSupport = []
    }


{-| -}
type State
    = State (Control Model)


{-| -}
type ButtonType
    = Button
    | Link


{-| -}
type Msg
    = SetState State
    | ShowItWorked String String
    | NoOp


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        SetState newState ->
            ( newState, Cmd.none )

        ShowItWorked group message ->
            let
                _ =
                    Debug.log group message
            in
            ( state, Cmd.none )

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
init : State
init =
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
        |> State


iconChoice : Control.Control (Maybe Svg)
iconChoice =
    Control.choice
        [ ( "Nothing", Control.value Nothing )
        , ( "Just Performance", Control.value (Just UiIcon.performance) )
        , ( "Just Share", Control.value (Just UiIcon.share) )
        , ( "Just Download", Control.value (Just UiIcon.download) )
        ]


viewButtonExamples : State -> Html Msg
viewButtonExamples (State control) =
    let
        model =
            Control.currentValue control
    in
    [ Control.view (State >> SetState) control
        |> fromUnstyled
    , buttons model
    , toggleButtons
    , Button.delete
        { label = "Delete Something"
        , onClick = ShowItWorked "ButtonExample" "delete"
        }
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


toggleButtons : Html Msg
toggleButtons =
    div []
        [ Heading.h3 [] [ text "Button toggle" ]
        , div [ css [ Css.displayFlex, Css.marginBottom (Css.px 20) ] ]
            [ Button.toggleButton
                { onDeselect = ShowItWorked "ButtonExample" "onDeselect"
                , onSelect = ShowItWorked "ButtonExample" "onSelect"
                , label = "5"
                , pressed = False
                }
            , Button.toggleButton
                { onDeselect = ShowItWorked "ButtonExample" "onDeselect"
                , onSelect = ShowItWorked "ButtonExample" "onSelect"
                , label = "5"
                , pressed = True
                }
            ]
        ]
