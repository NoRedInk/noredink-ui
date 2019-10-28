module Examples.Button exposing (Msg, State, example, init, update)

{-|

@docs Msg, State, example, init, update

-}

import Css exposing (middle, verticalAlign)
import Debug.Control as Control exposing (Control)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, id)
import ModuleExample as ModuleExample exposing (Category(..), ModuleExample, ModuleMessages)
import Nri.Ui.Button.V9 as Button
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.Svg.V1 as Svg exposing (Svg)
import Nri.Ui.UiIcon.V1 as UiIcon


{-| -}
type State parentMsg
    = State (Control (Model parentMsg))


{-| -}
type ButtonType
    = Button
    | Link


{-| -}
example :
    (String -> ModuleMessages (Msg parentMsg) parentMsg)
    -> State parentMsg
    -> ModuleExample parentMsg
example unnamedMessages state =
    let
        messages =
            unnamedMessages "ButtonExample"
    in
    { name = "Nri.Ui.Button.V9"
    , category = Buttons
    , content = [ viewButtonExamples messages state ]
    }


{-| -}
type Msg parentMsg
    = SetState (State parentMsg)
    | NoOp


{-| -}
update : Msg msg -> State msg -> ( State msg, Cmd (Msg msg) )
update msg state =
    case msg of
        SetState newState ->
            ( newState, Cmd.none )

        NoOp ->
            ( state, Cmd.none )



-- INTERNAL


type alias Model msg =
    { label : String
    , icon : Maybe Svg
    , buttonType : ButtonType
    , width : Button.Attribute msg
    , state : Button.Attribute msg
    }


{-| -}
init : State msg
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


viewButtonExamples :
    ModuleMessages (Msg parentMsg) parentMsg
    -> State parentMsg
    -> Html parentMsg
viewButtonExamples messages (State control) =
    let
        model =
            Control.currentValue control
    in
    [ Control.view (State >> SetState >> messages.wrapper) control
        |> fromUnstyled
    , buttons messages model
    , toggleButtons messages
    , Button.delete
        { label = "Delete Something"
        , onClick = messages.showItWorked "delete"
        }
    , Button.link "linkExternalWithTracking"
        [ Button.unboundedWidth
        , Button.secondary
        , Button.linkExternalWithTracking
            { url = "#"
            , track = messages.showItWorked "linkExternalWithTracking clicked"
            }
        ]
    ]
        |> div []


buttons :
    ModuleMessages (Msg parentMsg) parentMsg
    -> Model parentMsg
    -> Html parentMsg
buttons messages model =
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
                , Button.onClick (messages.showItWorked "Button clicked!")
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


toggleButtons : ModuleMessages (Msg parentMsg) parentMsg -> Html parentMsg
toggleButtons messages =
    div []
        [ Heading.h3 [] [ text "Button toggle" ]
        , Button.toggleButton
            { onDeselect = messages.showItWorked "onDeselect"
            , onSelect = messages.showItWorked "onSelect"
            , label = "5"
            , pressed = False
            }
        , Button.toggleButton
            { onDeselect = messages.showItWorked "onDeselect"
            , onSelect = messages.showItWorked "onSelect"
            , label = "5"
            , pressed = True
            }
        ]
