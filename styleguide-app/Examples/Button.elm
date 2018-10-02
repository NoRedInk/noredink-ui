module Examples.Button exposing (Msg, State, example, init, update)

{- \
   @docs Msg, State, example, init, update,
-}

import Css exposing (middle, verticalAlign)
import Debug.Control as Control exposing (Control)
import Headings
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import ModuleExample as ModuleExample exposing (Category(..), ModuleExample, ModuleMessages)
import Nri.Ui.AssetPath exposing (Asset)
import Nri.Ui.Button.V4 as Button
import Nri.Ui.Icon.V3 as Icon


{-| -}
type Msg
    = SetState State


{-| -}
type State
    = State (Control Model)


{-| -}
type ButtonType
    = Button
    | Link
    | CopyToClipboard


{-| -}
example :
    { r | teach_assignments_copyWhite_svg : Asset, x : String }
    -> (String -> ModuleMessages Msg parentMsg)
    -> State
    -> ModuleExample parentMsg
example assets unnamedMessages state =
    let
        messages =
            unnamedMessages "ButtonExample"
    in
    { filename = "Nri.Ui.Button.V4"
    , category = Buttons
    , content =
        [ viewButtonExamples assets messages state ]
            |> List.map toUnstyled
    }


{-| -}
init : { r | performance : String, lock : String } -> State
init assets =
    Control.record Model
        |> Control.field "label" (Control.string "Button")
        |> Control.field "icon (copyToClipboard has only one choice)"
            (Control.maybe False <|
                Control.choice
                    [ ( "Performance", Control.value (Icon.performance assets) )
                    , ( "Lock", Control.value (Icon.lock assets) )
                    ]
            )
        |> Control.field "height (button and copyToClipboard only)"
            (Control.choice
                [ ( "Nri.Ui.Button.V4.HeightDefault", Control.value Button.HeightDefault )
                , ( "Nri.Ui.Button.V4.HeightBounded 2", Control.value (Button.HeightBounded 2) )
                , ( "Nri.Ui.Button.V4.HeightUnbounded", Control.value Button.HeightUnbounded )
                ]
            )
        |> Control.field "width"
            (Control.choice
                [ ( "Nri.Ui.Button.V4.WidthExact 120", Control.value <| Button.WidthExact 120 )
                , ( "Nri.Ui.Button.V4.WidthExact 70", Control.value <| Button.WidthExact 70 )
                , ( "Nri.Ui.Button.V4.WidthUnbounded", Control.value <| Button.WidthUnbounded )
                ]
            )
        |> Control.field "button type"
            (Control.choice
                [ ( "Nri.Ui.Button.V4.button", Control.value Button )
                , ( "Nri.Ui.Button.V4.link", Control.value Link )
                , ( "Nri.Ui.Button.V4.copyToClipboard", Control.value CopyToClipboard )
                ]
            )
        |> Control.field "state (button only)"
            (Control.choice <|
                List.map (\x -> ( toString x, Control.value x ))
                    [ Button.Enabled
                    , Button.Disabled
                    , Button.Error
                    , Button.Unfulfilled
                    , Button.Loading
                    , Button.Success
                    ]
            )
        |> State


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        SetState newState ->
            ( newState, Cmd.none )



-- INTERNAL


type alias Model =
    { label : String
    , icon : Maybe Icon.IconType
    , height : Button.ButtonHeight
    , width : Button.ButtonWidth
    , buttonType : ButtonType
    , state : Button.ButtonState
    }


viewButtonExamples :
    { r | teach_assignments_copyWhite_svg : Asset, x : String }
    -> ModuleMessages Msg parentMsg
    -> State
    -> Html parentMsg
viewButtonExamples assets messages (State control) =
    let
        model =
            Control.currentValue control
    in
    [ Control.view (State >> SetState >> messages.wrapper) control
        |> fromUnstyled
    , buttons assets messages model.height sizes model
    , toggleButtons messages
    , Button.delete assets
        { label = "Delete Something"
        , onClick = messages.showItWorked "delete"
        }
    , Button.linkExternalWithTracking
        (messages.showItWorked "linkExternalWithTracking clicked")
        { size = Button.Medium
        , style = Button.Secondary
        , width = Button.WidthUnbounded
        , label = "linkExternalWithTracking"
        , icon = Nothing
        , url = "#"
        }
    ]
        |> div []


sizes : List Button.ButtonSize
sizes =
    [ Button.Small
    , Button.Medium
    , Button.Large
    ]


allStyles : List Button.ButtonStyle
allStyles =
    [ Button.Primary
    , Button.Secondary
    , Button.Borderless
    , Button.Danger
    , Button.Premium
    ]


buttons :
    { r | teach_assignments_copyWhite_svg : Asset }
    -> ModuleMessages Msg parentMsg
    -> Button.ButtonHeight
    -> List Button.ButtonSize
    -> Model
    -> Html parentMsg
buttons assets messages height sizes model =
    let
        exampleRow height style =
            List.concat
                [ [ td
                        [ css
                            [ verticalAlign middle
                            ]
                        ]
                        [ text <| toString style ]
                  ]
                , sizes
                    |> List.map (exampleCell style height)
                ]
                |> tr []

        exampleCell style height size =
            (case model.buttonType of
                Link ->
                    Button.link
                        { size = size
                        , style = style
                        , label = model.label
                        , icon = model.icon
                        , url = ""
                        , width = model.width
                        }

                Button ->
                    Button.button
                        { size = size
                        , style = style
                        , onClick = messages.showItWorked (toString ( style, size ))
                        , height = model.height
                        , width = model.width
                        }
                        { label = model.label
                        , icon = model.icon
                        , state = model.state
                        }

                CopyToClipboard ->
                    Button.copyToClipboard
                        assets
                        { size = size
                        , style = style
                        , copyText = "wire up in your coffee file with clipboard.js"
                        , buttonLabel = model.label
                        , withIcon = model.icon /= Nothing
                        , height = model.height
                        , width = model.width
                        }
            )
                |> List.singleton
                |> td []
    in
    List.concat
        [ [ sizes
                |> List.map (\size -> th [] [ text <| toString size ])
                |> (\cells -> tr [] (th [] [] :: cells))
          ]
        , allStyles
            |> List.map (exampleRow height)
        ]
        |> table []


toggleButtons : ModuleMessages Msg parentMsg -> Html parentMsg
toggleButtons messages =
    div []
        [ Headings.h3 [ text "Button toggle" |> toUnstyled ]
            |> fromUnstyled
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
