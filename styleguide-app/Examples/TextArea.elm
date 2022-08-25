module Examples.TextArea exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Category exposing (Category(..))
import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.View as ControlView
import Dict exposing (Dict)
import Example exposing (Example)
import Html.Styled as Html
import Html.Styled.Attributes as Attributes exposing (css)
import Nri.Ui.Checkbox.V6 as Checkbox
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.InputStyles.V3 as InputStyles exposing (Theme(..))
import Nri.Ui.TextArea.V4 as TextArea


moduleName : String
moduleName =
    "TextArea"


version : Int
version =
    4


{-| -}
example : Example State Msg
example =
    { name = moduleName
    , version = version
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , categories = [ Inputs ]
    , keyboardSupport = []
    , preview =
        [ Html.div [ css [ Css.position Css.relative ] ]
            [ Html.textarea
                [ css
                    [ InputStyles.input Standard
                    , Css.minHeight (Css.px 100)
                    , Css.maxWidth (Css.px 140)
                    , Css.backgroundColor Colors.white |> Css.important
                    , Css.cursor Css.pointer |> Css.important
                    , Css.resize Css.none
                    ]
                , Attributes.class "override-sass-styles"
                , Attributes.disabled True
                , Attributes.id "preview-textarea"
                ]
                []
            , Html.label
                [ css [ InputStyles.label Standard False ]
                , Attributes.for "preview-textarea"
                ]
                [ Html.text "Label" ]
            ]
        ]
    , view =
        \ellieLinkConfig state ->
            let
                settings =
                    Control.currentValue state.settings
            in
            [ ControlView.view
                { ellieLinkConfig = ellieLinkConfig
                , name = moduleName
                , version = version
                , update = UpdateControl
                , settings = state.settings
                , mainType = Nothing
                , extraCode = []
                , toExampleCode = \_ -> []
                }
            , Heading.h2 [ Heading.plaintext "Example" ]
            , TextArea.view
                { value = Maybe.withDefault "" <| Dict.get 5 state.textValues
                , autofocus = False
                , onInput = InputGiven 5
                , onBlur = Nothing
                , isInError = settings.isInError
                , label = settings.label
                , height = TextArea.Fixed
                , placeholder = "Placeholder"
                , showLabel = settings.showLabel
                }
            , Heading.h2 [ Heading.plaintext "Textarea controls" ]
            , Html.div []
                [ Checkbox.viewWithLabel
                    { identifier = "textarea-autoresize"
                    , label = "Autoresize"
                    , setterMsg = ToggleAutoResize
                    , selected = state.autoResize
                    , disabled = False
                    , theme = Checkbox.Square
                    , containerCss = []
                    , enabledLabelCss = []
                    , disabledLabelCss = []
                    }
                ]
            , TextArea.view
                { value = Maybe.withDefault "" <| Dict.get 1 state.textValues
                , autofocus = False
                , onInput = InputGiven 1
                , onBlur = Nothing
                , isInError = False
                , label = "TextArea.view"
                , height =
                    if state.autoResize == Checkbox.Selected then
                        TextArea.AutoResize TextArea.SingleLine

                    else
                        TextArea.Fixed
                , placeholder = "Placeholder"
                , showLabel = True
                }
            , Html.br [ css [ Css.marginBottom (Css.px 10) ] ] []
            , TextArea.writing
                { value = Maybe.withDefault "" <| Dict.get 2 state.textValues
                , autofocus = False
                , onInput = InputGiven 2
                , onBlur = Nothing
                , isInError = False
                , label = "TextArea.writing"
                , height =
                    if state.autoResize == Checkbox.Selected then
                        TextArea.AutoResize TextArea.DefaultHeight

                    else
                        TextArea.Fixed
                , placeholder = "Placeholder"
                , showLabel = True
                }
            , Html.br [ css [ Css.marginBottom (Css.px 10) ] ] []
            , TextArea.contentCreation
                { value = Maybe.withDefault "" <| Dict.get 3 state.textValues
                , autofocus = False
                , onInput = InputGiven 3
                , onBlur = Nothing
                , isInError = False
                , label = "TextArea.contentCreation"
                , height =
                    if state.autoResize == Checkbox.Selected then
                        TextArea.AutoResize TextArea.DefaultHeight

                    else
                        TextArea.Fixed
                , placeholder = "Placeholder"
                , showLabel = True
                }
            , Html.br [ css [ Css.marginBottom (Css.px 10) ] ] []
            , TextArea.writing
                { value = Maybe.withDefault "" <| Dict.get 4 state.textValues
                , autofocus = False
                , onInput = InputGiven 4
                , onBlur = Just (InputGiven 4 "Neener neener Blur happened")
                , isInError = False
                , label = "TextArea.writing onBlur demonstration"
                , height =
                    if state.autoResize == Checkbox.Selected then
                        TextArea.AutoResize TextArea.DefaultHeight

                    else
                        TextArea.Fixed
                , placeholder = "Placeholder"
                , showLabel = True
                }
            ]
    }


{-| -}
type alias State =
    { textValues : Dict Int String
    , autoResize : Checkbox.IsSelected
    , settings : Control Settings
    }


{-| -}
init : State
init =
    { textValues = Dict.empty
    , autoResize = Checkbox.NotSelected
    , settings = initControls
    }


type alias Settings =
    { label : String
    , showLabel : Bool
    , isInError : Bool
    }


initControls : Control Settings
initControls =
    Control.record Settings
        |> Control.field "label" (Control.string "Introductory paragraph")
        |> Control.field "showLabel" (Control.bool True)
        |> Control.field "isInError" (Control.bool False)


{-| -}
type Msg
    = InputGiven Id String
    | ToggleAutoResize Bool
    | UpdateControl (Control Settings)


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    let
        toggle bool =
            if bool then
                Checkbox.Selected

            else
                Checkbox.NotSelected
    in
    case msg of
        InputGiven id newValue ->
            ( { state | textValues = Dict.insert id newValue state.textValues }
            , Cmd.none
            )

        ToggleAutoResize bool ->
            ( { state | autoResize = toggle bool }
            , Cmd.none
            )

        UpdateControl settings ->
            ( { state | settings = settings }, Cmd.none )



-- INTERNAL


type alias Id =
    Int
