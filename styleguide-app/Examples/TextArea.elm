module Examples.TextArea exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Category exposing (Category(..))
import Css
import Dict exposing (Dict)
import Example exposing (Example)
import Html.Styled as Html
import Html.Styled.Attributes as Attributes exposing (css)
import Nri.Ui.Checkbox.V5 as Checkbox
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.InputStyles.V3 as InputStyles exposing (Theme(..))
import Nri.Ui.TextArea.V4 as TextArea


{-| -}
type Msg
    = InputGiven Id String
    | ToggleLabel Bool
    | ToggleAutoResize Bool
    | ToggleErrorState Bool


{-| -}
type alias State =
    { textValues : Dict Int String
    , showLabel : Checkbox.IsSelected
    , isInError : Checkbox.IsSelected
    , autoResize : Checkbox.IsSelected
    }


{-| -}
example : Example State Msg
example =
    { name = "TextArea"
    , version = 4
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , categories = [ Inputs ]
    , keyboardSupport = []
    , preview =
        [ Html.div [ css [ Css.position Css.relative ] ]
            [ Html.textarea
                [ css
                    [ InputStyles.input Standard False
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
            [ Heading.h1 [] [ Html.text "Textarea controls" ]
            , Html.div []
                [ Checkbox.viewWithLabel
                    { identifier = "show-textarea-label"
                    , label = "Show Label"
                    , setterMsg = ToggleLabel
                    , selected = state.showLabel
                    , disabled = False
                    , theme = Checkbox.Square
                    }
                , Checkbox.viewWithLabel
                    { identifier = "textarea-autoresize"
                    , label = "Autoresize"
                    , setterMsg = ToggleAutoResize
                    , selected = state.autoResize
                    , disabled = False
                    , theme = Checkbox.Square
                    }
                , Checkbox.viewWithLabel
                    { identifier = "textarea-isInError"
                    , label = "Show Error State"
                    , setterMsg = ToggleErrorState
                    , selected = state.isInError
                    , disabled = False
                    , theme = Checkbox.Square
                    }
                ]
            , TextArea.view
                { value = Maybe.withDefault "" <| Dict.get 1 state.textValues
                , autofocus = False
                , onInput = InputGiven 1
                , onBlur = Nothing
                , isInError = state.isInError == Checkbox.Selected
                , label = "TextArea.view"
                , height =
                    if state.autoResize == Checkbox.Selected then
                        TextArea.AutoResize TextArea.SingleLine

                    else
                        TextArea.Fixed
                , placeholder = "Placeholder"
                , showLabel = state.showLabel == Checkbox.Selected
                }
            , Html.br [ css [ Css.marginBottom (Css.px 10) ] ] []
            , TextArea.writing
                { value = Maybe.withDefault "" <| Dict.get 2 state.textValues
                , autofocus = False
                , onInput = InputGiven 2
                , onBlur = Nothing
                , isInError = state.isInError == Checkbox.Selected
                , label = "TextArea.writing"
                , height =
                    if state.autoResize == Checkbox.Selected then
                        TextArea.AutoResize TextArea.DefaultHeight

                    else
                        TextArea.Fixed
                , placeholder = "Placeholder"
                , showLabel = state.showLabel == Checkbox.Selected
                }
            , Html.br [ css [ Css.marginBottom (Css.px 10) ] ] []
            , TextArea.contentCreation
                { value = Maybe.withDefault "" <| Dict.get 3 state.textValues
                , autofocus = False
                , onInput = InputGiven 3
                , onBlur = Nothing
                , isInError = state.isInError == Checkbox.Selected
                , label = "TextArea.contentCreation"
                , height =
                    if state.autoResize == Checkbox.Selected then
                        TextArea.AutoResize TextArea.DefaultHeight

                    else
                        TextArea.Fixed
                , placeholder = "Placeholder"
                , showLabel = state.showLabel == Checkbox.Selected
                }
            , Html.br [ css [ Css.marginBottom (Css.px 10) ] ] []
            , TextArea.writing
                { value = Maybe.withDefault "" <| Dict.get 4 state.textValues
                , autofocus = False
                , onInput = InputGiven 4
                , onBlur = Just (InputGiven 4 "Neener neener Blur happened")
                , isInError = state.isInError == Checkbox.Selected
                , label = "TextArea.writing onBlur demonstration"
                , height =
                    if state.autoResize == Checkbox.Selected then
                        TextArea.AutoResize TextArea.DefaultHeight

                    else
                        TextArea.Fixed
                , placeholder = "Placeholder"
                , showLabel = state.showLabel == Checkbox.Selected
                }
            ]
    }


{-| -}
init : State
init =
    { textValues = Dict.empty
    , showLabel = Checkbox.Selected
    , isInError = Checkbox.NotSelected
    , autoResize = Checkbox.NotSelected
    }


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

        ToggleLabel bool ->
            ( { state | showLabel = toggle bool }
            , Cmd.none
            )

        ToggleErrorState bool ->
            ( { state | isInError = toggle bool }
            , Cmd.none
            )

        ToggleAutoResize bool ->
            ( { state | autoResize = toggle bool }
            , Cmd.none
            )



-- INTERNAL


type alias Id =
    Int
