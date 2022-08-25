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
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (css)
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
            , settings.theme
                { value = Maybe.withDefault "" <| Dict.get 1 state.textValues
                , autofocus = False
                , onInput = InputGiven 1
                , onBlur =
                    if settings.onBlur then
                        Just (InputGiven 1 "Neener neener Blur happened")

                    else
                        Nothing
                , isInError = settings.isInError
                , label = settings.label
                , height = settings.height
                , placeholder = settings.placeholder
                , showLabel = settings.showLabel
                }
            ]
    }


{-| -}
type alias State =
    { textValues : Dict Int String
    , settings : Control Settings
    }


{-| -}
init : State
init =
    { textValues = Dict.empty
    , settings = initControls
    }


type alias Settings =
    { theme : TextArea.Model Msg -> Html Msg
    , label : String
    , showLabel : Bool
    , placeholder : String
    , isInError : Bool
    , onBlur : Bool
    , height : TextArea.HeightBehavior
    }


initControls : Control Settings
initControls =
    Control.record Settings
        |> Control.field "theme"
            (Control.choice
                [ ( "view", Control.value TextArea.view )
                , ( "writing", Control.value TextArea.writing )
                ]
            )
        |> Control.field "label" (Control.string "Introductory paragraph")
        |> Control.field "showLabel" (Control.bool True)
        |> Control.field "placeholder" (Control.string "A long time ago, in a galaxy pretty near here actually...")
        |> Control.field "isInError" (Control.bool False)
        |> Control.field "onBlur" (Control.bool False)
        |> Control.field "height"
            (Control.choice
                [ ( "fixed", Control.value TextArea.Fixed )
                , ( "autoresize default", Control.value (TextArea.AutoResize TextArea.DefaultHeight) )
                , ( "autoresize singleline", Control.value (TextArea.AutoResize TextArea.SingleLine) )
                ]
            )


{-| -}
type Msg
    = InputGiven Id String
    | UpdateControl (Control Settings)


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        InputGiven id newValue ->
            ( { state | textValues = Dict.insert id newValue state.textValues }
            , Cmd.none
            )

        UpdateControl settings ->
            ( { state | settings = settings }, Cmd.none )



-- INTERNAL


type alias Id =
    Int
