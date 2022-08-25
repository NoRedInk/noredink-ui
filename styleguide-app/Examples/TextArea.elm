module Examples.TextArea exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Category exposing (Category(..))
import Code
import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.View as ControlView
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

                toExampleCode name =
                    [ moduleName ++ "." ++ name
                    , Code.record
                        [ ( "value", Code.string state.value )
                        , ( "autofocus", Code.bool False )
                        , ( "onInput", "identity" )
                        , ( "onBlur"
                          , Code.maybe Nothing
                            -- TODO: support multiline if
                          )
                        , ( "isInError", Code.bool settings.isInError )
                        , ( "label", Code.string settings.label )
                        , ( "height", Tuple.first settings.height )
                        , ( "placeholder", Code.string settings.placeholder )
                        , ( "showLabel", Code.bool settings.showLabel )
                        ]
                    ]
                        |> String.join ""
            in
            [ ControlView.view
                { ellieLinkConfig = ellieLinkConfig
                , name = moduleName
                , version = version
                , update = UpdateControl
                , settings = state.settings
                , mainType = Just "RootHtml.Html String"
                , extraCode = []
                , toExampleCode =
                    \_ ->
                        [ { sectionName = "view"
                          , code = toExampleCode "view"
                          }
                        , { sectionName = "writing"
                          , code = toExampleCode "writing"
                          }
                        ]
                }
            , Heading.h2 [ Heading.plaintext "Example" ]
            , settings.theme
                { value = state.value
                , autofocus = False
                , onInput = UpdateValue
                , onBlur =
                    if settings.onBlur then
                        Just (UpdateValue "Neener neener Blur happened")

                    else
                        Nothing
                , isInError = settings.isInError
                , label = settings.label
                , height = Tuple.second settings.height
                , placeholder = settings.placeholder
                , showLabel = settings.showLabel
                }
            ]
    }


{-| -}
type alias State =
    { value : String
    , settings : Control Settings
    }


{-| -}
init : State
init =
    { value = ""
    , settings = initControls
    }


type alias Settings =
    { theme : TextArea.Model Msg -> Html Msg
    , label : String
    , showLabel : Bool
    , placeholder : String
    , isInError : Bool
    , onBlur : Bool
    , height : ( String, TextArea.HeightBehavior )
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
                [ ( "fixed"
                  , Control.value ( "TextArea.Fixed", TextArea.Fixed )
                  )
                , ( "autoresize default"
                  , Control.value
                        ( Code.withParens "TextArea.AutoResize TextArea.DefaultHeight"
                        , TextArea.AutoResize TextArea.DefaultHeight
                        )
                  )
                , ( "autoresize singleline"
                  , Control.value
                        ( Code.withParens "TextArea.AutoResize TextArea.SingleLine"
                        , TextArea.AutoResize TextArea.SingleLine
                        )
                  )
                ]
            )


{-| -}
type Msg
    = UpdateValue String
    | UpdateControl (Control Settings)


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        UpdateValue newValue ->
            ( { state | value = newValue }
            , Cmd.none
            )

        UpdateControl settings ->
            ( { state | settings = settings }, Cmd.none )
