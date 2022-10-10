module Examples.TextArea exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Category exposing (Category(..))
import Code
import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import Example exposing (Example)
import Html.Styled as Html
import Html.Styled.Attributes as Attributes exposing (css)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.InputStyles.V4 as InputStyles exposing (Theme(..))
import Nri.Ui.TextArea.V5 as TextArea


moduleName : String
moduleName =
    "TextArea"


version : Int
version =
    5


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
                ( settings, attributes ) =
                    Control.currentValue state.settings

                toExampleCode name =
                    [ moduleName ++ "." ++ name
                    , Code.record
                        [ ( "isInError", Code.bool settings.isInError )
                        , ( "label", Code.string settings.label )
                        , ( "height", Tuple.first settings.height )
                        , ( "placeholder", Code.string settings.placeholder )
                        ]
                    , Code.list <|
                        ("TextArea.value " ++ Code.string state.value)
                            :: "TextArea.onInput identity"
                            :: List.map Tuple.first attributes
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
                , renderExample = Code.unstyledView
                , toExampleCode =
                    \_ ->
                        [ { sectionName = "view"
                          , code = toExampleCode "view"
                          }
                        ]
                }
            , Heading.h2 [ Heading.plaintext "Example" ]
            , TextArea.view
                { isInError = settings.isInError
                , label = settings.label
                , height = Tuple.second settings.height
                , placeholder = settings.placeholder
                }
                (TextArea.value state.value
                    :: TextArea.onInput UpdateValue
                    :: List.map Tuple.second attributes
                )
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
    ( Settings_
    , List ( String, TextArea.Attribute Msg )
    )


type alias Settings_ =
    { label : String
    , placeholder : String
    , isInError : Bool
    , height : ( String, TextArea.HeightBehavior )
    }


controlAttributes : Control (List ( String, TextArea.Attribute Msg ))
controlAttributes =
    ControlExtra.list
        |> ControlExtra.optionalListItem
            "theme"
            (Control.choice
                [ ( "standard", Control.value ( "TextArea.standard", TextArea.standard ) )
                , ( "writing", Control.value ( "TextArea.writing", TextArea.writing ) )
                ]
            )
        |> ControlExtra.optionalBoolListItem "onBlur"
            ( "TextArea.onBlur " ++ Code.string "Neener neener Blur happened"
            , TextArea.onBlur (UpdateValue "Neener neener Blur happened")
            )


initControls : Control Settings
initControls =
    Control.record
        (\attributes label placeholder isInError height ->
            ( Settings_ label placeholder isInError height
            , attributes
            )
        )
        |> Control.field "attributes" controlAttributes
        |> Control.field "label" (Control.string "Introductory paragraph")
        |> Control.field "placeholder" (Control.string "A long time ago, in a galaxy pretty near here actually...")
        |> Control.field "isInError" (Control.bool False)
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
