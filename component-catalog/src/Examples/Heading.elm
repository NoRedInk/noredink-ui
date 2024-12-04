module Examples.Heading exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Category exposing (Category(..))
import Code
import CommonControls
import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import Example exposing (Example)
import Html.Styled exposing (..)
import Nri.Ui.ClickableText.V4 as ClickableText
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Spacing.V1 as Spacing
import Nri.Ui.Text.V6 as Text
import ViewHelpers exposing (viewExamples)


moduleName : String
moduleName =
    "Heading"


version : Int
version =
    3


{-| -}
example : Example State Msg
example =
    { name = moduleName
    , version = version
    , categories = [ Text ]
    , keyboardSupport = []
    , init = ( init, Cmd.none )
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview =
        [ Heading.h1 [ Heading.plaintext "h1" ]
        , Heading.h2 [ Heading.plaintext "h2" ]
        , Heading.h3 [ Heading.plaintext "h3" ]
        , Heading.h4 [ Heading.plaintext "h4" ]
        ]
    , about =
        [ Text.smallBody
            [ Text.html
                [ text "Please refer to the "
                , ClickableText.link "headings testing guide"
                    [ ClickableText.linkExternal "https://paper.dropbox.com/doc/Accessibility-testing-Headings--CJezbs8vSmyIxcqChSGyEj7CAg-6l0cMj4e1kArUIb7fgNsH"
                    , ClickableText.appearsInline
                    ]
                , text " to understand expectations and user impact of proper heading structure."
                ]
            ]
        ]
    , view =
        \ellieLinkConfig state ->
            let
                examples =
                    [ ( "h1", Heading.h1 )
                    , ( "h2", Heading.h2 )
                    , ( "h3", Heading.h3 )
                    , ( "h4", Heading.h4 )
                    , ( "h5", Heading.h5 )
                    ]

                attributes =
                    List.map Tuple.second (Control.currentValue state.control)
            in
            [ ControlView.view
                { ellieLinkConfig = ellieLinkConfig
                , name = moduleName
                , version = version
                , update = UpdateControl
                , settings = state.control
                , mainType = Just "RootHtml.Html msg"
                , extraCode = []
                , renderExample = Code.unstyledView
                , toExampleCode =
                    \settings ->
                        let
                            toExampleCode ( name, _ ) =
                                { sectionName = name
                                , code =
                                    moduleName
                                        ++ "."
                                        ++ name
                                        ++ "\n    [ "
                                        ++ String.join "\n    , " (List.map Tuple.first settings)
                                        ++ "\n    ]"
                                }
                        in
                        List.map toExampleCode examples
                }
            , Heading.h2
                [ Heading.plaintext "Customizable Examples"
                , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
                ]
            , examples
                |> List.map (\( name, view ) -> ( name, view attributes ))
                |> viewExamples
            ]
    }


{-| -}
type alias State =
    { control : Control Settings
    }


init : State
init =
    { control =
        Control.list
            |> ControlExtra.listItem "content" controlContent
            |> CommonControls.css { moduleName = moduleName, use = Heading.css }
            |> ControlExtra.optionalListItem "style" controlStyle
    }


controlContent : Control ( String, Heading.Attribute msg )
controlContent =
    CommonControls.content
        { moduleName = moduleName
        , paragraph = Nothing
        , plaintext = Heading.plaintext
        , markdown = Just Heading.markdown
        , html = Heading.html
        , httpError = Nothing
        }


controlStyle : Control ( String, Heading.Attribute msg )
controlStyle =
    CommonControls.choice moduleName
        [ ( "top", Heading.top )
        , ( "subhead", Heading.subhead )
        , ( "small", Heading.small )
        ]


type alias Settings =
    List ( String, Heading.Attribute Msg )


{-| -}
type Msg
    = UpdateControl (Control Settings)


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        UpdateControl settings ->
            ( { state | control = settings }, Cmd.none )
