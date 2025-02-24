module Examples.Text exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Category exposing (Category(..))
import Code
import CommonControls
import Css
import Css.Media exposing (withMedia)
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import Example exposing (Example)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Container.V2 as Container
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.MediaQuery.V1 exposing (mobile, notMobile)
import Nri.Ui.Spacing.V1 as Spacing
import Nri.Ui.Text.V6 as Text


moduleName : String
moduleName =
    "Text"


version : Int
version =
    6


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
        [ Text.caption [ Text.plaintext "caption" ]
        , Text.smallBody [ Text.plaintext "smallBody" ]
        , Text.mediumBody [ Text.plaintext "mediumBody" ]
        , Text.ugMediumBody [ Text.plaintext "ugMediumBody" ]
        ]
    , about = []
    , view =
        \ellieLinkConfig state ->
            let
                attributes =
                    List.map Tuple.second (Control.currentValue state.control)

                contentString =
                    String.concat <| List.map Tuple.first (Control.currentValue state.control)
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
                            toExampleCode name =
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
                        [ toExampleCode "mediumBody"
                        , toExampleCode "smallBody"
                        , toExampleCode "smallBodyGray"
                        , toExampleCode "caption"
                        , toExampleCode "footnote"
                        , toExampleCode "ugMediumBody"
                        , toExampleCode "ugSmallBody"
                        ]
                }
            , Heading.h2
                [ Heading.plaintext "Examples"
                , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
                ]
            , Text.caption [ Text.plaintext "NOTE: When using these styles, please read the documentation in the Elm module about \"Understanding spacing\"" ]
            , Heading.h3 [ Heading.plaintext "Paragraph styles" ]
            , viewExamples
                [ ( "mediumBody", Text.mediumBody )
                , ( "smallBody", Text.smallBody )
                , ( "smallBodyGray", Text.smallBodyGray )
                , ( "caption", Text.caption )
                , ( "footnote", Text.footnote )
                ]
                attributes
            , Heading.h3
                [ Heading.plaintext "Paragraph styles for user-authored content"
                , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
                ]
            , viewExamples
                [ ( "ugMediumBody", Text.ugMediumBody )
                , ( "ugSmallBody", Text.ugSmallBody )
                ]
                attributes
            , if String.startsWith "Text.html" contentString then
                Html.div
                    []
                    [ Heading.h3
                        [ Heading.plaintext "Html Code"
                        , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
                        ]
                    , viewCodeContainer <| CommonControls.exampleHtmlString
                    ]

              else
                Html.text ""
            ]
    }


viewCodeContainer : String -> Html msg
viewCodeContainer code =
    Container.view
        [ Container.html
            [ ControlView.viewCode code
            ]
        , Container.css
            [ Css.padding (Css.px 20)
            , Css.flexGrow (Css.num 1)
            , Css.backgroundColor Colors.gray20
            , withMedia [ mobile ]
                [ Css.borderTopLeftRadius Css.zero
                , Css.borderTopRightRadius Css.zero
                ]
            , withMedia [ notMobile ]
                [ Css.borderTopLeftRadius Css.zero
                , Css.borderBottomLeftRadius Css.zero
                ]
            ]
        ]


viewExamples : List ( String, List (Text.Attribute msg) -> Html msg ) -> List (Text.Attribute msg) -> Html msg
viewExamples examples attributes =
    let
        viewExample ( name, view ) =
            Html.tr []
                [ Html.th [] [ Html.text name ]
                , Html.td [] [ view attributes ]
                ]
    in
    Html.table [ css [ Css.width (Css.pct 100) ] ]
        [ Html.tbody [] <|
            List.map viewExample examples
        ]


{-| -}
type alias State =
    { control : Control (List ( String, Text.Attribute Msg ))
    }


{-| -}
init : State
init =
    { control =
        Control.list
            |> ControlExtra.listItem "content" controlContent
            |> ControlExtra.optionalBoolListItem "noBreak"
                ( "Text.noBreak True", Text.noBreak True )
            |> CommonControls.css { moduleName = "Text", use = Text.css }
    }


controlContent : Control ( String, Text.Attribute msg )
controlContent =
    CommonControls.content
        { moduleName = "Text"
        , paragraph = Nothing
        , plaintext = Text.plaintext
        , markdown = Just Text.markdown
        , html = Text.html
        , httpError = Nothing
        }


{-| -}
type Msg
    = UpdateControl (Control (List ( String, Text.Attribute Msg )))


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        UpdateControl newControl ->
            ( { state | control = newControl }, Cmd.none )
