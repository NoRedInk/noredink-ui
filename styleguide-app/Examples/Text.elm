module Examples.Text exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Category exposing (Category(..))
import CommonControls
import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import Example exposing (Example)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.Text.V6 as Text


{-| -}
example : Example State Msg
example =
    { name = "Text"
    , version = 6
    , categories = [ Text ]
    , keyboardSupport = []
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview =
        [ Text.caption [ Text.plaintext "caption" ]
        , Text.smallBody [ Text.plaintext "smallBody" ]
        , Text.mediumBody [ Text.plaintext "mediumBody" ]
        , Text.ugMediumBody [ Text.plaintext "ugMediumBody" ]
        ]
    , view =
        \state ->
            let
                attributes =
                    Control.currentValue state.control
            in
            [ Text.caption [ Text.plaintext "NOTE: When using these styles, please read the documentation in the Elm module about \"Understanding spacing\"" ]
            , ControlView.view
                { update = UpdateControl
                , settings = state.control
                , toExampleCode =
                    \settings ->
                        let
                            toExampleCode name =
                                { sectionName = name
                                , code =
                                    "Text."
                                        ++ name
                                        ++ "\n    [ "
                                        ++ "\n      TODO"
                                        ++ "\n    ]"
                                }
                        in
                        [ toExampleCode "mediumBody"
                        , toExampleCode "smallBody"
                        , toExampleCode "smallBodyGray"
                        , toExampleCode "caption"
                        , toExampleCode "ugMediumBody"
                        , toExampleCode "ugSmallBody"
                        ]
                }
            , Heading.h2 [ Heading.style Heading.Top ] [ Html.text "Paragraph styles" ]
            , viewExamples
                [ ( "mediumBody", Text.mediumBody )
                , ( "smallBody", Text.smallBody )
                , ( "smallBodyGray", Text.smallBodyGray )
                , ( "caption", Text.caption )
                ]
                attributes
            , Heading.h2 [ Heading.style Heading.Top ] [ Html.text "Paragraph styles for user-authored content" ]
            , viewExamples
                [ ( "ugMediumBody", Text.ugMediumBody )
                , ( "ugSmallBody", Text.ugSmallBody )
                ]
                attributes
            ]
    }


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
    { control : Control (List (Text.Attribute Msg))
    }


{-| -}
init : State
init =
    { control =
        ControlExtra.list
            |> ControlExtra.listItem "content" controlContent
            |> ControlExtra.listItem "noBreak"
                (Control.map Text.noBreak (Control.bool False))
            |> ControlExtra.optionalListItem "css"
                (Control.value
                    (Text.css
                        [ Css.border3 (Css.px 1) Css.solid Colors.aqua
                        , Css.color Colors.aquaDark
                        ]
                    )
                )
    }


controlContent : Control (Text.Attribute msg)
controlContent =
    CommonControls.content
        { moduleName = "Text"
        , plaintext = Text.plaintext
        , markdown = Just Text.markdown
        , html = Text.html
        , httpError = Nothing
        }
        |> Control.map Tuple.second


{-| -}
type Msg
    = UpdateControl (Control (List (Text.Attribute Msg)))


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        UpdateControl newControl ->
            ( { state | control = newControl }, Cmd.none )
