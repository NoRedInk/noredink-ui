module Examples.Container exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Category exposing (Category(..))
import Code
import CommonControls
import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import Example exposing (Example)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Container.V2 as Container
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Spacing.V1 as Spacing
import Nri.Ui.Text.V6 as Text


moduleName : String
moduleName =
    "Container"


version : Int
version =
    2


{-| -}
example : Example State Msg
example =
    { name = moduleName
    , version = version
    , categories = [ Layout ]
    , keyboardSupport = []
    , init = ( init, Cmd.none )
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview =
        [ Container.view []
        , Container.view
            [ Container.disabled
            , Container.css [ Css.marginTop (Css.px 8) ]
            ]
        ]
    , about = []
    , view =
        \ellieLinkConfig state ->
            let
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
                            stringAttributes =
                                List.map Tuple.first settings
                        in
                        [ { sectionName = "Default Container"
                          , code = viewExampleCode stringAttributes
                          }
                        , { sectionName = "Gray Container"
                          , code = viewExampleCode ("Container.gray" :: stringAttributes)
                          }
                        , { sectionName = "Pillow Container"
                          , code = viewExampleCode ("Container.pillow" :: stringAttributes)
                          }
                        , { sectionName = "Buttony Container"
                          , code = viewExampleCode ("Container.buttony" :: stringAttributes)
                          }
                        , { sectionName = "Disabled Container"
                          , code = viewExampleCode ("Container.disabled" :: stringAttributes)
                          }
                        ]
                }
            , Heading.h2
                [ Heading.plaintext "Customizable Examples"
                , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
                ]
            , Html.div
                [ css
                    [ Css.property "display" "grid"
                    , Css.property "grid-template-columns" "repeat(2, minmax(0, 1fr))"
                    , Css.property "column-gap" "24px"
                    , Css.property "row-gap" "8px"
                    ]
                ]
                [ viewExample
                    { name = "Default Container"
                    , description = "Your go-to container."
                    }
                    (Container.default :: defaultExampleContent :: attributes)
                , viewExample
                    { name = "Gray Container"
                    , description = "A container that doesn’t draw too much attention to itself."
                    }
                    (Container.gray :: grayExampleContent :: attributes)
                , viewExample
                    { name = "Pillow Container"
                    , description = "When you want something big and soft."
                    }
                    (Container.pillow :: pillowExampleContent :: attributes)
                , viewExample
                    { name = "Buttony Container"
                    , description = "Used for clickable button card things."
                    }
                    (Container.buttony :: buttonyExampleContent :: attributes)
                , viewExample
                    { name = "Disabled Container"
                    , description = "Used to indicate content is locked/inaccessible"
                    }
                    (Container.disabled :: disabledExampleContent :: attributes)
                ]
            ]
    }


viewExample : { name : String, description : String } -> List (Container.Attribute msg) -> Html msg
viewExample { name, description } attributes =
    Html.section
        [ css
            [ Css.marginTop (Css.px 32)
            ]
        ]
        [ Heading.h3
            [ Heading.plaintext name
            , Heading.css [ Css.margin Css.zero ]
            ]
        , Text.smallBodyGray
            [ Text.css [ Css.margin3 (Css.px 2) Css.zero (Css.px 12) ]
            , Text.plaintext description
            ]
        , Container.view attributes
        ]


contentStack : List (Html msg) -> Container.Attribute msg
contentStack children =
    Container.html
        [ Html.div
            [ css
                [ Css.displayFlex
                , Css.flexDirection Css.column
                , Css.property "gap" "8px"
                ]
            ]
            children
        ]


contentTitle : String -> Html msg
contentTitle text =
    Heading.h3
        [ Heading.plaintext text
        , Heading.css [ Css.margin Css.zero ]
        ]


flushParagraphMargin : Text.Attribute msg
flushParagraphMargin =
    Text.css [ Css.margin Css.zero ]


defaultExampleContent : Container.Attribute msg
defaultExampleContent =
    contentStack
        [ contentTitle "Persuasive Essay: Should homework be banned?"
        , Text.smallBodyGray
            [ flushParagraphMargin
            , Text.plaintext "Due Friday · 12 of 24 students have started"
            ]
        ]


grayExampleContent : Container.Attribute msg
grayExampleContent =
    Container.html
        [ Text.mediumBody
            [ flushParagraphMargin
            , Text.html
                [ Html.strong [] [ Html.text "Tip: " ]
                , Html.text "Students do their best work when assignments are released in small batches."
                ]
            ]
        ]


pillowExampleContent : Container.Attribute msg
pillowExampleContent =
    contentStack
        [ Heading.h2
            [ Heading.plaintext "Quick Write"
            , Heading.css [ Css.margin Css.zero ]
            ]
        , Text.mediumBody
            [ flushParagraphMargin
            , Text.plaintext "Describe a tradition that matters to your family. Use vivid details so your reader can picture the scene."
            ]
        ]


buttonyExampleContent : Container.Attribute msg
buttonyExampleContent =
    Container.html
        [ Html.div
            [ css
                [ Css.displayFlex
                , Css.alignItems Css.center
                , Css.property "gap" "16px"
                ]
            ]
            [ Html.div
                [ css
                    [ Css.flexShrink Css.zero
                    , Css.width (Css.px 44)
                    , Css.height (Css.px 44)
                    , Css.borderRadius (Css.pct 50)
                    , Css.backgroundColor Colors.frost
                    , Css.color Colors.azure
                    , Css.displayFlex
                    , Css.alignItems Css.center
                    , Css.justifyContent Css.center
                    , Css.fontSize (Css.px 18)
                    , Fonts.baseFont
                    ]
                ]
                [ Html.text "▶" ]
            , Html.div
                [ css
                    [ Css.displayFlex
                    , Css.flexDirection Css.column
                    , Css.property "gap" "2px"
                    ]
                ]
                [ Html.div
                    [ css
                        [ Fonts.baseFont
                        , Css.fontWeight Css.bold
                        , Css.color Colors.navy
                        , Css.fontSize (Css.px 16)
                        ]
                    ]
                    [ Html.text "Start practice: Comma splices" ]
                , Text.caption
                    [ flushParagraphMargin
                    , Text.plaintext "About 10 minutes · Diagnostic"
                    ]
                ]
            ]
        ]


disabledExampleContent : Container.Attribute msg
disabledExampleContent =
    contentStack
        [ contentTitle "Unit 5: Argument Writing"
        , Text.smallBody
            [ flushParagraphMargin
            , Text.plaintext "🔒 Complete Unit 4 to unlock this lesson."
            ]
        ]


viewExampleCode : List String -> String
viewExampleCode attributes =
    Code.fromModule moduleName "view"
        ++ Code.listMultiline attributes 1


{-| -}
type alias State =
    { control : Control (List ( String, Container.Attribute Msg ))
    }


{-| -}
init : State
init =
    { control =
        Control.list
            |> ControlExtra.listItems "Content"
                (Control.list
                    |> ControlExtra.optionalListItem "content" controlContent
                )
            |> ControlExtra.listItems "CSS & Style options"
                (Control.list
                    |> ControlExtra.optionalListItem "paddingPx" controlPaddingPx
                    |> CommonControls.css { moduleName = moduleName, use = Container.css }
                    |> CommonControls.mobileCss { moduleName = moduleName, use = Container.mobileCss }
                    |> CommonControls.quizEngineMobileCss { moduleName = moduleName, use = Container.quizEngineMobileCss }
                    |> CommonControls.notMobileCss { moduleName = moduleName, use = Container.notMobileCss }
                )
    }


controlPaddingPx : Control ( String, Container.Attribute msg )
controlPaddingPx =
    Control.map
        (\val ->
            ( "Container.paddingPx " ++ String.fromFloat val
            , Container.paddingPx val
            )
        )
        (Control.float 20)


controlContent : Control ( String, Container.Attribute msg )
controlContent =
    CommonControls.content
        { moduleName = "Container"
        , paragraph = Just Container.paragraph
        , plaintext = Container.plaintext
        , markdown = Just Container.markdown
        , html = Container.html
        , httpError = Nothing
        }


{-| -}
type Msg
    = UpdateControl (Control (List ( String, Container.Attribute Msg )))


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        UpdateControl newControl ->
            ( { state | control = newControl }, Cmd.none )
