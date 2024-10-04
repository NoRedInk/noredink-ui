module Examples.LoadingShimmer exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Accessibility.Styled.Key as Key
import Category exposing (Category(..))
import Code
import CommonControls
import Css exposing (middle, verticalAlign)
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import EllieLink
import Example exposing (Example)
import Guidance
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attributes exposing (css)
import Nri.Ui.ClickableText.V4 as ClickableText
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.LoadingShimmer.V1 as LoadingShimmer
import Nri.Ui.Spacing.V1 as Spacing
import Nri.Ui.Text.V6 as Text
import Nri.Ui.Tooltip.V3 as Tooltip
import Nri.Ui.UiIcon.V1 as UiIcon
import Set exposing (Set)


{-| -}
version : Int
version =
    1


{-| -}
example : Example State Msg
example =
    { name = moduleName
    , version = version
    , init = ( init, Cmd.none )
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview =
        [ LoadingShimmer.view
            [ LoadingShimmer.line
            ]
        ]
    , about =
        [ Guidance.helpfullyDisabled moduleName
        , Text.smallBody
            [ Text.html
                [ text "Which LoadingShimmer type should you use?"
                ]
            ]
        , ul [ css [ Css.paddingLeft (Css.px 42), Css.margin Css.zero ] ]
            [ li []
                [ Text.smallBody
                    [ Text.html
                        [ strong [] [ text "Link type:" ]
                        , text " When the button takes the user to a URL, whether this results in a new page load or whether the URL is a SPA route. (This allows users to do things like copy the URL, open the link in a new tab, etc.  Please use the "
                        , ClickableText.link "Assistive technology notification design & development guide"
                            [ ClickableText.linkExternal "https://noredinkaccessibility.screenstepslive.com/a/1651037-assistive-technology-notification-design-development-guide"
                            , ClickableText.appearsInline
                            ]
                        , text " to ensure you're managing the user's focus properly within a SPA.)"
                        ]
                    ]
                ]
            , li [] [ Text.smallBody [ Text.markdown "**LoadingShimmer type:** When the button performs an action on the same page, or when the button submits a form, even if the form submission ultimately directs the user to a new URL." ] ]
            ]
        , Guidance.useRadioButtonDotless
        ]
    , view = \ellieLinkConfig state -> [ viewLoadingShimmerExamples ellieLinkConfig state ]
    , categories = [ Buttons ]
    , keyboardSupport = []
    }


moduleName : String
moduleName =
    "LoadingShimmer"


{-| -}
type alias State =
    { control : Control (List ( String, LoadingShimmer.Attribute Msg ))
    }


{-| -}
init : State
init =
    { control = controlAttributes
    }


{-| -}
type Msg
    = UpdateControl (Control (List ( String, LoadingShimmer.Attribute Msg )))



{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        UpdateControl newControl ->
            ( { state | control = newControl }, Cmd.none )




-- INTERNAL


type alias Model =
    { attributes : List ( String, LoadingShimmer.Attribute Msg )
    }


controlAttributes : Control (List ( String, LoadingShimmer.Attribute Msg ))
controlAttributes =
    Control.list
        -- |> ControlExtra.listItems "Size & Width"
        --     (Control.list
        --         |> ControlExtra.optionalListItem "width"
        --             (CommonControls.choice moduleName
        --                 [ ( "exactWidth 120", LoadingShimmer.exactWidth 120 )
        --                 , ( "exactWidth 70", LoadingShimmer.exactWidth 70 )
        --                 , ( "boundedWidth 100 180", LoadingShimmer.boundedWidth { min = 100, max = 180 } )
        --                 , ( "unboundedWidth", LoadingShimmer.unboundedWidth )
        --                 , ( "fillContainerWidth", LoadingShimmer.fillContainerWidth )
        --                 ]
        --             )
        --         |> ControlExtra.optionalListItem "mobile width"
        --             (CommonControls.choice moduleName
        --                 [ ( "exactWidthForMobile 120", LoadingShimmer.exactWidthForMobile 120 )
        --                 , ( "exactWidthForMobile 70", LoadingShimmer.exactWidthForMobile 70 )
        --                 , ( "boundedWidthForMobile 100 180", LoadingShimmer.boundedWidthForMobile { min = 100, max = 180 } )
        --                 , ( "unboundedWidthForMobile", LoadingShimmer.unboundedWidthForMobile )
        --                 , ( "fillContainerWidthForMobile", LoadingShimmer.fillContainerWidthForMobile )
        --                 ]
        --             )
        --         |> ControlExtra.optionalListItem "quiz engine mobile width"
        --             (CommonControls.choice moduleName
        --                 [ ( "exactWidthForQuizEngineMobile 120", LoadingShimmer.exactWidthForQuizEngineMobile 120 )
        --                 , ( "exactWidthForQuizEngineMobile 70", LoadingShimmer.exactWidthForQuizEngineMobile 70 )
        --                 , ( "boundedWidthForQuizEngineMobile 100 180", LoadingShimmer.boundedWidthForQuizEngineMobile { min = 100, max = 180 } )
        --                 , ( "unboundedWidthForQuizEngineMobile", LoadingShimmer.unboundedWidthForQuizEngineMobile )
        --                 , ( "fillContainerWidthForQuizEngineMobile", LoadingShimmer.fillContainerWidthForQuizEngineMobile )
        --                 ]
        --             )
        --         |> ControlExtra.optionalListItem "narrow mobile width"
        --             (CommonControls.choice moduleName
        --                 [ ( "exactWidthForNarrowMobile 120", LoadingShimmer.exactWidthForNarrowMobile 120 )
        --                 , ( "exactWidthForNarrowMobile 70", LoadingShimmer.exactWidthForNarrowMobile 70 )
        --                 , ( "boundedWidthForNarrowMobile 100 180", LoadingShimmer.boundedWidthForNarrowMobile { min = 100, max = 180 } )
        --                 , ( "unboundedWidthForNarrowMobile", LoadingShimmer.unboundedWidthForNarrowMobile )
        --                 , ( "fillContainerWidthForNarrowMobile", LoadingShimmer.fillContainerWidthForNarrowMobile )
        --                 ]
        --             )
        --     )
        |> ControlExtra.listItems "Type"
            (Control.list
                |> ControlExtra.listItem "kind"
                    (CommonControls.choice moduleName
                        [ ( "line", LoadingShimmer.line )
                        ]
                    )
            )
        |> ControlExtra.listItems "CSS"
            (Control.list
                |> CommonControls.css
                    { moduleName = moduleName
                    , use = LoadingShimmer.css
                    }
             -- |> CommonControls.mobileCss
             --     { moduleName = moduleName
             --     , use = LoadingShimmer.mobileCss
             --     }
             -- |> CommonControls.quizEngineMobileCss
             --     { moduleName = moduleName
             --     , use = LoadingShimmer.quizEngineMobileCss
             --     }
             -- |> CommonControls.notMobileCss
             --     { moduleName = moduleName
             --     , use = LoadingShimmer.notMobileCss
             --     }
            )


viewLoadingShimmerExamples : EllieLink.Config -> State -> Html Msg
viewLoadingShimmerExamples ellieLinkConfig state =
    let
        model =
            Control.currentValue state.control
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
                [ toExampleCode "view"
                ]
        }
    , Heading.h2
        [ Heading.plaintext "Customizable example"
        , Heading.css
            [ Css.marginTop Spacing.verticalSpacerPx
            , Css.marginBottom (Css.px 10)
            ]
        ]
    , Heading.h2
        [ Heading.plaintext "Non-interactive examples"
        , Heading.css
            [ Css.marginTop Spacing.verticalSpacerPx
            , Css.marginBottom (Css.px 10)
            ]
        ]
    , loadingShimmerTable
    ]
        |> div []


widths : List ( String, LoadingShimmer.Attribute Msg )
widths =
    [ --      ( "exactWidth 120", LoadingShimmer.exactWidth 120 )
      -- , ( "exactWidth 70", LoadingShimmer.exactWidth 70 )
      -- , ( "boundedWidth 100 180", LoadingShimmer.boundedWidth { min = 100, max = 180 } )
      ( "fillContainerWidth", LoadingShimmer.fillContainerWidth )
    ]


loadingShimmerTable : Html Msg
loadingShimmerTable =
    let
        kinds =
            [ ( LoadingShimmer.line, "line" )
            ]

        exampleRow kindTuple =
            [ tr []
                (td
                    [ css [ verticalAlign middle, Css.borderTop3 (Css.px 1) Css.solid Colors.gray85 ]
                    , Attributes.rowspan 2
                    ]
                    [ code [] [ text (Code.fromModule moduleName (Tuple.second kindTuple)) ] ]
                    :: List.map
                        (exampleCell
                            [ Css.borderTop3 (Css.px 1) Css.solid Colors.gray85
                            , Css.paddingBottom Css.zero |> Css.important
                            ]
                            kindTuple
                        )
                        widths
                    ++ [ td [ css [ verticalAlign middle, Css.borderTop3 (Css.px 1) Css.solid Colors.gray85 ] ]
                            [ code [] [ text "LoadingShimmer.line" ] ]
                       ]
                )
            ]

        exampleCell cellStyle ( kind, kindName ) ( widthName, setWidth ) =
            inCell cellStyle <| LoadingShimmer.view [ setWidth, kind ]

        inCell style content =
            td
                [ css
                    [ verticalAlign middle
                    , Css.batch style
                    , Css.padding (Css.px 10)
                    ]
                ]
                [ content ]
    in
    List.concatMap exampleRow kinds
        |> table [ css [ Css.borderCollapse Css.collapse, Css.width (Css.pct 100) ] ]
        |> List.singleton
        |> div [ css [ Css.overflow Css.auto ] ]
