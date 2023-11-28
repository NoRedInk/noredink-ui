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
import Nri.Ui.AssignmentIcon.V2 as AssignmentIcon
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Container.V2 as Container
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Spacing.V1 as Spacing
import Nri.Ui.Svg.V1 as Svg exposing (Svg)
import Nri.Ui.UiIcon.V1 as UiIcon


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
    , state = init
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
            , viewExample
                { name = "Default Container"
                , description = "Your go-to container."
                }
                (Container.default :: attributes)
            , viewExample
                { name = "Gray Container"
                , description = "A container that doesn’t draw too much attention to itself."
                }
                (Container.gray :: attributes)
            , viewExample
                { name = "Pillow Container"
                , description = "When you want something big and soft."
                }
                (Container.pillow :: attributes)
            , viewExample
                { name = "Buttony Container"
                , description = "Used for clickable button card things."
                }
                (Container.buttony :: attributes)
            , viewExample
                { name = "Disabled Container"
                , description = "Used to indicate content is locked/inaccessible"
                }
                (Container.disabled :: attributes)
            ]
    }


viewExample : { name : String, description : String } -> List (Container.Attribute msg) -> Html msg
viewExample { name, description } attributes =
    Html.section
        [ css
            [ Css.marginTop (Css.px 20)
            ]
        ]
        [ Heading.h3 [ Heading.plaintext name ]
        , Html.text description
        , Container.view attributes
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
        ControlExtra.list
            |> ControlExtra.optionalListItem "paddingPx" controlPaddingPx
            |> CommonControls.css { moduleName = moduleName, use = Container.css }
            |> CommonControls.mobileCss { moduleName = moduleName, use = Container.mobileCss }
            |> CommonControls.quizEngineMobileCss { moduleName = moduleName, use = Container.quizEngineMobileCss }
            |> CommonControls.notMobileCss { moduleName = moduleName, use = Container.notMobileCss }
            |> ControlExtra.listItem "content" controlContent
            |> ControlExtra.optionalListItem "topLeftIcon" iconOptions
            |> ControlExtra.optionalBoolListItem "hideIconShadow" ( "Container.hideIconShadow", Container.hideIconShadow )
    }


controlPaddingPx : Control ( String, Container.Attribute msg )
controlPaddingPx =
    Control.map
        (\val ->
            ( "Container.paddingPx " ++ String.fromFloat val
            , Container.paddingPx val
            )
        )
        (ControlExtra.float 20)


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


iconOptions : Control ( String, Container.Attribute msg )
iconOptions =
    let
        goodOptions =
            [ ( "AssignmentIcon.quickWriteCircled", AssignmentIcon.quickWriteCircled )
            , ( "AssignmentIcon.unitDiagnosticCircled", AssignmentIcon.unitDiagnosticCircled )
            , ( "AssignmentIcon.practiceCircled", AssignmentIcon.practiceCircled )
            , ( "AssignmentIcon.passageQuizCircled", AssignmentIcon.passageQuizCircled )
            , ( "AssignmentIcon.planningDiagnosticCircled", AssignmentIcon.planningDiagnosticCircled )
            , ( "AssignmentIcon.quizCircled", AssignmentIcon.quizCircled )
            , ( "AssignmentIcon.guidedDraftCircled", AssignmentIcon.guidedDraftCircled )
            , ( "AssignmentIcon.selfReviewCircled", AssignmentIcon.selfReviewCircled )
            , ( "AssignmentIcon.peerReviewCircled", AssignmentIcon.peerReviewCircled )
            , ( "AssignmentIcon.gradingAssistantCircled", AssignmentIcon.gradingAssistantCircled )
            , ( "AssignmentIcon.dailyWritingCircled", AssignmentIcon.dailyWritingCircled )
            , ( "UiIcon.activity", UiIcon.activity )
            , ( "UiIcon.homeInCircle", UiIcon.homeInCircle )
            , ( "UiIcon.searchInCicle", UiIcon.searchInCicle )
            , ( "UiIcon.apple", UiIcon.apple )
            , ( "UiIcon.globe", UiIcon.globe )
            , ( "UiIcon.heapOfBoxes", UiIcon.heapOfBoxes )
            , ( "UiIcon.documents", UiIcon.documents )
            ]

        badOptions =
            [ ( "UiIcon.seeMore", UiIcon.seeMore )
            , ( "UiIcon.openClose", UiIcon.openClose )
            , ( "UiIcon.graduateCap", UiIcon.graduateCap )
            , ( "UiIcon.preview", UiIcon.preview )
            , ( "UiIcon.download", UiIcon.download )
            , ( "UiIcon.gift", UiIcon.gift )
            , ( "UiIcon.print", UiIcon.print )
            , ( "UiIcon.baldBulb", UiIcon.baldBulb )
            , ( "UiIcon.bulb", UiIcon.bulb )
            , ( "UiIcon.brain", UiIcon.brain )
            , ( "UiIcon.sort", UiIcon.sort )
            , ( "UiIcon.gear", UiIcon.gear )
            , ( "UiIcon.flipper", UiIcon.flipper )
            , ( "UiIcon.hamburger", UiIcon.hamburger )
            , ( "UiIcon.kebab", UiIcon.kebab )
            , ( "UiIcon.archive", UiIcon.archive )
            , ( "UiIcon.unarchive", UiIcon.unarchive )
            , ( "UiIcon.playInCircle", UiIcon.playInCircle )
            , ( "UiIcon.pauseInCircle", UiIcon.pauseInCircle )
            , ( "UiIcon.stopInCircle", UiIcon.stopInCircle )
            , ( "UiIcon.speaker", UiIcon.speaker )
            , ( "UiIcon.mutedSpeaker", UiIcon.mutedSpeaker )
            , ( "UiIcon.play", UiIcon.play )
            , ( "UiIcon.skip", UiIcon.skip )
            , ( "UiIcon.share", UiIcon.share )
            , ( "UiIcon.copyToClipboard", UiIcon.copyToClipboard )
            , ( "UiIcon.footsteps", UiIcon.footsteps )
            , ( "UiIcon.footstepsVideo", UiIcon.footstepsVideo )
            , ( "UiIcon.compass", UiIcon.compass )
            , ( "UiIcon.speedometer", UiIcon.speedometer )
            , ( "UiIcon.help", UiIcon.help )
            , ( "UiIcon.checklist", UiIcon.checklist )
            , ( "UiIcon.checklistComplete", UiIcon.checklistComplete )
            , ( "UiIcon.sparkleBulb", UiIcon.sparkleBulb )
            , ( "UiIcon.hat", UiIcon.hat )
            , ( "UiIcon.keychain", UiIcon.keychain )
            , ( "UiIcon.sprout", UiIcon.sprout )
            , ( "UiIcon.sapling", UiIcon.sapling )
            , ( "UiIcon.tree", UiIcon.tree )
            , ( "UiIcon.person", UiIcon.person )
            , ( "UiIcon.couple", UiIcon.couple )
            , ( "UiIcon.class", UiIcon.class )
            , ( "UiIcon.leaderboard", UiIcon.leaderboard )
            , ( "UiIcon.performance", UiIcon.performance )
            , ( "UiIcon.emptyCalendar", UiIcon.emptyCalendar )
            , ( "UiIcon.calendar", UiIcon.calendar )
            , ( "UiIcon.clock", UiIcon.clock )
            , ( "UiIcon.missingDocument", UiIcon.missingDocument )
            , ( "UiIcon.document", UiIcon.document )
            , ( "UiIcon.newspaper", UiIcon.newspaper )
            , ( "UiIcon.openBook", UiIcon.openBook )
            , ( "UiIcon.openBooks", UiIcon.openBooks )
            , ( "UiIcon.edit", UiIcon.edit )
            , ( "UiIcon.pen", UiIcon.pen )
            , ( "UiIcon.highlighter", UiIcon.highlighter )
            , ( "UiIcon.eraser", UiIcon.eraser )
            , ( "UiIcon.speechBalloon", UiIcon.speechBalloon )
            , ( "UiIcon.mail", UiIcon.mail )
            , ( "UiIcon.arrowTop", UiIcon.arrowTop )
            , ( "UiIcon.arrowRight", UiIcon.arrowRight )
            , ( "UiIcon.arrowDown", UiIcon.arrowDown )
            , ( "UiIcon.arrowLeft", UiIcon.arrowLeft )
            , ( "UiIcon.arrowPointingRight", UiIcon.arrowPointingRight )
            , ( "UiIcon.arrowPointingRightThick", UiIcon.arrowPointingRightThick )
            , ( "UiIcon.sortArrow", UiIcon.sortArrow )
            , ( "UiIcon.sortArrowDown", UiIcon.sortArrowDown )
            , ( "UiIcon.checkmark", UiIcon.checkmark )
            , ( "UiIcon.checkmarkInCircle", UiIcon.checkmarkInCircle )
            , ( "UiIcon.checkmarkInCircleInverse", UiIcon.checkmarkInCircleInverse )
            , ( "UiIcon.emptyCircle", UiIcon.emptyCircle )
            , ( "UiIcon.x", UiIcon.x )
            , ( "UiIcon.xInCircle", UiIcon.xInCircle )
            , ( "UiIcon.attention", UiIcon.attention )
            , ( "UiIcon.exclamation", UiIcon.exclamation )
            , ( "UiIcon.flag", UiIcon.flag )
            , ( "UiIcon.star", UiIcon.star )
            , ( "UiIcon.starFilled", UiIcon.starFilled )
            , ( "UiIcon.starOutline", UiIcon.starOutline )
            , ( "UiIcon.equals", UiIcon.equals )
            , ( "UiIcon.plus", UiIcon.plus )
            , ( "UiIcon.null", UiIcon.null )
            , ( "UiIcon.key", UiIcon.key )
            , ( "UiIcon.lock", UiIcon.lock )
            , ( "UiIcon.premiumLock", UiIcon.premiumLock )
            , ( "UiIcon.badge", UiIcon.badge )
            , ( "UiIcon.tada", UiIcon.tada )
            , ( "UiIcon.count", UiIcon.count )
            , ( "UiIcon.bold", UiIcon.bold )
            , ( "UiIcon.italic", UiIcon.italic )
            , ( "UiIcon.underline", UiIcon.underline )
            , ( "UiIcon.list", UiIcon.list )
            , ( "UiIcon.link", UiIcon.link )
            , ( "UiIcon.undo", UiIcon.undo )
            , ( "UiIcon.redo", UiIcon.redo )
            , ( "UiIcon.home", UiIcon.home )
            , ( "UiIcon.library", UiIcon.library )
            , ( "UiIcon.search", UiIcon.search )
            , ( "UiIcon.openQuotationMark", UiIcon.openQuotationMark )
            , ( "UiIcon.closeQuotationMark", UiIcon.closeQuotationMark )
            , ( "UiIcon.microscope", UiIcon.microscope )
            , ( "UiIcon.scale", UiIcon.scale )
            , ( "UiIcon.openInNewTab", UiIcon.openInNewTab )
            , ( "UiIcon.sync", UiIcon.sync )
            , ( "UiIcon.appleOutline", UiIcon.appleOutline )
            , ( "UiIcon.briefcase", UiIcon.briefcase )
            , ( "UiIcon.school", UiIcon.school )
            , ( "UiIcon.highSchool", UiIcon.highSchool )
            , ( "UiIcon.company", UiIcon.company )
            , ( "UiIcon.homeSchool", UiIcon.homeSchool )
            , ( "UiIcon.flagUs", UiIcon.flagUs )
            , ( "UiIcon.info", UiIcon.info )
            , ( "UiIcon.projectorScreen", UiIcon.projectorScreen )
            , ( "UiIcon.stretch", UiIcon.stretch )
            , ( "UiIcon.climbingStairs", UiIcon.climbingStairs )
            , ( "UiIcon.twoHandsHoldingBox", UiIcon.twoHandsHoldingBox )
            , ( "UiIcon.atlas", UiIcon.atlas )
            , ( "UiIcon.gradingAssistant", UiIcon.gradingAssistant )
            , ( "UiIcon.manuallyGraded", UiIcon.manuallyGraded )
            ]
    in
    ((goodOptions
        ++ List.map
            (Tuple.mapFirst (\name -> name ++ " -- not circular—hide the shadow if you must use"))
            badOptions
     )
        |> List.map
            (\( name, svg ) ->
                ( name
                , Control.value
                    ( name
                        ++ Code.newlineWithIndent 2
                        ++ "|> Svg.withColor"
                        ++ Code.newlineWithIndent 3
                        ++ "Colors.highlightPurpleDark"
                        ++ Code.newlineWithIndent 2
                        ++ "|> Container.topLeftIcon"
                    , svg
                        |> Svg.withColor Colors.highlightPurpleDark
                        |> Container.topLeftIcon
                    )
                )
            )
    )
        |> Control.choice
