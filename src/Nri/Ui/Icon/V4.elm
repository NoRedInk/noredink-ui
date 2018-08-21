module Nri.Ui.Icon.V4
    exposing
        ( IconLinkSpaModel
        , IconSize(..)
        , IconType
        , activity
        , add
        , arrowDown
        , arrowLeft
        , arrowRight
        , assignmentStartButtonPrimary
        , assignmentStartButtonSecondary
        , assignmentTypeDiagnostic
        , assignmentTypePractice
        , assignmentTypeQuiz
        , assignmentTypeWritingCycle
        , attention
        , bang
        , bulb
        , button
        , calendar
        , caret
        , checkMark
        , checkMarkSquiggily
        , checkMarkSvg
        , class
        , clever
        , clock
        , close
        , compassSvg
        , copy
        , customImage
        , customSvg
        , customSvgLink
        , darkBlueCheckMark
        , decorativeIcon
        , document
        , download
        , edit
        , editWriting
        , equalitySign
        , exclamation
        , facebook
        , flag
        , flipper
        , footsteps
        , gardening
        , gear
        , greenCheckMark
        , guidedWrite
        , help
        , highFive
        , icon
        , key
        , late
        , leaderboard
        , lightBulb
        , link
        , linkExternal
        , linkSpa
        , lock
        , lockDeprecated
        , logo
        , masteryBadge
        , newspaper
        , notStarred
        , okay
        , openClose
        , peerReview
        , performance
        , personBlue
        , preview
        , quickWrite
        , rating
        , revising
        , seeMore
        , share
        , skip
        , sort
        , sortArrow
        , speedometer
        , starred
        , submitting
        , thumbsUp
        , twitter
        , unarchive
        , writingAssignment
        , x
        , xSvg
        )

{-|

@docs icon, decorativeIcon, link, linkExternal, linkSpa, button
@docs IconType, IconSize, IconLinkSpaModel
@docs activity
@docs add
@docs arrowDown
@docs arrowLeft
@docs arrowRight
@docs assignmentStartButtonPrimary
@docs assignmentStartButtonSecondary
@docs assignmentTypeDiagnostic
@docs assignmentTypePractice
@docs assignmentTypeQuiz
@docs assignmentTypeWritingCycle
@docs attention
@docs bang
@docs bulb
@docs calendar
@docs caret
@docs checkMark
@docs checkMarkSquiggily
@docs checkMarkSvg
@docs class
@docs clever
@docs clock
@docs close
@docs copy
@docs compassSvg
@docs customImage
@docs customSvg
@docs customSvgLink
@docs darkBlueCheckMark
@docs document
@docs download
@docs edit
@docs editWriting
@docs equalitySign
@docs exclamation
@docs facebook
@docs flag
@docs flipper
@docs footsteps
@docs gardening
@docs gear
@docs greenCheckMark
@docs guidedWrite
@docs help
@docs highFive
@docs key
@docs late
@docs leaderboard
@docs lightBulb
@docs lock
@docs lockDeprecated
@docs logo
@docs masteryBadge
@docs newspaper
@docs notStarred
@docs okay
@docs openClose
@docs peerReview
@docs performance
@docs personBlue
@docs preview
@docs quickWrite
@docs seeMore
@docs share
@docs skip
@docs sort
@docs sortArrow
@docs speedometer
@docs starred
@docs thumbsUp
@docs twitter
@docs unarchive
@docs writingAssignment
@docs x
@docs xSvg
@docs submitting, rating, revising

-}

import Accessibility.Role as Role
import Accessibility.Styled exposing (..)
import Css exposing (..)
import EventExtras
import Html as RootHtml
import Html.Attributes as RootAttr exposing (..)
import Html.Styled
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events as Events
import Nri.Ui.AssetPath exposing (Asset(..))
import Nri.Ui.Assets as Assets
import Nri.Ui.Colors.V1
import Svg exposing (svg, use)
import Svg.Attributes exposing (xlinkHref)


{-| -}
type alias IconLinkModel msg =
    { alt : String
    , url : String
    , icon : IconType msg
    , disabled : Bool
    , size : IconSize
    }


{-| -}
type alias IconLinkSpaModel route msg =
    { alt : String
    , icon : IconType msg
    , disabled : Bool
    , size : IconSize
    , route : route
    }


type alias IconButtonModel msg =
    { alt : String
    , msg : msg
    , icon : IconType msg
    , disabled : Bool
    , size : IconSize
    }


{-| An icon that can be rendered using the functions provided by this module.
-}
type IconType msg
    = ImgIcon Asset
    | SvgIcon (List (Svg.Attribute msg) -> List (Svg.Svg msg) -> RootHtml.Html msg)
    | SvgLink String


{-| Used for determining sizes on Icon.buttons and Icon.links
-}
type IconSize
    = Small
    | Medium


{-| Create an icon that links to a part of NRI
Uses our default icon styles (25 x 25 px, azure)
-}
link : IconLinkModel msg -> Html msg
link =
    linkBase [ Attributes.target "_self" ]


{-| Create an accessible icon button with an onClick handler
Uses our default icon styles (25 x 25 px, azure)
-}
button : IconButtonModel msg -> Html msg
button model =
    Accessibility.Styled.button
        [ css
            [ Css.batch
                [ backgroundColor transparent
                , border zero
                , color Nri.Ui.Colors.V1.azure
                , fontFamily inherit
                , Css.property "cursor" "pointer"
                , padding zero
                , focus
                    [ backgroundColor transparent
                    ]
                ]
            , sizeStyles model.size
            ]
        , Events.onClick model.msg
        , Attributes.disabled model.disabled
        , Attributes.type_ "button"
        ]
        [ icon
            { alt = model.alt
            , icon = model.icon
            }
        ]


{-| -}
icon : { alt : String, icon : IconType msg } -> Html msg
icon config =
    case config.icon of
        SvgLink iconId ->
            svg [ svgStyle ]
                [ Svg.title [] [ RootHtml.text config.alt ]
                , use [ xlinkHref ("#" ++ iconId) ] []
                ]
                |> Html.Styled.fromUnstyled

        SvgIcon svgElement ->
            svgElement [ svgStyle ] [ Svg.title [] [ RootHtml.text config.alt ] ]
                |> Html.Styled.fromUnstyled

        ImgIcon assetPath ->
            img config.alt
                [ Attributes.src (Nri.Ui.AssetPath.url assetPath)
                ]


{-| Use this icon for purely decorative content that would be distracting
rather than helpful on a screenreader.
-}
decorativeIcon : IconType msg -> Html msg
decorativeIcon iconType =
    case iconType of
        SvgLink iconId ->
            svg
                [ svgStyle
                , Role.img
                ]
                [ use [ xlinkHref ("#" ++ iconId) ] []
                ]
                |> Html.Styled.fromUnstyled

        SvgIcon svgElement ->
            svgElement [ svgStyle, Role.img ] []
                |> Html.Styled.fromUnstyled

        ImgIcon assetPath ->
            decorativeImg [ Attributes.src (Nri.Ui.AssetPath.url assetPath) ]


{-| Use this link for routing within a single page app.

This will make a normal <a> tag, but change the onClick behavior to avoid reloading the page.

-}
linkSpa : (route -> String) -> (route -> msg) -> IconLinkSpaModel route msg -> Html msg
linkSpa toUrl toMsg config =
    linkBase
        [ EventExtras.onClickPreventDefaultForLinkWithHref (toMsg config.route)
            |> Attributes.fromUnstyled
        ]
        { alt = config.alt
        , url = toUrl config.route
        , icon = config.icon
        , disabled = config.disabled
        , size = config.size
        }


{-| Create an icon that links to an external site
Uses our default icon styles (25 x 25 px, azure)
-}
linkExternal : IconLinkModel msg -> Html msg
linkExternal =
    linkBase [ Attributes.target "_blank" ]


linkBase : List (Attribute msg) -> IconLinkModel msg -> Html msg
linkBase linkAttributes model =
    span
        []
        [ Html.Styled.a
            (linkAttributes ++ defaultLinkAttributes model)
            [ icon { alt = model.alt, icon = model.icon }
            ]
        ]


defaultLinkAttributes : IconLinkModel msg -> List (Attribute msg)
defaultLinkAttributes model =
    if model.disabled then
        [ css
            [ Css.cursor Css.notAllowed
            , linkStyles
            , sizeStyles model.size
            ]
        ]
    else
        [ css [ linkStyles, sizeStyles model.size ]
        , Attributes.href model.url
        ]


linkStyles : Style
linkStyles =
    Css.batch
        [ color Nri.Ui.Colors.V1.azure
        , display inlineBlock
        , fontFamily inherit
        , Css.property "cursor" "pointer"
        , padding zero
        , visited [ color Nri.Ui.Colors.V1.azure ]
        ]


sizeStyles : IconSize -> Style
sizeStyles size =
    Css.batch <|
        case size of
            Small ->
                [ Css.width (px 20)
                , Css.height (px 20)
                ]

            Medium ->
                [ Css.width (px 25)
                , Css.height (px 25)
                ]


{-| -}
activity : { r | activity : String } -> IconType msg
activity assets =
    SvgLink assets.activity


{-| -}
add : { r | icons_plusBlue_svg : Asset } -> IconType msg
add assets =
    SvgIcon Assets.plusBlueSvg


{-| -}
arrowDown : { r | arrowDown : String } -> IconType msg
arrowDown assets =
    SvgIcon Assets.arrowDownBlueSvg


{-| -}
arrowLeft : { r | leftArrowBlue_png : Asset } -> IconType msg
arrowLeft assets =
    SvgIcon Assets.arrowLeftBlueSvg


{-| -}
arrowRight : { r | icons_arrowRightBlue_svg : Asset } -> IconType msg
arrowRight assets =
    SvgIcon Assets.arrowRightBlueSvg


{-| -}
assignmentStartButtonPrimary : { r | assignmentStartButtonPrimary_svg : Asset } -> IconType msg
assignmentStartButtonPrimary assets =
    SvgIcon Assets.assignmentStartButtonPrimarySvg


{-| -}
assignmentStartButtonSecondary : { r | assignmentStartButtonSecondary_svg : Asset } -> IconType msg
assignmentStartButtonSecondary assets =
    SvgIcon Assets.assignmentStartButtonSecondarySvg


{-| -}
assignmentTypeDiagnostic : { r | diagnostic : String } -> IconType msg
assignmentTypeDiagnostic assets =
    SvgLink assets.diagnostic


{-| -}
assignmentTypePractice : { r | practice : String } -> IconType msg
assignmentTypePractice assets =
    SvgLink assets.practice


{-| -}
assignmentTypeQuiz : { r | quiz : String } -> IconType msg
assignmentTypeQuiz assets =
    SvgLink assets.quiz


{-| -}
assignmentTypeWritingCycle : { r | writingcycle : String } -> IconType msg
assignmentTypeWritingCycle assets =
    SvgLink assets.writingcycle


{-| -}
attention : { r | attention_svg : Asset } -> IconType msg
attention assets =
    SvgIcon Assets.attentionSvg


{-| -}
bang : { r | exclamationPoint_svg : Asset } -> IconType msg
bang assets =
    SvgIcon Assets.exclamationPointSvg


{-| -}
bulb : { r | bulb : String } -> IconType msg
bulb assets =
    SvgIcon Assets.bulbYellowSvg


{-| -}
calendar : { r | calendar : String } -> IconType msg
calendar assets =
    SvgIcon Assets.iconCalendarSvg


{-| -}
caret : { r | icons_arrowDownBlue_svg : Asset } -> IconType msg
caret assets =
    SvgIcon Assets.arrowDownBlueSvg


{-| -}
checkMark : { r | iconCheck_png : Asset } -> IconType msg
checkMark assets =
    SvgIcon Assets.checkWhiteSvg


{-| -}
checkMarkSquiggily : { r | squiggly_png : Asset } -> IconType msg
checkMarkSquiggily assets =
    SvgIcon Assets.squigglySvg


{-| -}
checkMarkSvg : { r | checkmark : String } -> IconType msg
checkMarkSvg assets =
    SvgLink assets.checkmark


{-| -}
class : { r | class : String } -> IconType msg
class assets =
    SvgLink assets.class


{-| -}
clever : { r | clever : String } -> IconType msg
clever assets =
    SvgLink assets.clever


{-| -}
clock : { r | clock : String } -> IconType msg
clock assets =
    SvgLink assets.clock


{-| -}
close : { r | icons_xBlue_svg : Asset } -> IconType msg
close assets =
    SvgIcon Assets.xBlueSvg


{-| -}
copy : { r | teach_assignments_copyWhite_svg : Asset } -> IconType msg
copy assets =
    SvgIcon Assets.copyWhiteSvg


{-| -}
compassSvg : { r | compass : String } -> IconType msg
compassSvg assets =
    SvgLink assets.compass


{-| -}
customSvg : (List (Svg.Attribute msg) -> List (Svg.Svg msg) -> RootHtml.Html msg) -> IconType msg
customSvg asset =
    SvgIcon asset


{-| -}
customSvgLink : String -> IconType msg
customSvgLink asset =
    SvgLink asset


{-| -}
customImage : Asset -> IconType msg
customImage asset =
    ImgIcon asset


{-| -}
darkBlueCheckMark : { r | darkBlueCheckmark_svg : Asset } -> IconType msg
darkBlueCheckMark assets =
    SvgIcon Assets.darkBlueCheckmarkSvg


{-| -}
document : { r | document : String } -> IconType msg
document assets =
    SvgLink assets.document


{-| -}
download : { r | download : String } -> IconType msg
download assets =
    SvgLink assets.download


{-| -}
edit : { r | edit : String } -> IconType msg
edit assets =
    SvgLink assets.edit


{-| -}
editWriting : { r | editWriting : String } -> IconType msg
editWriting assets =
    SvgLink assets.editWriting


{-| -}
equalitySign : { r | icons_equals_svg : Asset } -> IconType msg
equalitySign assets =
    SvgIcon Assets.equalsSvg


{-| -}
exclamation : { r | exclamation : String } -> IconType msg
exclamation assets =
    SvgLink assets.exclamation


{-| -}
facebook : { r | facebookBlue_svg : Asset } -> IconType msg
facebook assets =
    SvgIcon Assets.facebookBlueSvg


{-| -}
flag : { r | iconFlag_png : Asset } -> IconType msg
flag assets =
    SvgIcon Assets.flagRedSvg


{-| -}
flipper : { r | flipper : String } -> IconType msg
flipper assets =
    SvgLink assets.flipper


{-| -}
footsteps : { r | footsteps : String } -> IconType msg
footsteps assets =
    SvgLink assets.footsteps


{-| -}
gardening : { r | startingOffBadge_png : Asset } -> IconType msg
gardening assets =
    SvgIcon Assets.startingOffBadgeSvg


{-| -}
gear : { r | gear : String } -> IconType msg
gear assets =
    SvgLink assets.gear


{-| -}
greenCheckMark : { r | smallCheckmark_png : Asset } -> IconType msg
greenCheckMark assets =
    SvgIcon Assets.checkGreenSvg


{-| -}
guidedWrite : { r | icons_guidedWrite_svg : Asset } -> IconType msg
guidedWrite assets =
    ImgIcon assets.icons_guidedWrite_svg


{-| -}
help : { r | icons_helpBlue_svg : Asset } -> IconType msg
help assets =
    SvgIcon Assets.helpBlueSvg


{-| -}
highFive : { r | level3Badge_png : Asset } -> IconType msg
highFive assets =
    SvgIcon Assets.level3GraphicSvg


{-| -}
key : { r | key : String } -> IconType msg
key assets =
    SvgLink assets.key


{-| -}
late : { r | icons_clockRed_svg : Asset } -> IconType msg
late assets =
    SvgIcon Assets.clockRedSvg


{-| -}
leaderboard : { r | leaderboard : String } -> IconType msg
leaderboard assets =
    SvgLink assets.leaderboard


{-| -}
lightBulb : { r | hint_png : Asset } -> IconType msg
lightBulb assets =
    SvgIcon Assets.bulbYellowSvg


{-| -}
lock : { r | lock : String } -> IconType msg
lock assets =
    SvgLink assets.lock


{-| -}
lockDeprecated : { r | premiumLock_svg : Asset } -> IconType msg
lockDeprecated assets =
    SvgIcon Assets.premiumLockSvg


{-| -}
logo : { r | logoRedBlack_svg : Asset } -> IconType msg
logo assets =
    SvgIcon Assets.logoRedBlackSvg


{-| -}
masteryBadge : { r | masteryBadge : String } -> IconType msg
masteryBadge assets =
    SvgIcon Assets.iconMasteryBadgeSvg


{-| -}
newspaper : { r | newspaper : String } -> IconType msg
newspaper assets =
    SvgLink assets.newspaper


{-| -}
notStarred : { r | commentNotStarred_png : Asset } -> IconType msg
notStarred assets =
    SvgIcon Assets.starEmptySvg


{-| -}
okay : { r | level2Badge_png : Asset } -> IconType msg
okay assets =
    SvgIcon Assets.level2GraphicSvg


{-| -}
openClose : { r | openClose : String } -> IconType msg
openClose assets =
    SvgLink assets.openClose


{-| -}
peerReview : { r | icons_peerReview_svg : Asset } -> IconType msg
peerReview assets =
    SvgIcon Assets.peerReviewSvg


{-| -}
performance : { r | performance : String } -> IconType msg
performance assets =
    SvgLink assets.performance


{-| -}
personBlue : { r | personBlue_svg : Asset } -> IconType msg
personBlue assets =
    SvgIcon Assets.personBlueSvg


{-| -}
preview : { r | preview : String } -> IconType msg
preview assets =
    SvgLink assets.preview


{-| -}
quickWrite : { r | icons_quickWrite_svg : Asset } -> IconType msg
quickWrite assets =
    SvgIcon Assets.quickWriteSvg


{-| -}
seeMore : { r | seemore : String } -> IconType msg
seeMore assets =
    SvgLink assets.seemore


{-| -}
share : { r | share : String } -> IconType msg
share assets =
    SvgLink assets.share


{-| -}
skip : { r | skip : String } -> IconType msg
skip assets =
    SvgLink assets.skip


{-| -}
sort : { r | sort : String } -> IconType msg
sort assets =
    SvgLink assets.sort


{-| -}
sortArrow : { r | sortArrow : String } -> IconType msg
sortArrow assets =
    SvgLink assets.sortArrow


{-| -}
speedometer : { r | speedometer : String } -> IconType msg
speedometer assets =
    SvgLink assets.speedometer


{-| -}
starred : { r | commentStarred_png : Asset } -> IconType msg
starred assets =
    SvgIcon Assets.starOrangeSvg


{-| -}
thumbsUp : { r | level1Badge_png : Asset } -> IconType msg
thumbsUp assets =
    SvgIcon Assets.level1GraphicSvg


{-| -}
twitter : { r | twitterBlue_svg : Asset } -> IconType msg
twitter assets =
    SvgIcon Assets.twitterBlueSvg


{-| -}
unarchive : { r | unarchiveBlue2x_png : Asset } -> IconType msg
unarchive assets =
    SvgIcon Assets.unarchiveBlueSvg


{-| -}
writingAssignment : { r | writingAssignment : String } -> IconType msg
writingAssignment assets =
    SvgLink assets.writingAssignment


{-| -}
x : { r | xWhite_svg : Asset } -> IconType msg
x assets =
    SvgIcon Assets.xWhiteSvg


{-| -}
xSvg : { r | x : String } -> IconType msg
xSvg assets =
    SvgIcon Assets.xBlueSvg


{-| -}
submitting : { r | submitting : String } -> IconType msg
submitting assets =
    SvgLink assets.submitting


{-| -}
rating : { r | rating : String } -> IconType msg
rating assets =
    SvgLink assets.rating


{-| -}
revising : { r | revising : String } -> IconType msg
revising assets =
    SvgLink assets.revising


{-| Inlining SVG styles because styles.class doesn't work on SVG elements.
The `className` property of an SVG element isn't a string, it's an object and so
`styles.class` causes a runtime exception by attempting to overwrite it with
a string. Another workaround is to use the `Svg.Attributes.class` attribute but
since `withNamespace` hides a call to `Html.Attributes.class` we can't do it
properly.
-}
svgStyle : RootHtml.Attribute msg
svgStyle =
    RootAttr.style
        [ ( "fill", "currentColor" )
        , ( "width", "100%" )
        , ( "height", "100%" )
        ]
