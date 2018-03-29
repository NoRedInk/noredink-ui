module Nri.Ui.Icon.V1
    exposing
        ( Assets
        , AssignmentType(..)
        , IconSize(..)
        , IconType(..)
        , button
        , decorativeIcon
        , icon
        , link
        , linkExternal
        , styles
        )

{-|

@docs icon, decorativeIcon, link, linkExternal, button
@docs IconType, AssignmentType, IconSize
@docs styles
@docs Assets

-}

import Accessibility exposing (..)
import Accessibility.Role as Role
import Css exposing (..)
import Css.Foreign exposing (Snippet, adjacentSiblings, children, class, descendants, each, everything, media, selector, withClass)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (onClick)
import Nri.Ui.AssetPath exposing (Asset(..))
import Nri.Ui.Colors.V1
import Nri.Ui.Styles.V1
import Svg exposing (svg, use)
import Svg.Attributes exposing (xlinkHref)


{-| -}
type alias IconLinkModel =
    { alt : String
    , url : String
    , icon : IconType
    , disabled : Bool
    , size : IconSize
    }


type alias IconButtonModel msg =
    { alt : String
    , msg : msg
    , icon : IconType
    , disabled : Bool
    , size : IconSize
    }


type IconRenderVariant
    = ImgIcon Asset
    | SvgIcon String


{-| -}
type IconType
    = Activity
    | Add
    | ArrowDown
    | ArrowLeft
    | ArrowRight
    | AssignmentType AssignmentType
    | Attention
    | Bang
    | Bulb
    | Calendar
    | Caret
    | CheckMark
    | CheckMarkSquiggily
    | CheckMarkSvg
    | Class
    | Clever
    | Clock
    | Close
    | CompassSvg
    | Copy
    | Custom String String
    | DarkBlueCheckMark
    | Document
    | Download
    | Edit
    | EditWriting
    | EqualitySign
    | Exclamation
    | Facebook
    | Flag
    | Flipper
    | Footsteps
    | Gardening
    | Gear
    | GreenCheckMark
    | Help
    | HighFive
    | Key
    | Late
    | Leaderboard
    | LightBulb
    | Lock
    | LockDeprecated
    | Logo
    | Newspaper
    | NotStarred
    | Okay
    | OpenClose
    | PeerReview
    | Performance
    | PersonBlue
    | Preview
    | QuickWrite
    | SeeMore
    | Share
    | Sort
    | SortArrow
    | Speedometer
    | Starred
    | ThumbsUp
    | Twitter
    | Unarchive
    | WritingAssignment
    | X
    | XSvg


{-| Icons describing Assignment Types.
-}
type AssignmentType
    = Diagnostic
    | Practice
    | Quiz
    | WritingCycle


{-| Used for determining sizes on Icon.buttons and Icon.links
-}
type IconSize
    = Small
    | Medium


iconSizeToCssClass : IconSize -> CssClasses
iconSizeToCssClass size =
    case size of
        Small ->
            SmallIcon

        Medium ->
            MediumIcon


{-| Create an icon that links to a part of NRI
Uses our default icon styles (25 x 25 px, azure)
-}
link : Assets r -> IconLinkModel -> Html msg
link assets =
    linkBase assets (Attr.target "_self")


{-| Create an accessible icon button with an onClick handler
Uses our default icon styles (25 x 25 px, azure)
-}
button : Assets r -> IconButtonModel msg -> Html msg
button assets model =
    Accessibility.button
        [ styles.class [ Button, iconSizeToCssClass model.size ]
        , onClick model.msg
        , Attr.disabled model.disabled
        , Attr.type_ "button"
        ]
        [ icon
            assets
            { alt = model.alt
            , icon = model.icon
            }
        ]


{-| -}
icon : Assets r -> { alt : String, icon : IconType } -> Html msg
icon assets config =
    case iconVariant assets config.icon of
        SvgIcon iconId ->
            svg [ svgStyle ]
                [ Svg.title [] [ Accessibility.text config.alt ]
                , use [ xlinkHref ("#" ++ iconId) ] []
                ]

        ImgIcon assetPath ->
            img config.alt
                [ Attr.src (Nri.Ui.AssetPath.url assetPath)
                ]


{-| Use this icon for purely decorative content that would be distracting
rather than helpful on a screenreader.
-}
decorativeIcon : Assets r -> IconType -> Html msg
decorativeIcon assets iconType =
    case iconVariant assets iconType of
        SvgIcon iconId ->
            svg
                [ svgStyle
                , Role.img
                ]
                [ use [ xlinkHref ("#" ++ iconId) ] []
                ]

        ImgIcon assetPath ->
            decorativeImg [ Attr.src (Nri.Ui.AssetPath.url assetPath) ]


{-| Create an icon that links to an external site
Uses our default icon styles (25 x 25 px, azure)
-}
linkExternal : Assets r -> IconLinkModel -> Html msg
linkExternal assets =
    linkBase assets (Attr.target "_blank")


linkBase : Assets r -> Attribute Never -> IconLinkModel -> Html msg
linkBase assets linkTarget model =
    span
        []
        [ a
            (linkTarget :: linkAttributes model)
            [ icon assets { alt = model.alt, icon = model.icon }
            ]
        ]


linkAttributes : IconLinkModel -> List (Attribute Never)
linkAttributes model =
    if model.disabled then
        [ styles.class [ Disabled, Link, iconSizeToCssClass model.size ] ]
    else
        [ styles.class [ Link, iconSizeToCssClass model.size ], href model.url ]


iconVariant : Assets r -> IconType -> IconRenderVariant
iconVariant assets icon =
    case icon of
        Activity ->
            SvgIcon assets.activity

        Add ->
            ImgIcon assets.icons_plusBlue_svg

        ArrowDown ->
            SvgIcon assets.arrowDown

        ArrowLeft ->
            ImgIcon assets.leftArrowBlue_png

        ArrowRight ->
            ImgIcon assets.icons_arrowRightBlue_svg

        AssignmentType Diagnostic ->
            SvgIcon assets.diagnostic

        AssignmentType Practice ->
            SvgIcon assets.practice

        AssignmentType Quiz ->
            SvgIcon assets.quiz

        AssignmentType WritingCycle ->
            SvgIcon assets.writingcycle

        Attention ->
            ImgIcon assets.attention_svg

        Bang ->
            ImgIcon assets.exclamationPoint_svg

        Bulb ->
            SvgIcon assets.bulb

        Calendar ->
            SvgIcon assets.calendar

        Caret ->
            ImgIcon assets.icons_arrowDownBlue_svg

        CheckMark ->
            ImgIcon assets.iconCheck_png

        CheckMarkSquiggily ->
            ImgIcon assets.squiggly_png

        CheckMarkSvg ->
            SvgIcon assets.checkmark

        Class ->
            SvgIcon assets.class

        Clever ->
            SvgIcon assets.clever

        Clock ->
            SvgIcon assets.clock

        Close ->
            ImgIcon assets.icons_xBlue_svg

        Copy ->
            ImgIcon assets.teach_assignments_copyWhite_svg

        CompassSvg ->
            SvgIcon assets.compass

        Custom url _ ->
            ImgIcon (Asset url)

        DarkBlueCheckMark ->
            ImgIcon assets.darkBlueCheckmark_svg

        Document ->
            SvgIcon assets.document

        Download ->
            SvgIcon assets.download

        Edit ->
            SvgIcon assets.edit

        EditWriting ->
            SvgIcon assets.editWriting

        EqualitySign ->
            ImgIcon assets.icons_equals_svg

        Exclamation ->
            SvgIcon assets.exclamation

        Facebook ->
            ImgIcon assets.facebookBlue_svg

        Flag ->
            ImgIcon assets.iconFlag_png

        Flipper ->
            SvgIcon assets.flipper

        Footsteps ->
            SvgIcon assets.footsteps

        Gardening ->
            ImgIcon assets.startingOffBadge_png

        Gear ->
            SvgIcon assets.gear

        GreenCheckMark ->
            ImgIcon assets.smallCheckmark_png

        Help ->
            ImgIcon assets.icons_helpBlue_svg

        HighFive ->
            ImgIcon assets.level3Badge_png

        Key ->
            SvgIcon assets.key

        Late ->
            ImgIcon assets.icons_clockRed_svg

        Leaderboard ->
            SvgIcon assets.leaderboard

        LightBulb ->
            ImgIcon assets.hint_png

        Lock ->
            SvgIcon assets.lock

        LockDeprecated ->
            ImgIcon assets.premiumLock_svg

        Logo ->
            ImgIcon assets.logoRedBlack_svg

        Newspaper ->
            SvgIcon assets.newspaper

        NotStarred ->
            ImgIcon assets.commentNotStarred_png

        Okay ->
            ImgIcon assets.level2Badge_png

        OpenClose ->
            SvgIcon assets.openClose

        PeerReview ->
            ImgIcon assets.icons_peerReview_svg

        Performance ->
            SvgIcon assets.performance

        PersonBlue ->
            ImgIcon assets.personBlue_svg

        Preview ->
            SvgIcon assets.preview

        QuickWrite ->
            ImgIcon assets.icons_quickWrite_svg

        SeeMore ->
            SvgIcon assets.seemore

        Share ->
            SvgIcon assets.share

        Sort ->
            SvgIcon assets.sort

        SortArrow ->
            SvgIcon assets.sortArrow

        Speedometer ->
            SvgIcon assets.speedometer

        Starred ->
            ImgIcon assets.commentStarred_png

        ThumbsUp ->
            ImgIcon assets.level1Badge_png

        Twitter ->
            ImgIcon assets.twitterBlue_svg

        Unarchive ->
            ImgIcon assets.unarchiveBlue2x_png

        WritingAssignment ->
            SvgIcon assets.writingAssignment

        X ->
            ImgIcon assets.xWhite_svg

        XSvg ->
            SvgIcon assets.x


{-| -}
type alias Assets r =
    { r
        | activity : String
        , arrowDown : String
        , attention_svg : Asset
        , bulb : String
        , calendar : String
        , checkmark : String
        , class : String
        , clever : String
        , clock : String
        , commentNotStarred_png : Asset
        , commentStarred_png : Asset
        , compass : String
        , darkBlueCheckmark_svg : Asset
        , diagnostic : String
        , document : String
        , download : String
        , edit : String
        , editWriting : String
        , exclamation : String
        , exclamationPoint_svg : Asset
        , facebookBlue_svg : Asset
        , flipper : String
        , footsteps : String
        , gear : String
        , hint_png : Asset
        , iconCheck_png : Asset
        , iconFlag_png : Asset
        , icons_arrowDownBlue_svg : Asset
        , icons_arrowRightBlue_svg : Asset
        , icons_clockRed_svg : Asset
        , icons_equals_svg : Asset
        , icons_helpBlue_svg : Asset
        , icons_peerReview_svg : Asset
        , icons_plusBlue_svg : Asset
        , icons_quickWrite_svg : Asset
        , icons_xBlue_svg : Asset
        , key : String
        , leaderboard : String
        , leftArrowBlue_png : Asset
        , level1Badge_png : Asset
        , level2Badge_png : Asset
        , level3Badge_png : Asset
        , lock : String
        , logoRedBlack_svg : Asset
        , newspaper : String
        , openClose : String
        , performance : String
        , personBlue_svg : Asset
        , practice : String
        , premiumLock_svg : Asset
        , preview : String
        , quiz : String
        , seemore : String
        , share : String
        , smallCheckmark_png : Asset
        , sort : String
        , sortArrow : String
        , speedometer : String
        , squiggly_png : Asset
        , startingOffBadge_png : Asset
        , teach_assignments_copyWhite_svg : Asset
        , twitterBlue_svg : Asset
        , unarchiveBlue2x_png : Asset
        , writingAssignment : String
        , writingcycle : String
        , x : String
        , xWhite_svg : Asset
    }


{-| Inlining SVG styles because styles.class doesn't work on SVG elements.
The `className` property of an SVG element isn't a string, it's an object and so
`styles.class` causes a runtime exception by attempting to overwrite it with
a string. Another workaround is to use the `Svg.Attributes.class` attribute but
since `withNamespace` hides a call to `Html.Attributes.class` we can't do it
properly.
-}
svgStyle : Attribute msg
svgStyle =
    style
        [ ( "fill", "currentColor" )
        , ( "width", "100%" )
        , ( "height", "100%" )
        ]


type CssClasses
    = Disabled
    | Button
    | Link
    | SvgIconContainer
    | SmallIcon
    | MediumIcon


{-| -}
styles : Nri.Ui.Styles.V1.Styles Never CssClasses msg
styles =
    Nri.Ui.Styles.V1.styles "Nri-Ui-Icon-V1-"
        [ Css.Foreign.class Disabled
            [ Css.property "cursor" "not-allowed" ]
        , Css.Foreign.class Button
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
        , Css.Foreign.class Link
            [ color Nri.Ui.Colors.V1.azure
            , display inlineBlock
            , fontFamily inherit
            , Css.property "cursor" "pointer"
            , padding zero
            , visited [ color Nri.Ui.Colors.V1.azure ]
            ]
        , Css.Foreign.class SmallIcon
            [ Css.width (px 20)
            , Css.height (px 20)
            ]
        , Css.Foreign.class MediumIcon
            [ Css.width (px 25)
            , Css.height (px 25)
            ]
        , Css.Foreign.class SvgIconContainer
            [ fill currentColor ]
        ]
