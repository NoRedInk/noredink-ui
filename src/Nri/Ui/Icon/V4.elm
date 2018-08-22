module Nri.Ui.Icon.V4
    exposing
        ( IconLinkSpaModel
        , IconSize(..)
        , IconType(..)
        , StandardIcon(..)
        , button
        , customImage
        , customSvgSprite
        , decorativeIcon
        , icon
        , link
        , linkExternal
        , linkSpa
        )

{-|

@docs icon, decorativeIcon, link, linkExternal, linkSpa, button
@docs IconType, IconSize, IconLinkSpaModel
@docs customImage
@docs customSvgSprite

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
type alias IconLinkModel =
    { alt : String
    , url : String
    , icon : IconType
    , disabled : Bool
    , size : IconSize
    }


{-| -}
type alias IconLinkSpaModel route =
    { alt : String
    , icon : IconType
    , disabled : Bool
    , size : IconSize
    , route : route
    }


type alias IconButtonModel msg =
    { alt : String
    , msg : msg
    , icon : IconType
    , disabled : Bool
    , size : IconSize
    }


{-| An icon that can be rendered using the functions provided by this module.
-}
type IconType
    = ImgUrl Asset
    | SvgSprite String
    | StandardIcon StandardIcon


{-| Used for determining sizes on Icon.buttons and Icon.links
-}
type IconSize
    = Small
    | Medium


{-| Create an icon that links to a part of NRI
Uses our default icon styles (25 x 25 px, azure)
-}
link : IconLinkModel -> Html msg
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
icon : { alt : String, icon : IconType } -> Html msg
icon config =
    case config.icon of
        SvgSprite iconId ->
            svg [ svgStyle ]
                [ Svg.title [] [ RootHtml.text config.alt ]
                , use [ xlinkHref ("#" ++ iconId) ] []
                ]
                |> Html.Styled.fromUnstyled

        StandardIcon iconId ->
            let
                svgElement =
                    convertIconType iconId
            in
            svgElement [ svgStyle ] [ Svg.title [] [ RootHtml.text config.alt ] ]
                |> Html.Styled.fromUnstyled

        ImgUrl assetPath ->
            img config.alt
                [ Attributes.src (Nri.Ui.AssetPath.url assetPath)
                ]


{-| Use this icon for purely decorative content that would be distracting
rather than helpful on a screenreader.
-}
decorativeIcon : IconType -> Html msg
decorativeIcon iconType =
    case iconType of
        SvgSprite iconId ->
            svg
                [ svgStyle
                , Role.img
                ]
                [ use [ xlinkHref ("#" ++ iconId) ] []
                ]
                |> Html.Styled.fromUnstyled

        StandardIcon iconId ->
            let
                svgElement =
                    convertIconType iconId
            in
            svgElement [ svgStyle, Role.img ] []
                |> Html.Styled.fromUnstyled

        ImgUrl assetPath ->
            decorativeImg [ Attributes.src (Nri.Ui.AssetPath.url assetPath) ]


{-| Use this link for routing within a single page app.

This will make a normal <a> tag, but change the onClick behavior to avoid reloading the page.

-}
linkSpa : (route -> String) -> (route -> msg) -> IconLinkSpaModel route -> Html msg
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
linkExternal : IconLinkModel -> Html msg
linkExternal =
    linkBase [ Attributes.target "_blank" ]


linkBase : List (Attribute msg) -> IconLinkModel -> Html msg
linkBase linkAttributes model =
    span
        []
        [ Html.Styled.a
            (linkAttributes ++ defaultLinkAttributes model)
            [ icon { alt = model.alt, icon = model.icon }
            ]
        ]


defaultLinkAttributes : IconLinkModel -> List (Attribute msg)
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


{-| Specifies a custom Image to be used, passing the Asset definition with its url.

    The URL will be used as a relative URL.

-}
customImage : Asset -> IconType
customImage asset =
    ImgUrl asset


{-| Specifies a custom SVG Sprite to be used, passing the id to be used in `xlinkHref ("#" ++ theId)`.

    We will trust this sprite will be present in the webpage and will make no effort to verify that.

-}
customSvgSprite : String -> IconType
customSvgSprite ref =
    SvgSprite ref


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


{-| Our list of standard icons provided with this package as built-in SVGs in Elm
-}
type StandardIcon
    = Add
    | ArrowDown
    | ArrowLeft
    | ArrowRight
    | AssignmentStartButtonPrimary
    | AssignmentStartButtonSecondary
    | Attention
    | Bang
    | Bulb
    | Calendar
    | Caret
    | CheckMark
    | CheckMarkSquiggily
    | Close
    | Copy
    | DarkBlueCheckMark
    | EqualitySign
    | Facebook
    | Flag
    | Gardening
    | GreenCheckMark
    | Help
    | HighFive
    | Late
    | LightBulb
    | LockDeprecated
    | Logo
    | MasteryBadge
    | NotStarred
    | Okay
    | PeerReview
    | PersonBlue
    | QuickWrite
    | Starred
    | ThumbsUp
    | Twitter
    | Unarchive
    | X
    | XSvg


convertIconType : StandardIcon -> (List (Svg.Attribute msg) -> List (Svg.Svg msg) -> RootHtml.Html msg)
convertIconType iconType =
    case iconType of
        Add ->
            Assets.plusBlueSvg

        ArrowDown ->
            Assets.arrowDownBlueSvg

        ArrowLeft ->
            Assets.arrowLeftBlueSvg

        ArrowRight ->
            Assets.arrowRightBlueSvg

        AssignmentStartButtonPrimary ->
            Assets.assignmentStartButtonPrimarySvg

        AssignmentStartButtonSecondary ->
            Assets.assignmentStartButtonSecondarySvg

        Attention ->
            Assets.attentionSvg

        Bang ->
            Assets.exclamationPointSvg

        Bulb ->
            Assets.bulbYellowSvg

        Calendar ->
            Assets.iconCalendarSvg

        Caret ->
            Assets.arrowDownBlueSvg

        CheckMark ->
            Assets.checkWhiteSvg

        CheckMarkSquiggily ->
            Assets.squigglySvg

        Close ->
            Assets.xBlueSvg

        Copy ->
            Assets.copyWhiteSvg

        DarkBlueCheckMark ->
            Assets.darkBlueCheckmarkSvg

        EqualitySign ->
            Assets.equalsSvg

        Facebook ->
            Assets.facebookBlueSvg

        Flag ->
            Assets.flagRedSvg

        Gardening ->
            Assets.startingOffBadgeSvg

        GreenCheckMark ->
            Assets.checkGreenSvg

        Help ->
            Assets.helpBlueSvg

        HighFive ->
            Assets.level3GraphicSvg

        Late ->
            Assets.clockRedSvg

        LightBulb ->
            Assets.bulbYellowSvg

        LockDeprecated ->
            Assets.premiumLockSvg

        Logo ->
            Assets.logoRedBlackSvg

        MasteryBadge ->
            Assets.iconMasteryBadgeSvg

        NotStarred ->
            Assets.starEmptySvg

        Okay ->
            Assets.level2GraphicSvg

        PeerReview ->
            Assets.peerReviewSvg

        PersonBlue ->
            Assets.personBlueSvg

        QuickWrite ->
            Assets.quickWriteSvg

        Starred ->
            Assets.starOrangeSvg

        ThumbsUp ->
            Assets.level1GraphicSvg

        Twitter ->
            Assets.twitterBlueSvg

        Unarchive ->
            Assets.unarchiveBlueSvg

        X ->
            Assets.xWhiteSvg

        XSvg ->
            Assets.xBlueSvg


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
