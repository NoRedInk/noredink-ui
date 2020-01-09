module Nri.Ui.HighlightIcon.V1 exposing
    ( highlighter, highlighterCursor
    , eraser, eraserCursor
    )

{-|

@docs highlighter, highlighterCursor
@docs eraser, eraserCursor

-}

import Color
import Css
import Nri.Ui.Colors.Extra as ColorsExtra
import Nri.Ui.Svg.V1
import Svg.Styled as Svg
import Svg.Styled.Attributes as Attributes


{-| -}
highlighter : Nri.Ui.Svg.V1.Svg
highlighter =
    Svg.svg
        [ Attributes.width "100%"
        , Attributes.height "100%"
        , Attributes.fill "currentcolor"
        , Attributes.viewBox "0 0 20 19"
        ]
        [ Svg.path [ Attributes.d highlighterD ] []
        ]
        |> Nri.Ui.Svg.V1.fromHtml


{-| -}
highlighterCursor : Css.Color -> Css.Style
highlighterCursor color =
    cursor
        { color = color
        , width = "20"
        , height = "19"
        , d = highlighterD
        }


highlighterD : String
highlighterD =
    "M9.41807931,13.4291766 L4.26622163,8.34000495 L14.5705927,0.195646008 L17.6613576,3.25018551 L9.41807931,13.4291766 Z M3.76264181,10.5348681 L3.75164785,8.84876664 L8.90416124,13.9394477 L7.1966016,13.9293264 L3.75153857,15.9749069 L1.69136377,13.9394542 L3.76264181,10.5348681 Z M1.69127635,14.9567702 L2.72092661,15.9749069 L2.20609055,16.4835649 L0.145915533,16.4835649 L1.69127635,14.9567702 Z M0.146003179,18.6438119 L0.146003179,17.2038918 L19.0888228,17.2038918 L19.0888228,18.6438119 L0.146003179,18.6438119 Z"


{-| -}
eraser : Nri.Ui.Svg.V1.Svg
eraser =
    Svg.svg
        [ Attributes.width "100%"
        , Attributes.height "100%"
        , Attributes.fill "currentcolor"
        , Attributes.viewBox "0 0 15 18"
        ]
        [ Svg.g [ Attributes.transform "translate(-2.000000, 0.000000)" ]
            [ Svg.path [ Attributes.d eraserD ] []
            ]
        ]
        |> Nri.Ui.Svg.V1.fromHtml


{-| -}
eraserCursor : Css.Color -> Css.Style
eraserCursor color =
    cursor
        { color = color
        , width = "15"
        , height = "18"
        , d = eraserD
        }


eraserD : String
eraserD =
    "M7.50872094,7.57232834 C7.41337492,7.57232834 7.31802679,7.54438231 7.23336386,7.4876675 C7.00897077,7.33560457 6.94977931,7.02901258 7.1026734,6.8046195 L11.5699138,0.215913337 C11.7227974,-0.00930038984 12.0285687,-0.067671209 12.2529618,0.0852228822 C12.4773549,0.23728581 12.5365464,0.543877801 12.3836523,0.768270885 L7.91641186,7.35697705 C7.82106584,7.4967093 7.66653678,7.57232202 7.50872094,7.57232202 Z M11.1163911,10.2939151 C11.0218657,10.2939151 10.9256969,10.2667897 10.841034,10.2092543 C10.6166409,10.0571914 10.5574494,9.75142001 10.7095208,9.52620629 L15.3461502,2.67763004 C15.4990338,2.45241631 15.8039845,2.39404549 16.0291982,2.54611684 C16.2535913,2.69817977 16.3127828,3.00395111 16.1607114,3.22916484 L11.524082,10.0777411 C11.4287339,10.2182961 11.2742069,10.2939067 11.1163911,10.2939067 Z M9.25795172,8.80371274 C9.18561946,8.80371274 9.1132872,8.78234237 9.04917399,8.73877889 C8.8814936,8.62288314 8.83875287,8.39356199 8.95464861,8.2258774 L13.8370499,1.1306876 C13.9529456,0.963007217 14.1822668,0.920266478 14.3499514,1.03616222 C14.517636,1.15205797 14.5603725,1.38137912 14.4444768,1.54906371 L9.56207547,8.64425351 C9.49056596,8.74864132 9.37549085,8.80371274 9.25795382,8.80371274 Z M5.97580306,17.5648066 C5.70290791,17.5648066 5.42755083,17.4891854 5.18262065,17.3297241 L2.82022271,15.7918403 C2.49472228,15.5797779 2.27114984,15.2534568 2.19142127,14.8720685 C2.11169271,14.4906802 2.18484561,14.1018851 2.39773287,13.7764057 L3.65615639,11.8554924 C3.84191826,11.5719078 4.22330444,11.4921792 4.50688899,11.6779495 C4.79129417,11.8645341 4.87020209,12.2450976 4.68443181,12.5295028 L3.42600829,14.4504161 C3.3824448,14.5169955 3.38737707,14.5852161 3.39477338,14.6197399 C3.40216968,14.6542616 3.4243628,14.7183748 3.49094214,14.7619383 L5.85334008,16.2998221 C5.95772789,16.3672221 6.10239452,16.3376327 6.17061304,16.2332428 L7.42903656,14.3123294 C7.61479843,14.0287448 7.99618461,13.9490163 8.27976916,14.1347866 C8.56417434,14.3213712 8.64308226,14.7019346 8.45731198,14.9863398 L7.19888846,16.9072532 C6.91942817,17.3330193 6.45254578,17.5648192 5.97581569,17.5648192 L5.97580306,17.5648066 Z M10.2031634,11.890801 C10.1053512,11.890801 10.007537,11.8620322 9.92124118,11.8020286 L6.11072509,9.13304705 C5.88797329,8.97769734 5.83454737,8.67110956 5.98989707,8.44835776 C6.14524678,8.2264266 6.45183456,8.17218004 6.67458636,8.32752974 L10.4851025,10.9965112 C10.7078543,11.151861 10.7612802,11.4584487 10.6059305,11.6812005 C10.5105844,11.8184667 10.3576967,11.8907989 10.2031634,11.8907989 Z M9.09929419,13.4683282 C9.00148204,13.4683282 8.90366778,13.4395594 8.81737198,13.3795557 L5.00685588,10.7105742 C4.78410408,10.5552245 4.73067816,10.2486367 4.88602787,10.0258849 C5.04137757,9.80313311 5.34796535,9.74888654 5.57071715,9.90505689 L9.38123325,12.5740384 C9.60398505,12.7293881 9.65741097,13.0359759 9.50206126,13.2587277 C9.40753589,13.3959938 9.25464811,13.4683261 9.09929419,13.4683261 Z M9.60388404,17.2737942 C9.60388404,18.2420686 8.15067369,18.2420686 8.15067369,17.2737942 C8.15067369,16.3055203 9.60388404,16.3055203 9.60388404,17.2737942 Z M11.8455003,17.2737942 C11.8455003,17.9593047 10.8164147,17.9593047 10.8164147,17.2737942 C10.8164147,16.5874636 11.8455003,16.5874636 11.8455003,17.2737942 Z M10.9396373,15.4467076 C10.9396373,16.4149814 9.48642697,16.4149814 9.48642697,15.4467076 C9.48642697,14.4784337 10.9396373,14.4784337 10.9396373,15.4467076 Z M13.1900912,15.4467076 C13.1900912,16.1445482 12.1429095,16.1445482 12.1429095,15.4467076 C12.1429095,14.7488669 13.1900912,14.7488669 13.1900912,15.4467076 Z M14.1247818,17.2737942 C14.1247818,17.7283465 13.4433751,17.7283465 13.4433751,17.2737942 C13.4433751,16.8192424 14.1247818,16.8192424 14.1247818,17.2737942 Z"


cursor { color, d, width, height } =
    let
        fill =
            Color.toRGBString (ColorsExtra.fromCssColor color)

        svgString =
            [ "%3Csvg xmlns='http://www.w3.org/2000/svg' "
            , "width='" ++ width
            , "' height='" ++ height
            , "' viewBox='0 0 " ++ width ++ " " ++ height
            , "' fill='" ++ fill
            , "' %3E"
            , "%3Cpath d='" ++ d ++ "'" ++ "/%3E"
            , "%3C/svg%3E"
            ]
    in
    Css.property "cursor" ("url(\"data:image/svg+xml," ++ String.join "" svgString ++ "\") 0 " ++ height ++ ",auto")
