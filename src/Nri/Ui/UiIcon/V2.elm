module Nri.Ui.UiIcon.V2 exposing
    ( seeMore, openClose, download, sort, gear, flipper, hamburger, kebab
    , archive, unarchive
    , playInCircle, pauseInCircle, stopInCircle, speaker, mutedSpeaker
    , play, skip
    , share, preview, copyToClipboard, gift, print
    , activity
    , footsteps, footstepsVideo, compass, help, checklist, review
    , sparkleBulb, baldBulb
    , hat, keychain
    , sprout, sapling, tree
    , person, couple, class, leaderboard, performance
    , emptyCalendar, calendar, clock
    , missingDocument, document, documents, text, texts
    , edit, highlighter, eraser
    , speechBalloon, speechBalloonOutline, mail
    , arrowTop, arrowRight, arrowDown, arrowLeft, arrowPointingRight, arrowPointingRightThick, sortArrow, sortArrowDown
    , checkmark, checkmarkInCircle, checkmarkInCircleInverse, emptyCircle, x, xInCircle
    , attention, exclamation
    , flag, star, starFilled, starOutline, no
    , equals, plus, null
    , key, lock, premiumLock
    , badge, tada, count
    , bold, italic, underline, list, link, undo, redo
    , home, homeInCircle, library
    , search, searchInCicle
    , microscope, scale
    , openInNewTab, sync, delete, addSticker, circle
    , retire, unretire, publish, unpublish, duplicate
    , apple, appleOutline, briefcase
    , school, college, company, homeSchool, graduateCap
    , flagUs, globe
    , info
    , brain, projectorScreen, stretch
    , gradingAssistant, manuallyGraded
    )

{-| How to add new icons: <https://paper.dropbox.com/doc/How-to-create-a-new-SVG-icon-for-use-in-Elm--Ay9uhSLfGUAix0ERIiJ0Dm8dAg-8WNqtARdr4EgjmYEHPeYD>

@docs seeMore, openClose, download, sort, gear, flipper, hamburger, kebab
@docs archive, unarchive
@docs playInCircle, pauseInCircle, stopInCircle, speaker, mutedSpeaker
@docs play, skip
@docs share, preview, copyToClipboard, gift, print
@docs activity
@docs footsteps, footstepsVideo, compass, help, checklist, review
@docs sparkleBulb, baldBulb
@docs hat, keychain
@docs sprout, sapling, tree
@docs person, couple, class, leaderboard, performance
@docs emptyCalendar, calendar, clock
@docs missingDocument, document, documents, text, texts
@docs edit, highlighter, eraser
@docs speechBalloon, speechBalloonOutline, mail
@docs arrowTop, arrowRight, arrowDown, arrowLeft, arrowPointingRight, arrowPointingRightThick, sortArrow, sortArrowDown
@docs checkmark, checkmarkInCircle, checkmarkInCircleInverse, emptyCircle, x, xInCircle
@docs attention, exclamation
@docs flag, star, starFilled, starOutline, no
@docs equals, plus, null
@docs key, lock, premiumLock
@docs badge, tada, count
@docs bold, italic, underline, list, link, undo, redo
@docs home, homeInCircle, library
@docs search, searchInCicle
@docs microscope, scale
@docs openInNewTab, sync, delete, addSticker, circle
@docs retire, unretire, publish, unpublish, duplicate
@docs apple, appleOutline, briefcase
@docs school, college, company, homeSchool, graduateCap
@docs flagUs, globe
@docs info
@docs brain, projectorScreen, stretch
@docs gradingAssistant, manuallyGraded

    import Html.Styled exposing (..)
    import Nri.Ui.Colors.V1 as Colors
    import Nri.Ui.Svg.V1 as Svg
    import Nri.Ui.UiIcon.V2 as UiIcon

    view : Html msg
    view =
        UiIcon.unarchive
            |> Svg.withColor Colors.lichen
            |> Svg.toHtml

-}

import Nri.Ui.Svg.V1
import Svg.Styled as Svg
import Svg.Styled.Attributes as Attributes


{-| -}
unarchive : Nri.Ui.Svg.V1.Svg
unarchive =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M20 6H2V12C2 15.7712 2 17.6569 3.17157 18.8284C4.34315 20 6.22876 20 10 20H12C15.7712 20 17.6569 20 18.8284 18.8284C20 17.6569 20 15.7712 20 12V6Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M20 6H2L3.2 4.4C4.08328 3.22229 4.52492 2.63344 5.15836 2.31672C5.7918 2 6.52786 2 8 2H14C15.4721 2 16.2082 2 16.8416 2.31672C17.4751 2.63344 17.9167 3.22229 18.8 4.4L20 6Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M11 10V16.5M8 12.5C8.58984 11.8932 10.1597 9.5 11 9.5C11.8403 9.5 13.4102 11.8932 14 12.5"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
archive : Nri.Ui.Svg.V1.Svg
archive =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M20 6H2V12C2 15.7712 2 17.6569 3.17157 18.8284C4.34315 20 6.22876 20 10 20H12C15.7712 20 17.6569 20 18.8284 18.8284C20 17.6569 20 15.7712 20 12V6Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M20 6H2L3.2 4.4C4.08328 3.22229 4.52492 2.63344 5.15836 2.31672C5.7918 2 6.52786 2 8 2H14C15.4721 2 16.2082 2 16.8416 2.31672C17.4751 2.63344 17.9167 3.22229 18.8 4.4L20 6Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M11 16V9.5M8 13.5C8.58984 14.1068 10.1597 16.5 11 16.5C11.8403 16.5 13.4102 14.1068 14 13.5"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
share : Nri.Ui.Svg.V1.Svg
share =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.path
            [ Attributes.d "M8.39585 3.5H7.35419C4.40791 3.5 2.93477 3.5 2.01948 4.37868C1.10419 5.25736 1.10419 6.67157 1.10419 9.5V13.5C1.10419 16.3284 1.10419 17.7426 2.01948 18.6213C2.93477 19.5 4.40791 19.5 7.35419 19.5H11.5608C14.5071 19.5 15.9802 19.5 16.8955 18.6213C17.4885 18.052 17.6973 17.2579 17.7708 16"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M15.1667 6V2.85355C15.1667 2.65829 15.3316 2.5 15.535 2.5C15.6326 2.5 15.7263 2.53725 15.7954 2.60355L20.5275 7.14645C20.7634 7.37282 20.8958 7.67986 20.8958 8C20.8958 8.32014 20.7634 8.62718 20.5275 8.85355L15.7954 13.3964C15.7263 13.4628 15.6326 13.5 15.535 13.5C15.3316 13.5 15.1667 13.3417 15.1667 13.1464V10H12.1157C7.875 10 6.3125 13.5 6.3125 13.5V11C6.3125 8.23858 8.64435 6 11.5208 6H15.1667Z"
            , Attributes.fill "currentColor"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            ]
            []
        ]


{-| -}
seeMore : Nri.Ui.Svg.V1.Svg
seeMore =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.path
            [ Attributes.d "M10.9959 11H11.0049"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2.5"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M14.9998 11H15.0088"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2.5"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M6.99982 11H7.0088"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2.5"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M21 11C21 5.47715 16.5228 1 11 1C5.47715 1 1 5.47715 1 11C1 16.5228 5.47715 21 11 21C16.5228 21 21 16.5228 21 11Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
preview : Nri.Ui.Svg.V1.Svg
preview =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.path
            [ Attributes.d "M20.544 10.045C20.848 10.4713 21 10.6845 21 11C21 11.3155 20.848 11.5287 20.544 11.955C19.1779 13.8706 15.6892 18 11 18C6.31078 18 2.8221 13.8706 1.45604 11.955C1.15201 11.5287 1 11.3155 1 11C1 10.6845 1.15201 10.4713 1.45604 10.045C2.8221 8.12944 6.31078 4 11 4C15.6892 4 19.1779 8.12944 20.544 10.045Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M14 11C14 9.3431 12.6569 8 11 8C9.3431 8 8 9.3431 8 11C8 12.6569 9.3431 14 11 14C12.6569 14 14 12.6569 14 11Z"
            , Attributes.fill "currentColor"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            ]
            []
        ]


{-| -}
print : Nri.Ui.Svg.V1.Svg
print =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M16 17H20C20.5523 17 21 16.5523 21 16V10C21 8.34315 19.6569 7 18 7H4C2.34315 7 1 8.34315 1 10V16C1 16.5523 1.44772 17 2 17H6"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M17.5 10.5H17.509"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M16 7V2.5C16 1.67157 15.3284 1 14.5 1H7.5C6.67157 1 6 1.67157 6 2.5V7"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M16 15V19C16 20.1046 15.1046 21 14 21H8C6.89543 21 6 20.1046 6 19V15H16Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
copyToClipboard : Nri.Ui.Svg.V1.Svg
copyToClipboard =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M8 14C8 11.1716 8 9.7574 8.87868 8.87868C9.7574 8 11.1716 8 14 8H15C17.8284 8 19.2426 8 20.1213 8.87868C21 9.7574 21 11.1716 21 14V15C21 17.8284 21 19.2426 20.1213 20.1213C19.2426 21 17.8284 21 15 21H14C11.1716 21 9.7574 21 8.87868 20.1213C8 19.2426 8 17.8284 8 15V14Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M15.9999 8C15.9975 5.04291 15.9528 3.51121 15.092 2.46243C14.9258 2.25989 14.7401 2.07418 14.5376 1.90796C13.4312 1 11.7875 1 8.5 1C5.21252 1 3.56878 1 2.46243 1.90796C2.25989 2.07417 2.07418 2.25989 1.90796 2.46243C1 3.56878 1 5.21252 1 8.5C1 11.7875 1 13.4312 1.90796 14.5376C2.07417 14.7401 2.25989 14.9258 2.46243 15.092C3.51121 15.9528 5.04291 15.9975 8 15.9999"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
info : Nri.Ui.Svg.V1.Svg
info =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.path
            [ Attributes.d "M21 11C21 5.47715 16.5228 1 11 1C5.47715 1 1 5.47715 1 11C1 16.5228 5.47715 21 11 21C16.5228 21 21 16.5228 21 11Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M11.2422 16V11C11.2422 10.5286 11.2422 10.2929 11.0957 10.1464C10.9493 10 10.7136 10 10.2422 10"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M10.992 7H11.001"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
performance : Nri.Ui.Svg.V1.Svg
performance =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M6 17V15M11 17V14M16 17V12M1.5 11C1.5 6.52166 1.5 4.28249 2.89124 2.89124C4.28249 1.5 6.52166 1.5 11 1.5C15.4783 1.5 17.7175 1.5 19.1088 2.89124C20.5 4.28249 20.5 6.52166 20.5 11C20.5 15.4783 20.5 17.7175 19.1088 19.1088C17.7175 20.5 15.4783 20.5 11 20.5C6.52166 20.5 4.28249 20.5 2.89124 19.1088C1.5 17.7175 1.5 15.4783 1.5 11Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M4.99219 10.4863C7.14729 10.5581 12.0341 10.2328 14.8137 5.82132M12.9923 5.28835L14.8678 4.98649C15.0964 4.95738 15.432 5.13785 15.5145 5.35298L16.0104 6.99142"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
openClose : Nri.Ui.Svg.V1.Svg
openClose =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.path
            [ Attributes.d "M3 5V17"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M7.00012 11.0005H19.0001"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M11 7C11 7 7.00001 9.946 7 11C6.99999 12.0541 11 15 11 15"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
download : Nri.Ui.Svg.V1.Svg
download =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M11 14V4M11 14C10.2998 14 8.99153 12.0057 8.5 11.5M11 14C11.7002 14 13.0085 12.0057 13.5 11.5"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M4 18H18.0001"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
edit : Nri.Ui.Svg.V1.Svg
edit =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M13 6L4.39171 14.6083C4.1354 14.8646 3.95356 15.1858 3.86564 15.5374L3 19L6.46257 18.1344C6.81424 18.0464 7.1354 17.8646 7.39171 17.6083L16 9M13 6L15.2929 3.70711C15.6834 3.31658 16.3166 3.31658 16.7071 3.70711L18.2929 5.29289C18.6834 5.68342 18.6834 6.31658 18.2929 6.70711L16 9M13 6L16 9"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
gear : Nri.Ui.Svg.V1.Svg
gear =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M14.5 11C14.5 12.933 12.933 14.5 11 14.5C9.067 14.5 7.5 12.933 7.5 11C7.5 9.067 9.067 7.5 11 7.5C12.933 7.5 14.5 9.067 14.5 11Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M8.77212 1.68377L8.00003 4L6.44019 4.98656L3.88799 4.35506C3.46012 4.2492 3.0134 4.43595 2.78819 4.81484L1.42399 7.10993C1.17816 7.52353 1.2617 8.05356 1.62284 8.37147L3.41297 9.9474V12.0526L1.6234 13.6285C1.26234 13.9464 1.17885 14.4764 1.42466 14.8899L2.78891 17.1851C3.01412 17.564 3.46085 17.7508 3.88871 17.6449L6.44092 17.0134L7.91861 17.8421L8.62398 20.2781C8.74779 20.7057 9.13934 21 9.58454 21H12.4163C12.8614 21 13.253 20.7057 13.3768 20.2781L14.0822 17.8421L15.5591 17.0134L18.1113 17.6449C18.5392 17.7508 18.9859 17.564 19.2111 17.1851L20.6011 14.8466C20.8352 14.4528 20.7717 13.9502 20.4471 13.627L18.6409 11.8287L18.6416 10.1713L20.4478 8.37298C20.7725 8.04974 20.836 7.54721 20.6019 7.15339L19.2118 4.81484C18.9866 4.43595 18.5399 4.2492 18.112 4.35506L15.5598 4.98656L14 4L13.2279 1.68377C13.0918 1.27543 12.7097 1 12.2792 1H9.72084C9.29034 1 8.90823 1.27543 8.77212 1.68377Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
sort : Nri.Ui.Svg.V1.Svg
sort =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M21 11C21 5.47715 16.5228 1 11 1C5.47715 1 1 5.47715 1 11C1 16.5228 5.47715 21 11 21C16.5228 21 21 16.5228 21 11Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M8 11H14.0001"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M9 14.5H13"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M7 7.5H15"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
emptyCalendar : Nri.Ui.Svg.V1.Svg
emptyCalendar =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.path
            [ Attributes.d "M15 1V5M7 1V5"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M12 3H10C6.22876 3 4.34315 3 3.17157 4.17157C2 5.34315 2 7.22876 2 11V13C2 16.7712 2 18.6569 3.17157 19.8284C4.34315 21 6.22876 21 10 21H12C15.7712 21 17.6569 21 18.8284 19.8284C20 18.6569 20 16.7712 20 13V11C20 7.22876 20 5.34315 18.8284 4.17157C17.6569 3 15.7712 3 12 3Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M2 9H20"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
calendar : Nri.Ui.Svg.V1.Svg
calendar =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.path
            [ Attributes.d "M15 1V5M7 1V5"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M12 3H10C6.22876 3 4.34315 3 3.17157 4.17157C2 5.34315 2 7.22876 2 11V13C2 16.7712 2 18.6569 3.17157 19.8284C4.34315 21 6.22876 21 10 21H12C15.7712 21 17.6569 21 18.8284 19.8284C20 18.6569 20 16.7712 20 13V11C20 7.22876 20 5.34315 18.8284 4.17157C17.6569 3 15.7712 3 12 3Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M2 9H20"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M10.9955 13H11.0045M10.9955 17H11.0045M14.991 13H15M7 13H7.00897M7 17H7.00897"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
missingDocument : Nri.Ui.Svg.V1.Svg
missingDocument =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M14 14C12.8954 14 12 14.8954 12 16V21"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M1 1L21 21"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M5.02496 1H16.9969C18.1014 1 18.9969 1.89543 18.9969 3V14.0145L18.5052 14.5052M3.02496 2.99688L3 18.9984C2.99828 20.1042 3.89421 21.0015 5 21.0015H11.9955L16.5031 16.5031"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
document : Nri.Ui.Svg.V1.Svg
document =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M7 16H15"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M7 12H11"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M19 19V8L12 1H5C3.89543 1 3 1.89543 3 3V19C3 20.1046 3.89543 21 5 21H17C18.1046 21 19 20.1046 19 19Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M12 1V6C12 7.10457 12.8954 8 14 8H19"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
documents : Nri.Ui.Svg.V1.Svg
documents =
    Nri.Ui.Svg.V1.init "0 0 24 24"
        [ Svg.path
            [ Attributes.fill "none"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.d "M14.4998 19H12.4998C9.67139 19 8.25718 19 7.3785 18.1213C6.49982 17.2426 6.49982 15.8284 6.49982 13V8C6.49982 5.17157 6.49982 3.75736 7.3785 2.87868C8.25718 2 9.67139 2 12.4998 2H13.843C14.6605 2 15.0692 2 15.4368 2.15224C15.8043 2.30448 16.0933 2.59351 16.6714 3.17157L19.3282 5.82843C19.9063 6.40648 20.1953 6.69552 20.3476 7.06306C20.4998 7.4306 20.4998 7.83935 20.4998 8.65685V13C20.4998 15.8284 20.4998 17.2426 19.6211 18.1213C18.7425 19 17.3282 19 14.4998 19Z"
            ]
            []
        , Svg.path
            [ Attributes.fill "none"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.d "M14.9998 2.5V3.5C14.9998 5.38562 14.9998 6.32843 15.5856 6.91421C16.1714 7.5 17.1142 7.5 18.9998 7.5H19.9998"
            ]
            []
        , Svg.path
            [ Attributes.fill "none"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.d "M6.49942 5C4.84257 5 3.49942 6.34315 3.49942 8V16C3.49942 18.8285 3.49942 20.2427 4.3781 21.1213C5.25678 22 6.67099 22 9.49942 22H14.4998C16.1566 22 17.4998 20.6568 17.4998 19"
            ]
            []
        , Svg.path
            [ Attributes.fill "none"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.d "M10 11H14M10 15H17"
            ]
            []
        ]


{-| -}
leaderboard : Nri.Ui.Svg.V1.Svg
leaderboard =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.clipPath "url(#clip0_48_72)" ]
            [ Svg.path
                [ Attributes.d "M2.5 17C2.5 15.5858 2.5 14.8787 2.93934 14.4393C3.37868 14 4.08579 14 5.5 14H6C6.94281 14 7.41421 14 7.70711 14.2929C8 14.5858 8 15.0572 8 16V21H2.5V17Z"
                , Attributes.stroke "currentColor"
                , Attributes.strokeWidth "2"
                , Attributes.strokeLinecap "round"
                , Attributes.strokeLinejoin "round"
                , Attributes.fill "none"
                ]
                []
            , Svg.path
                [ Attributes.d "M14 18C14 17.0572 14 16.5858 14.2929 16.2929C14.5858 16 15.0572 16 16 16H16.5C17.9142 16 18.6213 16 19.0607 16.4393C19.5 16.8787 19.5 17.5858 19.5 19V21H14V18Z"
                , Attributes.stroke "currentColor"
                , Attributes.strokeWidth "2"
                , Attributes.strokeLinecap "round"
                , Attributes.strokeLinejoin "round"
                , Attributes.fill "none"
                ]
                []
            , Svg.path
                [ Attributes.d "M1 21H21"
                , Attributes.stroke "currentColor"
                , Attributes.strokeWidth "2"
                , Attributes.strokeLinecap "round"
                , Attributes.strokeLinejoin "round"
                , Attributes.fill "none"
                ]
                []
            , Svg.path
                [ Attributes.d "M8 15C8 13.5858 8 12.8787 8.43934 12.4393C8.87868 12 9.5858 12 11 12C12.4142 12 13.1213 12 13.5607 12.4393C14 12.8787 14 13.5858 14 15V21H8V15Z"
                , Attributes.stroke "currentColor"
                , Attributes.strokeWidth "2"
                , Attributes.strokeLinecap "round"
                , Attributes.strokeLinejoin "round"
                , Attributes.fill "none"
                ]
                []
            , Svg.path
                [ Attributes.d "M11.6911 1.57767L12.395 2.99715C12.491 3.19475 12.7469 3.38428 12.9629 3.42057L14.2388 3.6343C15.0547 3.77141 15.2467 4.36824 14.6587 4.957L13.6668 5.95709C13.4989 6.12646 13.4069 6.4531 13.4589 6.68699L13.7428 7.925C13.9668 8.90492 13.4509 9.284 12.591 8.77185L11.3951 8.05808C11.1791 7.92903 10.8232 7.92903 10.6032 8.05808L9.4073 8.77185C8.5514 9.284 8.03146 8.90089 8.25543 7.925L8.5394 6.68699C8.5914 6.4531 8.49941 6.12646 8.33143 5.95709L7.33954 4.957C6.7556 4.36824 6.94358 3.77141 7.75949 3.6343L9.0353 3.42057C9.2473 3.38428 9.5033 3.19475 9.5993 2.99715L10.3032 1.57767C10.6872 0.807442 11.3111 0.807442 11.6911 1.57767Z"
                , Attributes.stroke "currentColor"
                , Attributes.strokeWidth "2"
                , Attributes.strokeLinecap "round"
                , Attributes.strokeLinejoin "round"
                , Attributes.fill "none"
                ]
                []
            ]
        ]


{-| -}
class : Nri.Ui.Svg.V1.Svg
class =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M14.5 10C14.5 8.067 12.933 6.5 11 6.5C9.067 6.5 7.5 8.067 7.5 10C7.5 11.933 9.067 13.5 11 13.5C12.933 13.5 14.5 11.933 14.5 10Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M14.4827 10.3499C14.8047 10.4475 15.1462 10.5 15.5 10.5C17.433 10.5 19 8.933 19 7C19 5.067 17.433 3.5 15.5 3.5C13.6851 3.5 12.1928 4.8814 12.0173 6.65013"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M9.9827 6.65013C9.8072 4.8814 8.31492 3.5 6.5 3.5C4.567 3.5 3 5.067 3 7C3 8.933 4.567 10.5 6.5 10.5C6.85381 10.5 7.19535 10.4475 7.51727 10.3499"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M21 15.5C21 12.7386 18.5376 10.5 15.5 10.5"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M16.5 18.5C16.5 15.7386 14.0376 13.5 11 13.5C7.96243 13.5 5.5 15.7386 5.5 18.5"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M6.5 10.5C3.46243 10.5 1 12.7386 1 15.5"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
person : Nri.Ui.Svg.V1.Svg
person =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M16 7.5C16 4.73858 13.7614 2.5 11 2.5C8.23858 2.5 6 4.73858 6 7.5C6 10.2614 8.23858 12.5 11 12.5C13.7614 12.5 16 10.2614 16 7.5Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M18 19.5C18 15.634 14.866 12.5 11 12.5C7.13401 12.5 4 15.634 4 19.5"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
couple : Nri.Ui.Svg.V1.Svg
couple =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M12 10C12 7.79086 10.2091 6 8 6C5.79086 6 4 7.79086 4 10C4 12.2091 5.79086 14 8 14C10.2091 14 12 12.2091 12 10Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M10.0386 6.55773C10.0131 6.37547 10 6.18927 10 6C10 3.79086 11.7909 2 14 2C16.2091 2 18 3.79086 18 6C18 8.20914 16.2091 10 14 10C13.2554 10 12.5584 9.7966 11.9614 9.4423"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M14 20C14 16.6863 11.3137 14 8 14C4.68629 14 2 16.6863 2 20"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M20 16C20 12.6863 17.3137 10 14 10"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
clock : Nri.Ui.Svg.V1.Svg
clock =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.path
            [ Attributes.d "M11 21C16.5228 21 21 16.5228 21 11C21 5.47715 16.5228 1 11 1C5.47715 1 1 5.47715 1 11C1 16.5228 5.47715 21 11 21Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M11 7V11L13 13"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
flipper : Nri.Ui.Svg.V1.Svg
flipper =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.path
            [ Attributes.fillRule "evenodd"
            , Attributes.clipRule "evenodd"
            , Attributes.d "M6.99994 18.5476C7.27619 18.5476 7.49994 18.7713 7.49994 19.0476C7.49994 19.3238 7.27619 19.5476 6.99994 19.5476H5.49994C3.57019 19.5476 1.99994 17.9773 1.99994 16.0476V13.7546L0.853438 14.9011C0.658188 15.0963 0.341688 15.0963 0.146438 14.9011C-0.0488125 14.7058 -0.0488125 14.3893 0.146438 14.1941L2.14644 12.1941C2.34169 11.9988 2.65819 11.9988 2.85344 12.1941L4.85344 14.1941C5.04869 14.3893 5.04869 14.7058 4.85344 14.9011C4.75569 14.9988 4.62794 15.0476 4.49994 15.0476C4.37194 15.0476 4.24419 14.9988 4.14644 14.9011L2.99994 13.7546V16.0476C2.99994 17.4266 4.12094 18.5476 5.49994 18.5476H6.99994Z"
            , Attributes.fill "currentColor"
            ]
            []
        , Svg.path
            [ Attributes.fillRule "evenodd"
            , Attributes.clipRule "evenodd"
            , Attributes.d "M21.8535 6.21793C21.6583 6.02268 21.3418 6.02268 21.1465 6.21793L20 7.36443V5.07143C20 3.14168 18.4298 1.57143 16.5 1.57143H15C14.7235 1.57143 14.5 1.79518 14.5 2.07143C14.5 2.34793 14.7235 2.57143 15 2.57143H16.5C17.879 2.57143 19 3.69243 19 5.07143V7.36443L17.8535 6.21793C17.6582 6.02268 17.3418 6.02268 17.1465 6.21793C16.9513 6.41318 16.9513 6.72968 17.1465 6.92493L19.1465 8.92493C19.244 9.02268 19.372 9.07143 19.5 9.07143C19.628 9.07143 19.7558 9.02268 19.8535 8.92493L21.8535 6.92493C22.0488 6.72968 22.0488 6.41318 21.8535 6.21793Z"
            , Attributes.fill "currentColor"
            ]
            []
        , Svg.path
            [ Attributes.d "M14.3171 14.3171C13.6726 14.9616 12.6354 14.9616 10.5608 14.9616C8.48622 14.9616 7.44894 14.9616 6.80446 14.3171C6.15997 13.6727 6.15997 12.6354 6.15997 10.5608C6.15997 8.48625 6.15997 7.44897 6.80446 6.80449C7.44894 6.16 8.48622 6.16 10.5608 6.16C12.6353 6.16 13.6726 6.16 14.3171 6.80449C14.9616 7.44897 14.9616 8.48625 14.9616 10.5608C14.9616 12.6354 14.9616 13.6727 14.3171 14.3171Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M9.17108 14.9616V6.16"
            , Attributes.stroke "currentColor"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M14.9616 8.70784H6.15997"
            , Attributes.stroke "currentColor"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M14.9616 12.4138H6.15997"
            , Attributes.stroke "currentColor"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
arrowTop : Nri.Ui.Svg.V1.Svg
arrowTop =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M17 14L11 8L5 14"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "3"
            , Attributes.strokeMiterlimit "16"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
arrowRight : Nri.Ui.Svg.V1.Svg
arrowRight =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M8.00005 5L14 11L8 17"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "3"
            , Attributes.strokeMiterlimit "16"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
arrowDown : Nri.Ui.Svg.V1.Svg
arrowDown =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M4.99976 8.00005L10.9998 14L16.9998 8"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "3"
            , Attributes.strokeMiterlimit "16"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
arrowLeft : Nri.Ui.Svg.V1.Svg
arrowLeft =
    -- If changing the path or viewport, please verify that AnimatedIcon.arrowRightDown and arrowDownUp do not regress.
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M14 5L8 11.0001L14 17"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "3"
            , Attributes.strokeMiterlimit "16"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
arrowPointingRight : Nri.Ui.Svg.V1.Svg
arrowPointingRight =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.path
            [ Attributes.d "M19.0001 10.9998H3.00012"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            ]
            []
        , Svg.path
            [ Attributes.d "M14.0003 16C14.0003 16 19.0002 12.3176 19.0002 11C19.0002 9.6824 14.0002 6 14.0002 6"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
sortArrow : Nri.Ui.Svg.V1.Svg
sortArrow =
    Nri.Ui.Svg.V1.init "0 0 16 14"
        [ Svg.path
            [ Attributes.d "M1.93884 13.5C0.392881 13.5 -0.529211 11.772 0.328354 10.482L6.38952 1.36437C7.15567 0.211875 8.84433 0.211875 9.61048 1.36438L15.6716 10.482C16.5292 11.772 15.6071 13.5 14.0612 13.5H1.93884Z"
            , Attributes.fill "currentColor"
            ]
            []
        ]


{-| -}
sortArrowDown : Nri.Ui.Svg.V1.Svg
sortArrowDown =
    Nri.Ui.Svg.V1.init "0 0 16 14"
        [ Svg.path
            [ Attributes.d "M14.0612 0.5C15.6071 0.5 16.5292 2.22796 15.6716 3.51797L9.61048 12.6356C8.84433 13.7881 7.15567 13.7881 6.38952 12.6356L0.328354 3.51797C-0.529211 2.22796 0.392881 0.5 1.93884 0.5H14.0612Z"
            , Attributes.fill "currentColor"
            ]
            []
        ]


{-| -}
checkmark : Nri.Ui.Svg.V1.Svg
checkmark =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M4 13L7.5 16.5L18 5.5"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
checkmarkInCircle : Nri.Ui.Svg.V1.Svg
checkmarkInCircle =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.circle
            [ Attributes.cx "10.75"
            , Attributes.cy "10.75"
            , Attributes.r "10.75"
            , Attributes.fill "white"
            ]
            []
        , Svg.path
            [ Attributes.d "M10.75 21.5C4.81294 21.5 0 16.6871 0 10.75C0 4.81294 4.81294 0 10.75 0C16.6871 0 21.5 4.81294 21.5 10.75C21.5 16.6871 16.6871 21.5 10.75 21.5ZM15.5182 8.39018C15.8718 7.9659 15.8145 7.33534 15.3902 6.98177C14.9659 6.62821 14.3353 6.68553 13.9818 7.10981L9.6828 12.2686L7.45711 10.0429C7.06658 9.6524 6.43342 9.6524 6.04289 10.0429C5.65237 10.4334 5.65237 11.0666 6.04289 11.4571L9.0429 14.4571C9.2416 14.6558 9.5146 14.7617 9.7953 14.749C10.076 14.7362 10.3384 14.606 10.5182 14.3902L15.5182 8.39018Z"
            , Attributes.fill "currentColor"
            , Attributes.fillRule "evenodd"
            , Attributes.clipRule "evenodd"
            ]
            []
        ]


{-| -}
checkmarkInCircleInverse : Nri.Ui.Svg.V1.Svg
checkmarkInCircleInverse =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.path
            [ Attributes.d "M21 11C21 5.47715 16.5228 1 11 1C5.47715 1 1 5.47715 1 11C1 16.5228 5.47715 21 11 21C16.5228 21 21 16.5228 21 11Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M7 11.5L9.5 14L15 8"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
emptyCircle : Nri.Ui.Svg.V1.Svg
emptyCircle =
    Nri.Ui.Svg.V1.init "0 0 24 24"
        [ Svg.circle
            [ Attributes.cx "12"
            , Attributes.cy "12"
            , Attributes.r "10"
            , Attributes.fill "none"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinejoin "round"
            ]
            []
        ]


{-| -}
x : Nri.Ui.Svg.V1.Svg
x =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M4.00049 3.99988L18.0005 17.9999"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M18.0005 3.99988L4.00049 17.9999"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
xInCircle : Nri.Ui.Svg.V1.Svg
xInCircle =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M14.5 14.5L7.50073 7.5M7.50148 14.5L14.5008 7.5"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M21 11C21 5.47715 16.5228 1 11 1C5.47715 1 1 5.47715 1 11C1 16.5228 5.47715 21 11 21C16.5228 21 21 16.5228 21 11Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
exclamation : Nri.Ui.Svg.V1.Svg
exclamation =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.circle
            [ Attributes.cx "10.75"
            , Attributes.cy "10.75"
            , Attributes.r "10.75"
            , Attributes.fill "white"
            ]
            []
        , Svg.path
            [ Attributes.d "M0 10.75C0 4.81294 4.81294 0 10.75 0C16.6871 0 21.5 4.81294 21.5 10.75C21.5 16.6871 16.6871 21.5 10.75 21.5C4.81294 21.5 0 16.6871 0 10.75ZM10.75 6.25C11.3023 6.25 11.75 6.69772 11.75 7.25V11.25C11.75 11.8023 11.3023 12.25 10.75 12.25C10.1977 12.25 9.75 11.8023 9.75 11.25V7.25C9.75 6.69772 10.1977 6.25 10.75 6.25ZM10.75 13.25C10.1977 13.25 9.75 13.6977 9.75 14.25C9.75 14.8023 10.1977 15.25 10.75 15.25H10.759C11.3113 15.25 11.759 14.8023 11.759 14.25C11.759 13.6977 11.3113 13.25 10.759 13.25H10.75Z"
            , Attributes.fill "currentColor"
            , Attributes.fillRule "evenodd"
            , Attributes.clipRule "evenodd"
            ]
            []
        ]


{-| -}
attention : Nri.Ui.Svg.V1.Svg
attention =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M11 21C16.5228 21 21 16.5228 21 11C21 5.47715 16.5228 1 11 1C5.47715 1 1 5.47715 1 11C1 16.5228 5.47715 21 11 21Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M11 7.5V11.5"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M11 14.5H11.009"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
flag : Nri.Ui.Svg.V1.Svg
flag =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.path
            [ Attributes.d "M3 6V20"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M10.7576 2.90865C7.45236 1.22497 4.85125 2.21144 3.55426 3.2192C3.32048 3.40085 3.20358 3.49167 3.10179 3.69967C3 3.90767 3 4.10138 3 4.4888V13.7319C3.9697 12.6342 6.87879 10.9328 10.7576 12.9086C14.224 14.6744 17.1741 13.9424 18.5697 13.1795C18.7633 13.0737 18.8601 13.0207 18.9301 12.9028C19 12.7849 19 12.6569 19 12.4009V4.87389C19 4.04538 19 3.63113 18.8027 3.48106C18.6053 3.33099 18.1436 3.459 17.2202 3.71504C15.64 4.15319 13.3423 4.22532 10.7576 2.90865Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
star : Nri.Ui.Svg.V1.Svg
star =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.path
            [ Attributes.d "M12.7276 2.44418L14.4874 5.99288C14.7274 6.48687 15.3673 6.9607 15.9073 7.05143L19.0969 7.58575C21.1367 7.92853 21.6167 9.4206 20.1468 10.8925L17.6671 13.3927C17.2471 13.8161 17.0172 14.6327 17.1471 15.2175L17.8571 18.3125C18.417 20.7623 17.1271 21.71 14.9774 20.4296L11.9877 18.6452C11.4478 18.3226 10.5579 18.3226 10.0079 18.6452L7.01827 20.4296C4.8785 21.71 3.57865 20.7522 4.13859 18.3125L4.84851 15.2175C4.97849 14.6327 4.74852 13.8161 4.32856 13.3927L1.84884 10.8925C0.388999 9.4206 0.85895 7.92853 2.89872 7.58575L6.08837 7.05143C6.61831 6.9607 7.25824 6.48687 7.49821 5.99288L9.258 2.44418C10.2179 0.518607 11.7777 0.518607 12.7276 2.44418Z"
            , Attributes.fill "#FEC709"
            , Attributes.stroke "#004e95"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            ]
            []
        ]


{-| -}
starFilled : Nri.Ui.Svg.V1.Svg
starFilled =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.path
            [ Attributes.d "M12.7276 2.44418L14.4874 5.99288C14.7274 6.48687 15.3673 6.9607 15.9073 7.05143L19.0969 7.58575C21.1367 7.92853 21.6167 9.4206 20.1468 10.8925L17.6671 13.3927C17.2471 13.8161 17.0172 14.6327 17.1471 15.2175L17.8571 18.3125C18.417 20.7623 17.1271 21.71 14.9774 20.4296L11.9877 18.6452C11.4478 18.3226 10.5579 18.3226 10.0079 18.6452L7.01827 20.4296C4.8785 21.71 3.57865 20.7522 4.13859 18.3125L4.84851 15.2175C4.97849 14.6327 4.74852 13.8161 4.32856 13.3927L1.84884 10.8925C0.388999 9.4206 0.85895 7.92853 2.89872 7.58575L6.08837 7.05143C6.61831 6.9607 7.25824 6.48687 7.49821 5.99288L9.258 2.44418C10.2179 0.518607 11.7777 0.518607 12.7276 2.44418Z"
            , Attributes.fill "currentColor"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            ]
            []
        ]


{-| -}
starOutline : Nri.Ui.Svg.V1.Svg
starOutline =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M12.7276 2.44418L14.4874 5.99288C14.7274 6.48687 15.3673 6.9607 15.9073 7.05143L19.0969 7.58575C21.1367 7.92853 21.6167 9.4206 20.1468 10.8925L17.6671 13.3927C17.2471 13.8161 17.0172 14.6327 17.1471 15.2175L17.8571 18.3125C18.417 20.7623 17.1271 21.71 14.9774 20.4296L11.9877 18.6452C11.4478 18.3226 10.5579 18.3226 10.0079 18.6452L7.01827 20.4296C4.8785 21.71 3.57865 20.7522 4.13859 18.3125L4.84851 15.2175C4.97849 14.6327 4.74852 13.8161 4.32856 13.3927L1.84884 10.8925C0.388999 9.4206 0.85895 7.92853 2.89872 7.58575L6.08837 7.05143C6.61831 6.9607 7.25824 6.48687 7.49821 5.99288L9.258 2.44418C10.2179 0.518607 11.7777 0.518607 12.7276 2.44418Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
no : Nri.Ui.Svg.V1.Svg
no =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M4.04004 3.8L17.48 17.24"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M20.36 10.52C20.36 5.21806 16.0619 0.919998 10.76 0.919998C5.4581 0.919998 1.16003 5.21806 1.16003 10.52C1.16003 15.8219 5.4581 20.12 10.76 20.12C16.0619 20.12 20.36 15.8219 20.36 10.52Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
compass : Nri.Ui.Svg.V1.Svg
compass =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M21 11C21 5.47715 16.5228 1 11 1C5.47715 1 1 5.47715 1 11C1 16.5228 5.47715 21 11 21C16.5228 21 21 16.5228 21 11Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M11.4014 7.29796L14.3213 6.32465C15.2075 6.02924 15.6507 5.88153 15.8846 6.11544C16.1185 6.34935 15.9708 6.79247 15.6753 7.67871L14.702 10.5986C14.1986 12.1088 13.9469 12.8639 13.4054 13.4054C12.8639 13.9469 12.1088 14.1986 10.5986 14.702L7.67871 15.6753C6.79247 15.9708 6.34935 16.1185 6.11544 15.8846C5.88153 15.6507 6.02924 15.2075 6.32465 14.3213L7.29796 11.4014C7.80136 9.8912 8.05306 9.1361 8.59457 8.59457C9.1361 8.05306 9.8912 7.80136 11.4014 7.29796Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M11.0001 11L10.9937 11.0064"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
activity : Nri.Ui.Svg.V1.Svg
activity =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ]
            [ Svg.path
                [ Attributes.d "M5 11H7L9 7L13 15L15 11H17"
                , Attributes.stroke "currentColor"
                , Attributes.strokeWidth "2"
                , Attributes.strokeLinecap "round"
                , Attributes.strokeLinejoin "round"
                ]
                []
            , Svg.path
                [ Attributes.d "M20.5 3.5V18.5C20.5 19.6046 19.6046 20.5 18.5 20.5H3.5C2.39543 20.5 1.5 19.6046 1.5 18.5V3.5C1.5 2.39543 2.39543 1.5 3.5 1.5H18.5C19.6046 1.5 20.5 2.39543 20.5 3.5Z"
                , Attributes.stroke "currentColor"
                , Attributes.strokeWidth "2"
                , Attributes.strokeLinejoin "round"
                ]
                []
            ]
        ]


{-| -}
footsteps : Nri.Ui.Svg.V1.Svg
footsteps =
    Nri.Ui.Svg.V1.init "0 0 15 20"
        [ Svg.path
            [ Attributes.fillRule "nonzero"
            , Attributes.d "M1.77335904,11.7922305 L5.90660307,11.0289328 C5.68935769,12.5383477 7.40861515,15.2884313 5.45646759,16.0478489 C2.76105632,17.0954131 1.9234042,14.6781572 1.77335904,11.7922305 Z M0.176718476,7.06612115 C0.458843391,8.43725287 1.41615152,9.74198306 1.69435526,11.1030145 L6.15429763,10.2795555 C7.60395395,3.97240957 6.1871195,0.900338486 4.18808583,0.126920592 C2.03987926,-0.705098659 -0.729754357,2.66141923 0.176718476,7.06612115 Z M13.2274465,15.4953161 L9.09420249,14.7320184 C9.31066764,16.2422134 7.59141017,18.992317 9.54433797,19.7509345 C12.2397492,20.7984988 13.0774014,18.3812428 13.2274465,15.4953161 Z M13.3056301,14.8061401 C13.5838338,13.4443886 14.5411619,12.1404785 14.8232668,10.7692468 C15.7297797,6.36454491 12.9602061,2.99806702 10.8110592,3.83008627 C8.81279779,4.60354417 7.39603137,7.67557525 8.84562767,13.9827211 L13.3056301,14.8061401 Z"
            ]
            []
        ]


{-| -}
footstepsVideo : Nri.Ui.Svg.V1.Svg
footstepsVideo =
    Nri.Ui.Svg.V1.init "0 0 28 28"
        [ Svg.path
            [ Attributes.d "M4.92672 12.9714L9.41715 12.1318C9.18113 13.7921 11.049 16.8172 8.92812 17.6526C5.99977 18.8049 5.08973 16.1459 4.92672 12.9714Z"
            , Attributes.fill "currentColor"
            ]
            []
        , Svg.path
            [ Attributes.d "M3.19199 7.77273C3.4985 9.28097 4.53853 10.7162 4.84078 12.2133L9.68615 11.3075C11.2611 4.36965 9.72181 0.990372 7.55002 0.139613C5.21616 -0.775608 2.20718 2.92756 3.19199 7.77273Z"
            , Attributes.fill "currentColor"
            ]
            []
        , Svg.path
            [ Attributes.d "M17.3706 17.0448L12.8801 16.2052C13.1153 17.8664 11.2475 20.8915 13.3692 21.726C16.2975 22.8783 17.2076 20.2194 17.3706 17.0448Z"
            , Attributes.fill "currentColor"
            ]
            []
        , Svg.path
            [ Attributes.d "M17.4556 16.2867C17.7578 14.7888 18.7979 13.3545 19.1044 11.8461C20.0892 7.00096 17.0803 3.29783 14.7454 4.21305C12.5745 5.06386 11.0353 8.44309 12.6102 15.3809L17.4556 16.2867Z"
            , Attributes.fill "currentColor"
            ]
            []
        , Svg.circle
            [ Attributes.cx "17.6482"
            , Attributes.cy "20.5"
            , Attributes.r "7.5"
            , Attributes.fill "white"
            ]
            []
        , Svg.circle
            [ Attributes.cx "17.6482"
            , Attributes.cy "20.5"
            , Attributes.r "6"
            , Attributes.fill "white"
            , Attributes.stroke "currentColor"
            ]
            []
        , Svg.path
            [ Attributes.d "M21.1732 20.2835C21.3398 20.3797 21.3398 20.6203 21.1732 20.7165L16.0732 23.661C15.9065 23.7572 15.6982 23.6369 15.6982 23.4445V17.5555C15.6982 17.3631 15.9065 17.2428 16.0732 17.339L21.1732 20.2835Z"
            , Attributes.fill "currentColor"
            ]
            []
        ]


{-| -}
skip : Nri.Ui.Svg.V1.Svg
skip =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M4.5241 18.0621C3.85783 18.4721 3 17.9928 3 17.2104V4.78956C3 4.00724 3.85783 3.52789 4.5241 3.93791L14.6161 10.1483C15.2506 10.5388 15.2506 11.4612 14.6161 11.8517L4.5241 18.0621Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M19 3.5V18.5"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
playInCircle : Nri.Ui.Svg.V1.Svg
playInCircle =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M11 21C16.5228 21 21 16.5228 21 11C21 5.47715 16.5228 1 11 1C5.47715 1 1 5.47715 1 11C1 16.5228 5.47715 21 11 21Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M14.4527 10.073L10.125 7.64224C9.4009 7.23554 8.5 7.74946 8.5 8.5692V13.4308C8.5 14.2505 9.4009 14.7645 10.125 14.3578L14.4527 11.927C15.1824 11.5171 15.1824 10.4829 14.4527 10.073Z"
            , Attributes.fill "currentColor"
            ]
            []
        ]


{-| -}
play : Nri.Ui.Svg.V1.Svg
play =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M6.5241 18.0621C5.85783 18.4721 5 17.9928 5 17.2104V4.78956C5 4.00724 5.85783 3.52789 6.5241 3.93791L16.6161 10.1483C17.2506 10.5388 17.2506 11.4612 16.6161 11.8517L6.5241 18.0621Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "currentColor"
            ]
            []
        ]


{-| -}
stopInCircle : Nri.Ui.Svg.V1.Svg
stopInCircle =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M11 21C16.5228 21 21 16.5228 21 11C21 5.47715 16.5228 1 11 1C5.47715 1 1 5.47715 1 11C1 16.5228 5.47715 21 11 21Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M13 7.5H9C8.17157 7.5 7.5 8.17157 7.5 9V13C7.5 13.8284 8.17157 14.5 9 14.5H13C13.8284 14.5 14.5 13.8284 14.5 13V9C14.5 8.17157 13.8284 7.5 13 7.5Z"
            , Attributes.fill "currentColor"
            ]
            []
        ]


{-| -}
pauseInCircle : Nri.Ui.Svg.V1.Svg
pauseInCircle =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M11 21C16.5228 21 21 16.5228 21 11C21 5.47715 16.5228 1 11 1C5.47715 1 1 5.47715 1 11C1 16.5228 5.47715 21 11 21Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M8.5 8V14M13.5 8V14"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
hamburger : Nri.Ui.Svg.V1.Svg
hamburger =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.path
            [ Attributes.d "M3 4H19"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            ]
            []
        , Svg.path
            [ Attributes.d "M3 11H19"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            ]
            []
        , Svg.path
            [ Attributes.d "M3 18H19"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            ]
            []
        ]


{-| -}
kebab : Nri.Ui.Svg.V1.Svg
kebab =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.path
            [ Attributes.d "M9 11C9 9.89543 9.89313 9 10.9949 9H11.0051C12.1069 9 13 9.89543 13 11C13 12.1046 12.1069 13 11.0051 13H10.9949C9.89313 13 9 12.1046 9 11Z"
            , Attributes.fill "currentColor"
            , Attributes.fillRule "evenodd"
            , Attributes.clipRule "evenodd"
            ]
            []
        , Svg.path
            [ Attributes.d "M9 18C9 16.8954 9.89313 16 10.9949 16H11.0051C12.1069 16 13 16.8954 13 18C13 19.1046 12.1069 20 11.0051 20H10.9949C9.89313 20 9 19.1046 9 18Z"
            , Attributes.fill "currentColor"
            , Attributes.fillRule "evenodd"
            , Attributes.clipRule "evenodd"
            ]
            []
        , Svg.path
            [ Attributes.d "M9 4C9 2.89543 9.89313 2 10.9949 2H11.0051C12.1069 2 13 2.89543 13 4C13 5.10457 12.1069 6 11.0051 6H10.9949C9.89313 6 9 5.10457 9 4Z"
            , Attributes.fill "currentColor"
            , Attributes.fillRule "evenodd"
            , Attributes.clipRule "evenodd"
            ]
            []
        ]


{-| -}
equals : Nri.Ui.Svg.V1.Svg
equals =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M3 7H19"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            ]
            []
        , Svg.path
            [ Attributes.d "M3 15H19"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            ]
            []
        ]


{-| -}
plus : Nri.Ui.Svg.V1.Svg
plus =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M11 3V19M19 11H3"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
sparkleBulb : Nri.Ui.Svg.V1.Svg
sparkleBulb =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M13.9061 16.4694C13.9655 16.1726 14.1583 15.922 14.4176 15.7659C16.2676 14.6519 17.5 12.8361 17.5 10.5C17.5 6.91015 14.5899 4 11 4C7.41015 4 4.5 6.91015 4.5 10.5C4.5 12.8361 5.73235 14.6519 7.58241 15.7659C7.84173 15.922 8.03452 16.1726 8.09388 16.4694L8.33922 17.6961C8.43271 18.1635 8.84312 18.5 9.3198 18.5H12.6802C13.1569 18.5 13.5673 18.1635 13.6608 17.6961L13.9061 16.4694Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M20 10.5H20.5M1.5 10.5H2M17.3633 4.13604L17.7168 3.78249M4 17.5L4.5 17M17.5 17L18 17.5M4.2832 3.78319L4.63676 4.13674M11 1.5V1"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M13 18.5V20C13 20.5523 12.5523 21 12 21H10C9.4477 21 9 20.5523 9 20V18.5"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
baldBulb : Nri.Ui.Svg.V1.Svg
baldBulb =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.clipPath "url(#clip0_1340_663)" ]
            [ Svg.path
                [ Attributes.fillRule "evenodd"
                , Attributes.clipRule "evenodd"
                , Attributes.d "M11 2.42914C7.05258 2.42914 3.87035 5.56231 3.87035 9.4057C3.87035 10.7204 4.24103 11.9478 4.88585 12.9965C5.17834 13.4722 5.02594 14.0925 4.54545 14.3821C4.06496 14.6717 3.43834 14.5208 3.14585 14.0451C2.31278 12.6902 1.83331 11.1016 1.83331 9.4057C1.83331 4.42919 5.94718 0.412476 11 0.412476C16.0527 0.412476 20.1666 4.42919 20.1666 9.4057C20.1666 11.1016 19.6871 12.6902 18.8541 14.0451C18.5617 14.5208 17.935 14.6717 17.4545 14.3821C16.974 14.0925 16.8216 13.4722 17.1141 12.9965C17.759 11.9478 18.1296 10.7204 18.1296 9.4057C18.1296 5.56231 14.9474 2.42914 11 2.42914Z"
                , Attributes.fill "currentColor"
                ]
                []
            , Svg.path
                [ Attributes.d "M14.9144 14.7812C15.0472 14.7812 15.1782 14.7813 15.2885 14.7863C15.4051 14.7915 15.5616 14.8042 15.7248 14.853C16.2325 15.0048 16.5746 15.3724 16.6005 15.7894C16.6089 15.9233 16.5753 16.0413 16.5449 16.1281C16.2074 17.824 15.5105 18.3103 14.9143 18.3103H7.08572C6.48949 18.3103 5.79261 17.824 5.45502 16.1281C5.42476 16.0413 5.3911 15.9233 5.39942 15.7894C5.42533 15.3724 5.76745 15.0048 6.27518 14.853C6.43832 14.8042 6.59492 14.7915 6.71147 14.7863C6.82183 14.7813 6.95277 14.7812 7.0856 14.7812H14.9144Z"
                , Attributes.fill "currentColor"
                ]
                []
            , Svg.path
                [ Attributes.d "M9.30124 21.488C9.65293 21.5893 10.0687 21.5893 10.9001 21.5893C11.7315 21.5893 12.1473 21.5893 12.499 21.488C13.0429 21.3312 13.5006 21.0031 13.783 20.5675C13.8881 20.4053 13.9598 20.2185 14.0328 19.9531C14.1242 19.6203 14.17 19.454 14.0783 19.3352C13.9865 19.2163 13.8061 19.2163 13.4454 19.2163H8.35488C7.9941 19.2163 7.81371 19.2163 7.72198 19.3352C7.63027 19.454 7.676 19.6203 7.76748 19.9531C7.84044 20.2185 7.91207 20.4053 8.01721 20.5675C8.29969 21.0031 8.75737 21.3312 9.30124 21.488Z"
                , Attributes.fill "currentColor"
                ]
                []
            , Svg.path
                [ Attributes.fillRule "evenodd"
                , Attributes.clipRule "evenodd"
                , Attributes.d "M13.7394 8.7573C14.1468 9.14136 14.1626 9.77954 13.7747 10.1829C13.2725 10.705 12.6831 11.123 12.0185 11.3405V15.0334C12.0185 15.5903 11.5625 16.0417 11 16.0417C10.4375 16.0417 9.9815 15.5903 9.9815 15.0334V11.3405C9.31691 11.123 8.72754 10.705 8.22538 10.1829C7.83747 9.77954 7.85324 9.14136 8.26062 8.7573C8.66799 8.37326 9.31264 8.38889 9.70059 8.79219C10.2025 9.31399 10.6423 9.48752 11 9.48752C11.3577 9.48752 11.7975 9.31399 12.2994 8.79219C12.6874 8.38888 13.332 8.37326 13.7394 8.7573Z"
                , Attributes.fill "currentColor"
                ]
                []
            ]
        ]


{-| -}
help : Nri.Ui.Svg.V1.Svg
help =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M11 21C16.5228 21 21 16.5228 21 11C21 5.47715 16.5228 1 11 1C5.47715 1 1 5.47715 1 11C1 16.5228 5.47715 21 11 21Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "square"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M11 15.5H11.009"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M9 8.5C9 7.39543 9.8954 6.5 11 6.5C12.1046 6.5 13 7.39543 13 8.5C13 8.89815 12.8837 9.2691 12.6831 9.5808C12.0854 10.5097 11 11.3954 11 12.5"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
key : Nri.Ui.Svg.V1.Svg
key =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M14.5 13.5C17.8137 13.5 20.5 10.8137 20.5 7.5C20.5 4.18629 17.8137 1.5 14.5 1.5C11.1863 1.5 8.5 4.18629 8.5 7.5C8.5 8.38041 8.68962 9.2165 9.0303 9.9697L1.5 17.5V20.5H4.5V18.5H6.5V16.5H8.5L12.0303 12.9697C12.7835 13.3104 13.6196 13.5 14.5 13.5Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M16.5 5.5L15.5 6.5"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
lock : Nri.Ui.Svg.V1.Svg
lock =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M11 15.5V13.5"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M15.5 8V5.5C15.5 3.01472 13.4853 1 11 1C8.51472 1 6.5 3.01472 6.5 5.5V8"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M16.9999 8H5.00013C3.89548 8 3.00002 8.89554 3.00012 10.0002L3.00096 19.0002C3.00106 20.1047 3.89646 21 5.00096 21H16.9999C18.1045 21 18.9999 20.1046 18.9999 19V10C18.9999 8.89543 18.1045 8 16.9999 8Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
premiumLock : Nri.Ui.Svg.V1.Svg
premiumLock =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M15.5 8V5.5C15.5 3.01472 13.4853 1 11 1C8.51472 1 6.5 3.01472 6.5 5.5V8"
            , Attributes.stroke "#004e95"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M16.9999 8H5.00013C3.89548 8 3.00002 8.89554 3.00012 10.0002L3.00096 19.0002C3.00106 20.1047 3.89646 21 5.00096 21H16.9999C18.1045 21 18.9999 20.1046 18.9999 19V10C18.9999 8.89543 18.1045 8 16.9999 8Z"
            , Attributes.stroke "#004e95"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.fill "#FEC709"
            ]
            []
        , Svg.path
            [ Attributes.d "M11 15.5V13.5"
            , Attributes.stroke "#004e95"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
badge : Nri.Ui.Svg.V1.Svg
badge =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M3.26759 3.32782C4.95399 2.02741 7.57337 1 11 1C14.4266 1 17.046 2.02741 18.7324 3.32782C18.9693 3.51048 19.0877 3.60181 19.1849 3.76366C19.2665 3.89952 19.3252 4.10558 19.3275 4.26404C19.3302 4.4528 19.2672 4.62069 19.1413 4.95648C18.8305 5.78539 18.6751 6.19984 18.6122 6.61031C18.533 7.12803 18.5322 7.25474 18.6053 7.77338C18.6632 8.18457 18.9795 9.0598 19.6121 10.8103C19.844 11.452 20 12.1792 20 13C20 16 17.5 18.375 15 19C12.8082 19.548 11.6667 20.3333 11 21C10.3333 20.3333 9.1918 19.548 7 19C4.5 18.375 2 16 2 13C2 12.1792 2.15595 11.452 2.38785 10.8103C3.0205 9.0598 3.33682 8.18457 3.39473 7.77338C3.46777 7.25474 3.46702 7.12803 3.38777 6.61031C3.32494 6.19984 3.16952 5.78539 2.85868 4.95648C2.73276 4.62069 2.6698 4.4528 2.67252 4.26404C2.6748 4.10558 2.73351 3.89952 2.81509 3.76366C2.91227 3.60181 3.03071 3.51048 3.26759 3.32782Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M11.6911 6.57767L12.395 7.99715C12.491 8.19475 12.7469 8.38428 12.9629 8.42057L14.2388 8.6343C15.0547 8.77141 15.2467 9.3682 14.6587 9.957L13.6668 10.9571C13.4989 11.1265 13.4069 11.4531 13.4589 11.687L13.7428 12.925C13.9668 13.9049 13.4509 14.284 12.591 13.7718L11.3951 13.0581C11.1791 12.929 10.8232 12.929 10.6032 13.0581L9.4073 13.7718C8.5514 14.284 8.03146 13.9009 8.25543 12.925L8.5394 11.687C8.5914 11.4531 8.49941 11.1265 8.33143 10.9571L7.33954 9.957C6.7556 9.3682 6.94358 8.77141 7.75949 8.6343L9.0353 8.42057C9.2473 8.38428 9.5033 8.19475 9.5993 7.99715L10.3032 6.57767C10.6872 5.80744 11.3111 5.80744 11.6911 6.57767Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
gift : Nri.Ui.Svg.V1.Svg
gift =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M3 10V19C3 20.1046 3.89543 21 5 21H17C18.1046 21 19 20.1046 19 19V10"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M11 4.75V6H7.5C6.11929 6 5 4.88071 5 3.5V3.25C5 2.00736 6.00736 1 7.25 1C9.3211 1 11 2.67893 11 4.75Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M11 4.75V6H14.5C15.8807 6 17 4.88071 17 3.5V3.25C17 2.00736 15.9926 1 14.75 1C12.6789 1 11 2.67893 11 4.75Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M19.4443 6H2.56105C1.97904 6 1.50682 6.44617 1.50535 6.99747L1.5 8.99747C1.49853 9.5507 1.97161 10 2.5557 10H19.4443C20.0273 10 20.5 9.5523 20.5 9V7C20.5 6.44772 20.0273 6 19.4443 6Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M11 10V21"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
hat : Nri.Ui.Svg.V1.Svg
hat =
    Nri.Ui.Svg.V1.init "0 0 20 16"
        [ Svg.path [ Attributes.d "M10,0.1602 C5.3602,0.1602 0.4398002,1.4602 0.4398002,3.9 C0.4398002,5.26016 1.97964,6.2602 4.1398,6.8796 L4.1398,13.8796 C4.142926,14.29444 4.39526,14.66554 4.77964,14.82022 C8.13504,16.17334 11.88424,16.17334 15.23984,14.82022 C15.62422,14.665532 15.87734,14.29444 15.87968,13.8796 L15.87968,6.86 C18.03988,6.23968 19.57968,5.23968 19.57968,3.8804 C19.56015,1.46 14.63988,0.16 10.00008,0.16 L10,0.1602 Z M10,6.4204 C4.8204,6.4204 1.6398,4.94072 1.6398,3.8806 C1.6398,2.82044 4.8202,1.3602 10,1.3602 C15.1798,1.3602 18.3602,2.83988 18.3602,3.9 C18.3602,4.96012 15.1798,6.4204 10,6.4204 Z" ] []
        , Svg.path [ Attributes.d "M13.5398,2.3398 C12.38356,2.0437 11.1938,1.9023 10,1.92026 C8.80624,1.90229 7.6164,2.0437 6.4602,2.3398 C5.23988,2.67964 4.62036,3.19996 4.62036,3.87964 C4.62036,4.55932 5.24068,5.07964 6.4602,5.41948 L6.4602,5.42026 C7.61644,5.71636 8.8062,5.85776 10,5.8398 C11.19376,5.85777 12.3836,5.71636 13.5398,5.42026 C14.76012,5.08042 15.37964,4.5601 15.37964,3.88042 C15.37964,3.19996 14.7601,2.69996 13.5398,2.3398 Z" ] []
        ]


{-| -}
keychain : Nri.Ui.Svg.V1.Svg
keychain =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.path
            [ Attributes.d "M9.73684 3.73684L10.4211 4.42105M11.1053 9.21053C8.83799 9.21053 7 7.37253 7 5.10526C7 2.83799 8.83799 1 11.1053 1C13.3725 1 15.2105 2.83799 15.2105 5.10526C15.2105 5.70765 15.0808 6.27971 14.8477 6.79506L20 11.9474V14H17.9474V12.6316H16.5789V11.2632H15.2105L12.7951 8.84769C12.2797 9.0808 11.7076 9.21053 11.1053 9.21053Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M4.79627 4C4.69363 4.08753 4.59364 4.17978 4.49658 4.27675C2.50114 6.2703 2.50114 9.50251 4.49658 11.4961C4.89533 11.8944 5.34454 12.2138 5.82404 12.4534V15.6017C5.82404 15.7981 5.90212 15.9864 6.04111 16.1253L6.54675 16.6304L6.04111 17.1356C5.90212 17.2744 5.82404 17.4628 5.82404 17.6591C5.82404 17.8555 5.90212 18.0438 6.04111 18.1827L6.54675 18.6878L6.04111 19.193C5.90212 19.3319 5.82404 19.5202 5.82404 19.7165C5.82404 19.9129 5.90212 20.1012 6.04111 20.2401L7.58563 21.7831C7.87505 22.0723 8.34428 22.0723 8.6337 21.7831L10.1782 20.2401C10.3172 20.1012 10.3953 19.9129 10.3953 19.7165V12.4534C10.8748 12.2138 11.324 11.8944 11.7227 11.4961C11.8199 11.399 11.9123 11.299 12 11.1964C11.6973 11.2407 11.388 11.2637 11.0741 11.2637C10.6173 11.2637 10.1719 11.2154 9.74253 11.1238C9.62811 11.1815 9.51132 11.2326 9.39268 11.2773C9.1041 11.386 8.91309 11.6619 8.91309 11.9701V19.4099L8.10966 20.2125L7.61321 19.7165L8.11886 19.2114C8.25784 19.0725 8.33592 18.8842 8.33592 18.6878C8.33592 18.4915 8.25784 18.3031 8.11886 18.1643L7.61321 17.6591L8.11886 17.154C8.40827 16.8648 8.40827 16.396 8.11886 16.1069L7.30624 15.295V11.9701C7.30624 11.6619 7.11522 11.386 6.82663 11.2773C6.36008 11.1015 5.922 10.826 5.54465 10.449C4.40884 9.31425 4.1837 7.61422 4.86924 6.25538C4.7775 5.82634 4.72923 5.38122 4.72923 4.92483C4.72923 4.61069 4.7521 4.30189 4.79627 4Z"
            , Attributes.fill "currentColor"
            ]
            []
        ]


{-| -}
sprout : Nri.Ui.Svg.V1.Svg
sprout =
    Nri.Ui.Svg.V1.init "0 0 100 83"
        [ Svg.g [ Attributes.fillRule "nonzero" ]
            [ Svg.path
                [ Attributes.d "M66.3361328,23.0056641 L66.3361328,22.9205078 C66.3361328,21.8013672 65.146875,21.0789062 64.1560547,21.5994141 C61.0353516,23.2384766 57.9806641,25.1339844 55.1259766,27.3220703 C52.7337891,29.1554687 50.7003906,30.9380859 49.0453125,32.4488281 L49.0351563,29.2617187 C49.0351563,29.1556641 49.0974609,18.5953125 54.4810547,13.2119141 C55.09375,12.5990234 55.09375,11.6056641 54.4810547,10.9927734 C53.8681641,10.3802734 52.8748047,10.3802734 52.2619141,10.9927734 C47.7990234,15.4558594 46.4654297,22.5322266 46.0667969,26.4457031 C44.9107422,25.1599609 43.5933594,23.6900391 42.30625,22.4621094 C37.3503906,17.7333984 31.8285156,14.0056641 26.4798828,11.1158203 C25.4898438,10.5808594 24.2878906,11.3042969 24.2878906,12.4294922 L24.2878906,12.4294922 C24.2878906,12.9541016 24.5646484,13.4380859 25.0142578,13.7083984 C29.8099609,16.5931641 34.9601563,20.1953125 40.0970703,24.690625 C42.3849609,26.6927734 44.6691406,29.5775391 45.9025391,31.0087891 L45.9181641,35.8501953 C45.8878906,36.0291016 45.8878906,36.2123047 45.9199219,36.3910156 L46.0505859,69.3267578 C46.0533203,70.1916016 46.7552734,70.890625 47.6195312,70.890625 C47.6210938,70.890625 47.6230469,70.890625 47.6246094,70.890625 C48.4912109,70.8876953 49.1914062,70.1832031 49.1886719,69.3164062 L49.0591797,36.7457031 C50.8654297,34.9878906 53.5615234,32.4410156 57.0529297,29.7988281 C59.7716797,27.7412109 62.6439453,25.9228516 65.5595703,24.3132812 C66.0369141,24.0498047 66.3361328,23.5507813 66.3361328,23.0056641 Z"
                , Attributes.fill "#4D984E"
                ]
                []
            , Svg.path
                [ Attributes.d "M0,2.36015625 C7.72929687,8.24726563 14.3525391,16.0335937 22.5138672,21.2082031 C27.9757812,24.6710938 35.0310547,25.8072266 41.2017578,23.5763672 C43.83125,16.1564453 39.0654297,8.47226563 32.8615234,4.58789063 C23.5689453,-1.23027344 10.1871094,-0.90546875 0,2.36015625 Z"
                , Attributes.fill "#9CDD05"
                ]
                []
            , Svg.path
                [ Attributes.d "M100,12.1023437 C91.5154297,17.0859375 83.9130859,24.1132812 75.0804688,28.328125 C69.1693359,31.1488281 61.9376953,31.4369141 56.0029297,28.4466797 C54.2574219,20.6675781 59.9796875,13.5181641 66.6880859,10.3636719 C76.7361328,5.63828125 90.1523438,7.58515625 100,12.1023437 Z"
                , Attributes.fill "#C3EA21"
                ]
                []
            , Svg.path
                [ Attributes.d "M32.8615234,4.58789062 C23.5689453,-1.23027344 10.1871094,-0.90546875 0,2.36015625 C15.9494141,5.49433594 29.9521484,12.4251953 41.2017578,23.5761719 C43.83125,16.15625 39.0654297,8.47226563 32.8615234,4.58789062 Z"
                , Attributes.fill "#C3EA21"
                ]
                []
            , Svg.path
                [ Attributes.d "M56.1472656,28.5162109 C62.0546875,31.4300781 69.2167969,31.1259766 75.0804687,28.3279297 C83.9130859,24.1130859 91.5154297,17.0857422 100,12.1021484 C83.6082031,13.3207031 68.7960937,18.6943359 56.1472656,28.5162109 Z"
                , Attributes.fill "#9CDD05"
                ]
                []
            , Svg.path
                [ Attributes.d "M93.3470703,82.7828125 C93.3470703,78.7234375 89.3367188,74.2273438 81.6316406,73.76875 C79.3263672,67.3861328 73.19375,62.2677734 64.6273438,63.0894531 C57.7798828,45.5003906 20.8574219,48.8785156 15.4505859,73.4015625 C15.0931641,73.3667969 14.73125,73.3470703 14.3648438,73.3462891 C8.75488281,73.3355469 5.21347656,77.4210938 4.33027344,82.7828125 L93.3470703,82.7828125 Z"
                , Attributes.fill "#BA7D60"
                ]
                []
            , Svg.path
                [ Attributes.d "M55.9439453,65.7667969 C63.8917969,64.9451172 69.5816406,70.0632813 71.7205078,76.4460938 C77.3070313,76.8042969 80.8001953,79.6285156 82.0601563,82.7828125 L93.3470703,82.7828125 C93.3470703,78.7234375 89.3367188,74.2273438 81.6316406,73.76875 C79.3263672,67.3861328 73.19375,62.2677734 64.6275391,63.0894531 C61.984375,56.3 54.8591797,52.6378906 46.7962891,52.1068359 C50.6914063,53.4824219 55.0353516,57.228125 55.9439453,65.7667969 Z"
                , Attributes.fill "#A56A58"
                ]
                []
            ]
        ]


{-| -}
sapling : Nri.Ui.Svg.V1.Svg
sapling =
    Nri.Ui.Svg.V1.init "0 0 100 191"
        [ Svg.polygon [ Attributes.fill "#D97F4A", Attributes.points "44.4444444 116.260127 55.5555556 116.260127 55.5555556 182.186053 44.4444444 182.186053" ] []
        , Svg.polygon [ Attributes.fill "#D55F05", Attributes.points "50 116.260127 55.5555556 116.260127 55.5555556 182.186053 50 182.186053" ] []
        , Svg.path [ Attributes.d "M87.7777778,57.7777778 C88.4447337,55.2213541 88.8888889,52.6663774 88.8888889,50 C88.8888889,41.3324652 84.6672452,33.1105326 77.7777778,27.8891781 L77.7777778,27.7777778 C77.7777778,12.445023 65.3327548,0 50,0 C34.6672452,0 22.2222222,12.445023 22.2222222,27.7777778 L22.2222222,27.8891781 C15.3327548,33.1105326 11.1111111,41.3324652 11.1111111,50 C11.1111111,52.6663774 11.5552663,55.2213541 12.2222222,57.7777778 C4.33304407,64.8885996 0,73.7774885 0,83.3333333 C0,104.777199 22.445023,122.222222 50,122.222222 C77.554977,122.222222 100,104.777199 100,83.3333333 C100,73.7774885 95.6669559,64.8885996 87.7777778,57.7777778 Z", Attributes.fill "#C3EA21" ] []
        , Svg.path [ Attributes.d "M100,83.3333333 C100,104.777199 77.554977,122.222222 50,122.222222 L50,0 C65.3327548,0 77.7777778,12.445023 77.7777778,27.7777778 L77.7777778,27.8891781 C84.6672452,33.1105326 88.8888889,41.3324652 88.8888889,50 C88.8888889,52.6663774 88.4447337,55.2213541 87.7777778,57.7777778 C95.6669559,64.8885996 100,73.7774885 100,83.3333333 Z", Attributes.fill "#9CDD05" ] []
        , Svg.path [ Attributes.d "M27.7777778,100 C18.587963,100 11.1111111,92.5231481 11.1111111,83.3333333 C11.1111111,80.2633104 13.5966437,77.7777778 16.6666667,77.7777778 C19.7366896,77.7777778 22.2222222,80.2633104 22.2222222,83.3333333 C22.2222222,86.3990163 24.7120948,88.8888889 27.7777778,88.8888889 C30.8434607,88.8888889 33.3333333,86.3990163 33.3333333,83.3333333 C33.3333333,80.2633104 35.8188659,77.7777778 38.8888889,77.7777778 C41.9589119,77.7777778 44.4444444,80.2633104 44.4444444,83.3333333 C44.4444444,92.5231481 36.9675926,100 27.7777778,100 Z", Attributes.fill "#9CDD05" ] []
        , Svg.path [ Attributes.d "M72.2222222,100 C63.0324074,100 55.5555556,92.5231481 55.5555556,83.3333333 C55.5555556,80.2633104 58.0410881,77.7777778 61.1111111,77.7777778 C64.1811341,77.7777778 66.6666667,80.2633104 66.6666667,83.3333333 C66.6666667,86.3990163 69.1565393,88.8888889 72.2222222,88.8888889 C75.2879052,88.8888889 77.7777778,86.3990163 77.7777778,83.3333333 C77.7777778,80.2633104 80.2633104,77.7777778 83.3333333,77.7777778 C86.4033563,77.7777778 88.8888889,80.2633104 88.8888889,83.3333333 C88.8888889,92.5231481 81.412037,100 72.2222222,100 Z", Attributes.fill "#66BB00" ] []
        , Svg.path [ Attributes.d "M66.6666667,50 C66.6666667,59.2216437 59.2230904,66.6666667 50,66.6666667 C40.7769096,66.6666667 33.3333333,59.2216437 33.3333333,50 C33.3333333,46.8880207 35.7769096,44.4444444 38.8888889,44.4444444 C42.0008681,44.4444444 44.4444444,46.8880207 44.4444444,50 C44.4444444,53.1105326 46.8880207,55.5555556 50,55.5555556 C53.1119793,55.5555556 55.5555556,53.1105326 55.5555556,50 C55.5555556,46.8880207 57.9991319,44.4444444 61.1111111,44.4444444 C64.2230904,44.4444444 66.6666667,46.8880207 66.6666667,50 Z", Attributes.fill "#9CDD05" ] []
        , Svg.path [ Attributes.d "M66.6666667,50 C66.6666667,59.2216437 59.2230904,66.6666667 50,66.6666667 L50,55.5555556 C53.1119793,55.5555556 55.5555556,53.1105326 55.5555556,50 C55.5555556,46.8880207 57.9991319,44.4444444 61.1111111,44.4444444 C64.2230904,44.4444444 66.6666667,46.8880207 66.6666667,50 Z", Attributes.fill "#66BB00" ] []
        , Svg.path [ Attributes.d "M100,184.074074 C100,187.186053 97.5564237,189.62963 94.4444444,189.62963 L5.55555556,189.62963 C2.4435763,189.62963 0,187.186053 0,184.074074 C0,180.962095 2.4435763,178.518519 5.55555556,178.518519 L94.4444444,178.518519 C97.5564237,178.518519 100,180.962095 100,184.074074 Z", Attributes.fill "#C3EA21" ] []
        , Svg.path [ Attributes.d "M100,184.074074 C100,187.186053 97.5564237,189.62963 94.4444444,189.62963 L50,189.62963 L50,178.518519 L94.4444444,178.518519 C97.5564237,178.518519 100,180.962095 100,184.074074 Z", Attributes.fill "#9CDD05" ] []
        ]


{-| -}
tree : Nri.Ui.Svg.V1.Svg
tree =
    Nri.Ui.Svg.V1.init "0 0 100 100"
        [ Svg.path [ Attributes.d "M44.2554458,76.2957186 C36.1962642,76.2957186 29.6386609,69.7388768 29.6386609,61.6796952 L29.6386609,55.8332857 C29.6386609,54.2174899 30.9468311,52.9100811 32.5618657,52.9100811 C34.1776615,52.9100811 35.4858318,54.2174899 35.4858318,55.8332857 L35.4858318,61.6796952 C35.4858318,66.5156609 39.4194799,70.4493092 44.2554458,70.4493092 C45.8712416,70.4493092 47.1786506,71.7567179 47.1786506,73.3725139 C47.1786506,74.9883097 45.8712416,76.2957186 44.2554458,76.2957186 L44.2554458,76.2957186 Z", Attributes.fill "#D55F05" ] []
        , Svg.path [ Attributes.d "M61.7946739,70.4493092 L55.9482646,70.4493092 C54.3324686,70.4493092 53.0250599,69.1419004 53.0250599,67.5261044 C53.0250599,65.9103087 54.3324686,64.6028997 55.9482646,64.6028997 L61.7946739,64.6028997 C66.6298784,64.6028997 70.5642881,60.6692516 70.5642881,55.8332857 C70.5642881,54.2174899 71.8716969,52.9100811 73.4874928,52.9100811 C75.1032886,52.9100811 76.4106974,54.2174899 76.4106974,55.8332857 C76.4106974,63.8924673 69.8530942,70.4493092 61.7946739,70.4493092 L61.7946739,70.4493092 Z", Attributes.fill "#913F02" ] []
        , Svg.path [ Attributes.d "M58.8714693,55.8332857 L58.8714693,95.7256259 L41.3322411,95.7256259 L41.3322411,55.8332857 C41.3322411,54.1961693 42.6175679,52.9100811 44.2554458,52.9100811 L55.9482646,52.9100811 C57.585381,52.9100811 58.8714693,54.1961693 58.8714693,55.8332857 L58.8714693,55.8332857 Z", Attributes.fill "#D97F4A" ] []
        , Svg.path [ Attributes.d "M58.8714693,55.8332857 L58.8714693,95.7256259 L50.1018553,95.7256259 L50.1018553,52.9100811 L55.9482646,52.9100811 C57.585381,52.9100811 58.8714693,54.1961693 58.8714693,55.8332857 L58.8714693,55.8332857 Z", Attributes.fill "#D55F05" ] []
        , Svg.path [ Attributes.d "M99.9326344,40.1063075 L99.9326344,40.164939 C98.9975744,50.2206412 90.0916609,58.1716972 79.5090354,58.7564904 L44.2554458,58.7564904 L49.9259604,51.7412561 C49.984592,51.7412561 50.0432236,51.6826243 50.1018553,51.6239928 C55.246208,49.4020832 58.8714693,44.2569686 58.8714693,38.2940576 C58.8714693,32.2725149 55.246208,27.1274003 50.1018553,24.9054908 C48.2888439,24.0876939 46.3014607,23.6780341 44.2554458,23.6780341 L26.7154562,23.6780341 C25.0783398,23.6780341 23.7922517,22.391946 23.7922517,20.7548294 C23.7922517,9.47090917 32.9715254,0.097465538 44.2554458,0.097465538 L67.6410834,0.097465538 C78.5739756,0.097465538 87.5773547,8.94474751 88.1035161,19.7611378 C95.7621755,23.4442691 100.633929,31.5118267 99.9326344,40.1063075 L99.9326344,40.1063075 Z", Attributes.fill "#9CDD05" ] []
        , Svg.path [ Attributes.d "M99.9326344,40.1063075 L99.9326344,40.164939 C98.9975744,50.2206412 90.0916609,58.1716972 79.5090354,58.7564904 L50.1018553,58.7564904 L50.1018553,51.6239928 C55.246208,49.4020832 58.8714693,44.2569686 58.8714693,38.2940576 C58.8714693,32.2725149 55.246208,27.1274003 50.1018553,24.9054908 L50.1018553,0.097465538 L67.6410834,0.097465538 C78.5739756,0.097465538 87.5773547,8.94474751 88.1035161,19.7611378 C95.7621755,23.4442691 100.633929,31.5118267 99.9326344,40.1063075 L99.9326344,40.1063075 Z", Attributes.fill "#66BB00" ] []
        , Svg.path [ Attributes.d "M50.1018553,18.7080531 C48.2309737,18.1240213 46.3014607,17.8316247 44.2554458,17.8316247 L20.8690469,17.8316247 C15.1985323,17.8316247 9.93615468,20.0535344 6.07789013,24.1455642 C2.16099382,28.2383552 -0.0220819872,33.6751048 0.211682965,39.4050125 C0.796476291,50.045508 10.5795797,58.7564904 21.8048683,58.7564904 L44.2554458,58.7564904 C46.3014607,58.7564904 48.2309737,58.4640938 50.1018553,57.8793006 C58.5204412,55.3657558 64.7178786,47.5312018 64.7178786,38.2940576 C64.7178786,29.0569135 58.5204412,21.2223595 50.1018553,18.7080531 Z", Attributes.fill "#C3EA21" ] []
        , Svg.path [ Attributes.d "M64.7178786,38.2940576 C64.7178786,47.5312018 58.5204412,55.3657558 50.1018553,57.8793006 L50.1018553,18.7080531 C58.5204412,21.2223595 64.7178786,29.0569135 64.7178786,38.2940576 L64.7178786,38.2940576 Z", Attributes.fill "#9CDD05" ] []
        , Svg.path [ Attributes.d "M88.1035161,38.2940576 C88.1035161,39.9083306 86.7945844,41.2172622 85.1803114,41.2172622 C83.5660384,41.2172622 82.2571069,39.9083306 82.2571069,38.2940576 C82.2571069,36.6797847 83.5660384,35.3708529 85.1803114,35.3708529 C86.7945844,35.3708529 88.1035161,36.6797847 88.1035161,38.2940576 L88.1035161,38.2940576 Z", Attributes.fill "#CD0000" ] []
        , Svg.path [ Attributes.d "M76.4106974,20.7548294 C76.4106974,22.3691024 75.1017658,23.6780341 73.4874928,23.6780341 C71.8732199,23.6780341 70.5642881,22.3691024 70.5642881,20.7548294 C70.5642881,19.1405564 71.8732199,17.8316247 73.4874928,17.8316247 C75.1017658,17.8316247 76.4106974,19.1405564 76.4106974,20.7548294 L76.4106974,20.7548294 Z", Attributes.fill "#CD0000" ] []
        , Svg.path [ Attributes.d "M53.0250599,38.2940576 C53.0250599,39.931174 51.7389717,41.2172622 50.1018553,41.2172622 C48.4639774,41.2172622 47.1786506,39.931174 47.1786506,38.2940576 C47.1786506,36.6569411 48.4639774,35.3708529 50.1018553,35.3708529 C51.7389717,35.3708529 53.0250599,36.6569411 53.0250599,38.2940576 Z", Attributes.fill "#FF637B" ] []
        , Svg.path [ Attributes.d "M76.4106974,44.1404669 C76.4106974,45.7547399 75.1017658,47.0636717 73.4874928,47.0636717 C71.8732199,47.0636717 70.5642881,45.7547399 70.5642881,44.1404669 C70.5642881,42.5261939 71.8732199,41.2172622 73.4874928,41.2172622 C75.1017658,41.2172622 76.4106974,42.5261939 76.4106974,44.1404669 L76.4106974,44.1404669 Z", Attributes.fill "#CD0000" ] []
        , Svg.path [ Attributes.d "M41.3322411,44.1404669 C41.3322411,45.7547399 40.0233093,47.0636717 38.4090364,47.0636717 C36.794002,47.0636717 35.4858318,45.7547399 35.4858318,44.1404669 C35.4858318,42.5261939 36.794002,41.2172622 38.4090364,41.2172622 C40.0233093,41.2172622 41.3322411,42.5261939 41.3322411,44.1404669 Z", Attributes.fill "#FF637B" ] []
        , Svg.path [ Attributes.d "M23.7922517,44.1404669 C23.7922517,45.7547399 22.4840813,47.0636717 20.8690469,47.0636717 C19.2547739,47.0636717 17.9458422,45.7547399 17.9458422,44.1404669 C17.9458422,42.5261939 19.2547739,41.2172622 20.8690469,41.2172622 C22.4840813,41.2172622 23.7922517,42.5261939 23.7922517,44.1404669 Z", Attributes.fill "#FF637B" ] []
        , Svg.path [ Attributes.d "M64.7178786,14.9084201 C64.7178786,16.5226931 63.4089469,17.8316247 61.7946739,17.8316247 C60.1796395,17.8316247 58.8714693,16.5226931 58.8714693,14.9084201 C58.8714693,13.2941471 60.1796395,11.9852154 61.7946739,11.9852154 C63.4089469,11.9852154 64.7178786,13.2941471 64.7178786,14.9084201 L64.7178786,14.9084201 Z", Attributes.fill "#CD0000" ] []
        , Svg.path [ Attributes.d "M29.6386609,32.4476482 C29.6386609,34.0619211 28.3304908,35.3708529 26.7154562,35.3708529 C25.1011832,35.3708529 23.7922517,34.0619211 23.7922517,32.4476482 C23.7922517,30.8333752 25.1011832,29.5244436 26.7154562,29.5244436 C28.3304908,29.5244436 29.6386609,30.8333752 29.6386609,32.4476482 Z", Attributes.fill "#FF637B" ] []
        , Svg.path [ Attributes.d "M50.1018553,41.2172622 L50.1018553,35.3708529 C51.7389717,35.3708529 53.0250599,36.6569411 53.0250599,38.2940576 C53.0250599,39.931174 51.7389717,41.2172622 50.1018553,41.2172622 Z", Attributes.fill "#FF001E" ] []
        , Svg.path [ Attributes.d "M76.4106974,96.9530825 C76.4106974,98.5901989 75.1246092,99.8762872 73.4874928,99.8762872 L26.7154562,99.8762872 C25.0783398,99.8762872 23.7922517,98.5901989 23.7922517,96.9530825 C23.7922517,95.3159659 25.0783398,94.0298778 26.7154562,94.0298778 L73.4874928,94.0298778 C75.1246092,94.0298778 76.4106974,95.3159659 76.4106974,96.9530825 L76.4106974,96.9530825 Z", Attributes.fill "#C3EA21" ] []
        , Svg.path [ Attributes.d "M76.4106974,96.9530825 C76.4106974,98.5901989 75.1246092,99.8762872 73.4874928,99.8762872 L50.1018553,99.8762872 L50.1018553,94.0298778 L73.4874928,94.0298778 C75.1246092,94.0298778 76.4106974,95.3159659 76.4106974,96.9530825 L76.4106974,96.9530825 Z", Attributes.fill "#9CDD05" ] []
        ]


{-| -}
bold : Nri.Ui.Svg.V1.Svg
bold =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.path
            [ Attributes.d "M6 11H11.8299C13.7082 11 15.2308 9.433 15.2308 7.5C15.2308 5.567 13.7082 4 11.8299 4H8.30769C7.21984 4 6.67591 4 6.33795 4.34171C6 4.68342 6 5.23339 6 6.33333V11ZM6 11V15.6667C6 16.7666 6 17.3166 6.33795 17.6583C6.67591 18 7.21984 18 8.30769 18H12.6667C14.5076 18 16 16.433 16 14.5C16 12.567 14.5076 11 12.6667 11H11.7143"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
italic : Nri.Ui.Svg.V1.Svg
italic =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.path
            [ Attributes.d "M11 4H17.4166M7.33331 18.6667L14.6666 4M4.58331 18.6667H11"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
underline : Nri.Ui.Svg.V1.Svg
underline =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.path
            [ Attributes.d "M5.22222 3V10.5556C5.22222 13.7466 7.80902 16.3333 11 16.3333C14.191 16.3333 16.7778 13.7466 16.7778 10.5556V3M3 19H19"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
list : Nri.Ui.Svg.V1.Svg
list =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M7 4H19"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            ]
            []
        , Svg.path
            [ Attributes.d "M3 4H3.00898"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M3 11H3.00898"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M3 18H3.00898"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M7 11H19"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            ]
            []
        , Svg.path
            [ Attributes.d "M7 18H19"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            ]
            []
        ]


{-| -}
link : Nri.Ui.Svg.V1.Svg
link =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.path
            [ Attributes.d "M9 12.229C9.1416 12.4609 9.3097 12.6804 9.5042 12.8828C10.7117 14.1395 12.5522 14.336 13.9576 13.4722C14.218 13.3121 14.4634 13.1157 14.6872 12.8828L17.9266 9.5114C19.3578 8.02184 19.3578 5.60676 17.9266 4.11718C16.4953 2.6276 14.1748 2.62761 12.7435 4.11718L12.03 4.85978"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M9.9703 17.14L9.2565 17.8828C7.82526 19.3724 5.50471 19.3724 4.07345 17.8828C2.64218 16.3932 2.64218 13.9782 4.07345 12.4886L7.31287 9.1172C8.74413 7.62761 11.0647 7.6276 12.4959 9.1172C12.6904 9.3195 12.8584 9.539 13 9.7708"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
undo : Nri.Ui.Svg.V1.Svg
undo =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.path
            [ Attributes.d "M2 7H14C17.3137 7 20 9.6863 20 13C20 16.3137 17.3137 19 14 19H10"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M6 3L4.8462 3.87652C2.94873 5.31801 2 6.03875 2 7C2 7.96125 2.94873 8.68199 4.8462 10.1235L6 11"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
redo : Nri.Ui.Svg.V1.Svg
redo =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.path
            [ Attributes.d "M20 7H8C4.68629 7 2 9.6863 2 13C2 16.3137 4.68629 19 8 19H12"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M16 3L17.1538 3.87652C19.0513 5.31801 20 6.03875 20 7C20 7.96125 19.0513 8.68199 17.1538 10.1235L16 11"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
home : Nri.Ui.Svg.V1.Svg
home =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M8.06165 3.82633L2.23911 8.92134C1.7398 9.3583 2.07458 10.1343 2.76238 10.1343C3.18259 10.1343 3.52324 10.4489 3.52324 10.8371V14.0806C3.52324 16.871 3.52324 18.2662 4.46176 19.1331C5.40029 20 6.91082 20 9.9319 20H12.0681C15.0892 20 16.5997 20 17.5382 19.1331C18.4768 18.2662 18.4768 16.871 18.4768 14.0806V10.8371C18.4768 10.4489 18.8174 10.1343 19.2376 10.1343C19.9254 10.1343 20.2602 9.3583 19.7609 8.92134L13.9383 3.82633C12.5469 2.60878 11.8512 2 11 2C10.1488 2 9.4531 2.60878 8.06165 3.82633Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M11 15H11.009"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
homeInCircle : Nri.Ui.Svg.V1.Svg
homeInCircle =
    Nri.Ui.Svg.V1.init "0 0 40 41"
        [ Svg.g [ Attributes.fill "none" ]
            [ Svg.path
                [ Attributes.d "M40 21C40 32.0457 31.0457 41 20 41C8.9543 41 0 32.0457 0 21C0 9.9543 8.9543 1 20 1C31.0457 1 40 9.9543 40 21Z"
                , Attributes.fill "currentColor"
                ]
                []
            , Svg.path
                [ Attributes.d "M40 20C40 31.0457 31.0457 40 20 40C8.9543 40 0 31.0457 0 20C0 8.9543 8.9543 0 20 0C31.0457 0 40 8.9543 40 20Z"
                , Attributes.fill "white"
                ]
                []
            , Svg.path
                [ Attributes.d "M20 38.5C30.2173 38.5 38.5 30.2173 38.5 20C38.5 9.78273 30.2173 1.5 20 1.5C9.78273 1.5 1.5 9.78273 1.5 20C1.5 30.2173 9.78273 38.5 20 38.5ZM20 40C31.0457 40 40 31.0457 40 20C40 8.9543 31.0457 0 20 0C8.9543 0 0 8.9543 0 20C0 31.0457 8.9543 40 20 40Z"
                , Attributes.fill "currentColor"
                , Attributes.fillRule "evenodd"
                , Attributes.clipRule "evenodd"
                ]
                []
            , Svg.path
                [ Attributes.d "M29.6331 18.9887C29.8614 18.4595 29.7642 17.8027 29.2548 17.3569L23.3953 12.2296C23.2794 12.1282 23.1615 12.022 23.0412 11.9138C22.177 11.1359 21.1927 10.25 20 10.25C18.8073 10.25 17.823 11.1359 16.9588 11.9138C16.8385 12.022 16.7206 12.1282 16.6047 12.2296L10.7452 17.3569C10.2357 17.8027 10.1386 18.4595 10.3669 18.9887C10.5437 19.3985 10.9062 19.718 11.3678 19.8355C11.5751 19.8883 11.6787 19.9147 11.7208 19.9686C11.7629 20.0225 11.7632 20.1089 11.7638 20.2818C11.7668 21.1014 11.7732 22.9211 11.7732 23.1392C11.7732 24.4846 11.7732 25.5762 11.8984 26.4362C12.0295 27.3368 12.311 28.0912 12.9529 28.6841C13.5863 29.2691 14.378 29.5185 15.3234 29.6359C16.2423 29.75 17.4135 29.75 18.8803 29.75H21.1196C22.5864 29.75 23.7577 29.75 24.6765 29.6359C25.622 29.5185 26.4137 29.2691 27.0471 28.6841C27.689 28.0912 27.9705 27.3368 28.1016 26.4362C28.2268 25.5762 28.2268 24.4846 28.2268 23.1392C28.2268 22.9211 28.2332 21.1014 28.2362 20.2818C28.2368 20.1089 28.2371 20.0225 28.2792 19.9686C28.3212 19.9146 28.4249 19.8883 28.6322 19.8355C29.0937 19.718 29.4563 19.3985 29.6331 18.9887ZM20.0003 22.75C19.3099 22.75 18.7503 23.3096 18.7503 24C18.7503 24.6904 19.3099 25.25 20.0003 25.25H20.0093C20.6996 25.25 21.2593 24.6904 21.2593 24C21.2593 23.3096 20.6996 22.75 20.0093 22.75H20.0003Z"
                , Attributes.fill "currentColor"
                , Attributes.fillRule "evenodd"
                , Attributes.clipRule "evenodd"
                ]
                []
            ]
        ]


{-| -}
arrowPointingRightThick : Nri.Ui.Svg.V1.Svg
arrowPointingRightThick =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.path
            [ Attributes.d "M13.7695 19.5642C12.9054 20.2521 11.647 20.1099 10.959 19.2458C10.2711 18.3817 10.4143 17.1232 11.2783 16.4352L13.7695 19.5642ZM10.959 2.75458C11.6469 1.89038 12.9053 1.74733 13.7695 2.43525L13.7715 2.4372C13.7715 2.4372 13.7745 2.43867 13.7764 2.44013C13.7802 2.4432 13.7861 2.44732 13.793 2.45282C13.8067 2.46383 13.8263 2.48033 13.8516 2.50068C13.9022 2.54144 13.9754 2.60039 14.0674 2.67548C14.2515 2.82582 14.5125 3.04157 14.8252 3.30536C15.4489 3.83144 16.2873 4.55767 17.1309 5.34931C17.9652 6.13229 18.8474 7.01948 19.5361 7.862C19.879 8.28145 20.2124 8.73539 20.4688 9.19501C20.6981 9.60626 21 10.9997 21 10.9997C21 10.9997 20.6981 12.3932 20.4688 12.8044C20.2125 13.2639 19.8799 13.718 19.5371 14.1374C18.8484 14.98 17.9652 15.867 17.1309 16.6501C16.2874 17.4417 15.4488 18.1679 14.8252 18.694C14.5125 18.9579 14.2515 19.1736 14.0674 19.3239C13.9753 19.3991 13.9022 19.458 13.8516 19.4987C13.8265 19.5189 13.8067 19.5346 13.793 19.5456C13.7861 19.5511 13.7802 19.5562 13.7764 19.5593L13.7725 19.5622L13.7695 19.5642L11.2783 16.4352L11.2812 16.4333C11.2836 16.4314 11.2876 16.4278 11.293 16.4235C11.3037 16.4149 11.3199 16.4011 11.3418 16.3835C11.386 16.3479 11.4523 16.2946 11.5371 16.2253C11.7071 16.0865 11.9518 15.8846 12.2461 15.6364C12.8367 15.1382 13.6181 14.4609 14.3936 13.7331C14.6535 13.4892 14.906 13.2419 15.1494 12.9997H3C1.89552 12.9997 1.00015 12.1041 1 10.9997C1 9.89513 1.89543 8.9997 3 8.9997H15.1494C14.906 8.75747 14.6535 8.51022 14.3936 8.2663C13.6181 7.53859 12.8367 6.86112 12.2461 6.36298C11.9518 6.11478 11.7071 5.91288 11.5371 5.77411C11.4523 5.70484 11.386 5.65153 11.3418 5.61591C11.3199 5.59825 11.3037 5.58448 11.293 5.57587C11.2876 5.57159 11.2836 5.56801 11.2812 5.56611L11.2783 5.56415L11.125 5.42939C10.3996 4.72129 10.3143 3.56486 10.959 2.75458Z"
            , Attributes.fill "currentColor"
            ]
            []
        ]


{-| -}
library : Nri.Ui.Svg.V1.Svg
library =
    Nri.Ui.Svg.V1.init "0 0 24 24"
        [ Svg.g [ Attributes.fill "none" ]
            [ Svg.path
                [ Attributes.stroke "currentColor"
                , Attributes.strokeWidth "2"
                , Attributes.strokeLinecap "round"
                , Attributes.strokeLinejoin "round"
                , Attributes.d "M2 7C2 5.59987 2 4.8998 2.27248 4.36502C2.51217 3.89462 2.89462 3.51217 3.36502 3.27248C3.8998 3 4.59987 3 6 3C7.40013 3 8.1002 3 8.63498 3.27248C9.10538 3.51217 9.48783 3.89462 9.72752 4.36502C10 4.8998 10 5.59987 10 7V17C10 18.4001 10 19.1002 9.72752 19.635C9.48783 20.1054 9.10538 20.4878 8.63498 20.7275C8.1002 21 7.40013 21 6 21C4.59987 21 3.8998 21 3.36502 20.7275C2.89462 20.4878 2.51217 20.1054 2.27248 19.635C2 19.1002 2 18.4001 2 17V7Z"
                ]
                []
            , Svg.path
                [ Attributes.stroke "currentColor"
                , Attributes.strokeWidth "2"
                , Attributes.strokeLinecap "round"
                , Attributes.strokeLinejoin "round"
                , Attributes.d "M6 17H6.00898"
                ]
                []
            , Svg.path
                [ Attributes.stroke "currentColor"
                , Attributes.strokeWidth "2"
                , Attributes.strokeLinecap "round"
                , Attributes.strokeLinejoin "round"
                , Attributes.d "M2 7H10"
                ]
                []
            , Svg.path
                [ Attributes.stroke "currentColor"
                , Attributes.strokeWidth "2"
                , Attributes.strokeLinecap "round"
                , Attributes.strokeLinejoin "round"
                , Attributes.d "M11.4486 8.26843C11.0937 6.93838 10.9163 6.27336 11.0385 5.69599C11.146 5.18812 11.4108 4.72747 11.7951 4.38005C12.2319 3.98508 12.8942 3.80689 14.2187 3.4505C15.5432 3.09412 16.2055 2.91593 16.7804 3.03865C17.2862 3.1466 17.7449 3.41256 18.0909 3.79841C18.4842 4.23706 18.6617 4.90209 19.0166 6.23213L21.5514 15.7316C21.9063 17.0616 22.0837 17.7266 21.9615 18.304C21.854 18.8119 21.5892 19.2725 21.2049 19.62C20.7681 20.0149 20.1058 20.1931 18.7813 20.5495C17.4568 20.9059 16.7945 21.0841 16.2196 20.9614C15.7138 20.8534 15.2551 20.5874 14.9091 20.2016C14.5158 19.7629 14.3383 19.0979 13.9834 17.7679L11.4486 8.26843Z"
                ]
                []
            , Svg.path
                [ Attributes.stroke "currentColor"
                , Attributes.strokeWidth "2"
                , Attributes.strokeLinecap "round"
                , Attributes.strokeLinejoin "round"
                , Attributes.d "M17.7812 16.6953L17.7899 16.693"
                ]
                []
            , Svg.path
                [ Attributes.stroke "currentColor"
                , Attributes.strokeWidth "2"
                , Attributes.strokeLinecap "round"
                , Attributes.strokeLinejoin "round"
                , Attributes.d "M12 8.00019L18.5001 6"
                ]
                []
            ]
        ]


{-| -}
search : Nri.Ui.Svg.V1.Svg
search =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M16 16L20 20"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M18 10C18 5.58172 14.4183 2 10 2C5.58172 2 2 5.58172 2 10C2 14.4183 5.58172 18 10 18C14.4183 18 18 14.4183 18 10Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
searchInCicle : Nri.Ui.Svg.V1.Svg
searchInCicle =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.path
            [ Attributes.d "M11 0.250053C5.06294 0.250053 0.25 5.06299 0.25 11.0001C0.25 16.9371 5.06293 21.7501 11 21.7501C16.937 21.7501 21.75 16.9371 21.75 11.0001C21.75 5.06299 16.937 0.250053 11 0.250053ZM11 6.00005C8.23857 6.00005 6 8.23863 6 11.0001C6 13.7615 8.23857 16.0001 11 16.0001C12.0191 16.0001 12.9669 15.6952 13.7574 15.1717L15.2929 16.7072C15.6834 17.0977 16.3166 17.0977 16.7071 16.7072C17.0976 16.3166 17.0976 15.6835 16.7071 15.2929L15.1716 13.7574C15.6951 12.967 16 12.0191 16 11.0001C16 8.23863 13.7614 6.00005 11 6.00005ZM8 11.0001C8 9.3432 9.3431 8.00005 11 8.00005C12.6568 8.00005 14 9.3432 14 11.0001C14 12.6569 12.6568 14.0001 11 14.0001C9.3431 14.0001 8 12.6569 8 11.0001Z"
            , Attributes.fill "currentColor"
            , Attributes.fillRule "evenodd"
            , Attributes.clipRule "evenodd"
            ]
            []
        ]


{-| -}
speechBalloon : Nri.Ui.Svg.V1.Svg
speechBalloon =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.path
            [ Attributes.d "M2.75 1C1.23122 1 0 2.23122 0 3.75V15.7471C0 17.2659 1.23122 18.4971 2.75 18.4971H3.99902V19.7833C3.99902 21.2136 5.62163 22.0398 6.77832 21.1986L10.4929 18.4971H18.7483C20.2671 18.4971 21.4983 17.2659 21.4983 15.7471V3.75C21.4983 2.23122 20.2671 1 18.7483 1H2.75ZM10.2496 7.99707C10.6638 7.99674 10.9993 7.66069 10.999 7.24648C10.9987 6.83227 10.6626 6.49675 10.2484 6.49707L6.75332 6.49982C6.3391 6.50014 6.00358 6.83619 6.00391 7.25041C6.00423 7.66462 6.34028 8.00014 6.7545 7.99982L10.2496 7.99707ZM6.74902 11.4971C6.33481 11.4971 5.99902 11.8329 5.99902 12.2471C5.99902 12.6613 6.33481 12.9971 6.74902 12.9971H14.249C14.6632 12.9971 14.999 12.6613 14.999 12.2471C14.999 11.8329 14.6632 11.4971 14.249 11.4971H6.74902Z"
            , Attributes.fill "currentColor"
            , Attributes.fillRule "evenodd"
            , Attributes.clipRule "evenodd"
            ]
            []
        ]


{-| -}
speechBalloonOutline : Nri.Ui.Svg.V1.Svg
speechBalloonOutline =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M6.99902 11.4971H14.499M7.00391 6.49982L10.499 6.49707"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M18.9983 1H3C1.89543 1 1 1.89543 1 3V14.9971C1 16.1016 1.89543 16.9971 3 16.9971H4.99902V19.0333C4.99902 19.8506 5.92623 20.3227 6.58719 19.842L10.499 16.9971H18.9983C20.1029 16.9971 20.9983 16.1016 20.9983 14.9971V3C20.9983 1.89543 20.1029 1 18.9983 1Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
checklist : Nri.Ui.Svg.V1.Svg
checklist =
    Nri.Ui.Svg.V1.init "0 0 27 27"
        [ Svg.path [ Attributes.d "M11.0772,5.46017143 L25.1094857,5.46017143 C25.8126171,5.46017143 26.3851457,4.88761714 26.3851457,4.18451143 C26.3851457,3.48138 25.8125914,2.90885143 25.1094857,2.90885143 L11.0772,2.90885143 C10.3740686,2.90885143 9.80154,3.48140571 9.80154,4.18451143 C9.80154,4.88764286 10.3740943,5.46017143 11.0772,5.46017143 Z" ] []
        , Svg.path [ Attributes.d "M25.1094857,11.8386 L11.0772,11.8386 C10.3740686,11.8386 9.80154,12.4111543 9.80154,13.11426 C9.80154,13.8173657 10.3740943,14.38992 11.0772,14.38992 L25.1094857,14.38992 C25.8126171,14.38992 26.3851457,13.8173657 26.3851457,13.11426 C26.3851457,12.4111543 25.8125914,11.8386 25.1094857,11.8386 Z" ] []
        , Svg.path [ Attributes.d "M25.1094857,20.7684 L11.0772,20.7684 C10.3740686,20.7684 9.80154,21.3409543 9.80154,22.04406 C9.80154,22.7471914 10.3740943,23.31972 11.0772,23.31972 L25.1094857,23.31972 C25.8126171,23.31972 26.3851457,22.7471657 26.3851457,22.04406 C26.3851457,21.3409286 25.8125914,20.7684 25.1094857,20.7684 Z" ] []
        , Svg.path [ Attributes.d "M0,25.739701 C0,26.1738359 0.350748837,26.5245847 0.784883721,26.5245847 L7.06395349,26.5245847 C7.49808837,26.5245847 7.84883721,26.1738359 7.84883721,25.739701 L7.84883721,19.4606312 C7.84883721,19.0264963 7.49808837,18.6757475 7.06395349,18.6757475 L0.784883721,18.6757475 C0.350748837,18.6757475 0,19.0264963 0,19.4606312 L0,25.739701 Z M1.56976744,20.245515 L6.27906977,20.245515 L6.27906977,24.9548173 L1.56976744,24.9548173 L1.56976744,20.245515 Z" ] []
        , Svg.path [ Attributes.d "M6.34628571,9.6588 L3.42334286,12.5817429 L3.05169429,12.2100943 C2.55448286,11.7128829 1.74589714,11.7128829 1.24868571,12.2100943 C0.751474286,12.7073057 0.751474286,13.5158914 1.24868571,14.0131029 L2.52434571,15.2887629 C2.77546114,15.5398783 3.10191429,15.6604114 3.42835714,15.6604114 C3.7548,15.6604114 4.08126857,15.5348537 4.33236857,15.2887629 L8.15942571,11.4617057 C8.65663714,10.9644943 8.65663714,10.1559086 8.15942571,9.65869714 C7.65216,9.16148571 6.84858857,9.16148571 6.34636286,9.65869714 L6.34628571,9.6588 Z" ] []
        , Svg.path [ Attributes.d "M6.34628571,1.17308571 L3.42334286,4.09602857 L3.05169429,3.72438 C2.55448286,3.22716857 1.74589714,3.22716857 1.24868571,3.72438 C0.751474286,4.22159143 0.751474286,5.03017714 1.24868571,5.52738857 L2.52434571,6.80304857 C2.77546114,7.054164 3.10191429,7.17469714 3.42835714,7.17469714 C3.7548,7.17469714 4.08126857,7.04913943 4.33236857,6.80304857 L8.15942571,2.97599143 C8.65663714,2.47878 8.65663714,1.67019429 8.15942571,1.17298286 C7.65216,0.675771429 6.84858857,0.675771429 6.34636286,1.17298286 L6.34628571,1.17308571 Z" ] []
        ]


{-| -}
review : Nri.Ui.Svg.V1.Svg
review =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.path
            [ Attributes.d "M3.91833 8.08337H7.91834"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M3.91833 12.0834H6.91834"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M3.91833 16.0834H8.91834"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M5.04025 3C3.6141 3.04278 2.76381 3.20152 2.17546 3.79042C1.37 4.59663 1.37 5.8942 1.37 8.48933V14.4532C1.37 17.0483 1.37 18.3459 2.17546 19.1521C2.98091 19.9583 4.27728 19.9583 6.87 19.9583H11.4533C14.046 19.9583 15.3424 19.9583 16.1479 19.1521C16.2919 19.0079 16.4102 18.848 16.5074 18.6678M16.7693 5.0004C16.6523 4.49131 16.4609 4.10376 16.1479 3.79043C15.5595 3.20152 14.7093 3.04278 13.2831 3"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M5.03809 3.43748C5.03809 2.55152 5.7563 1.83331 6.64225 1.83331H11.6839C12.5699 1.83331 13.2881 2.55152 13.2881 3.43748C13.2881 4.32344 12.5699 5.04165 11.6839 5.04165H6.64225C5.7563 5.04165 5.03809 4.32344 5.03809 3.43748Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M21 18.125L18.0477 15.1727M18.0477 15.1727C18.2053 15.0153 18.3516 14.8467 18.4857 14.6682C19.0778 13.8803 19.4285 12.9008 19.4285 11.8393C19.4285 9.23563 17.3179 7.125 14.7143 7.125C12.1106 7.125 10 9.23563 10 11.8393C10 14.4429 12.1106 16.5535 14.7143 16.5535C16.0161 16.5535 17.1947 16.0259 18.0477 15.1727Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M12.7881 12.1042L13.9339 13.25L16.4548 10.5"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
text : Nri.Ui.Svg.V1.Svg
text =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.path
            [ Attributes.d "M19.4999 13V9C19.4999 5.22876 19.4999 3.34315 18.3284 2.17157C17.1568 1 15.2712 1 11.4999 1H10.5C6.72883 1 4.84323 1 3.67166 2.17156C2.50008 3.34312 2.50007 5.22872 2.50004 8.99993L2.5 12.9999C2.49997 16.7712 2.49995 18.6568 3.67153 19.8284C4.8431 21 6.72873 21 10.5 21H11.4999C15.2712 21 17.1568 21 18.3284 19.8284C19.4999 18.6569 19.4999 16.7712 19.4999 13Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M7 6H15M7 11H15M7 16H11"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
texts : Nri.Ui.Svg.V1.Svg
texts =
    Nri.Ui.Svg.V1.init "0 0 26 28"
        [ Svg.g [ Attributes.fill "none" ]
            [ Svg.path
                [ Attributes.d "M24.4999 19V15C24.4999 11.2288 24.4999 9.34315 23.3284 8.17157C22.1568 7 20.2712 7 16.4999 7H15.5C11.7288 7 9.84323 7 8.67166 8.17156C7.50008 9.34312 7.50007 11.2287 7.50004 14.9999L7.5 18.9999C7.49997 22.7712 7.49995 24.6568 8.67153 25.8284C9.8431 27 11.7287 27 15.5 27H16.4999C20.2712 27 22.1568 27 23.3284 25.8284C24.4999 24.6569 24.4999 22.7712 24.4999 19Z"
                , Attributes.stroke "currentColor"
                , Attributes.strokeWidth "2"
                , Attributes.strokeLinecap "round"
                , Attributes.strokeLinejoin "round"
                ]
                []
            , Svg.path
                [ Attributes.d "M21.4999 16V12C21.4999 8.22876 21.4999 6.34315 20.3284 5.17157C19.1568 4 17.2712 4 13.4999 4H12.5C8.72883 4 6.84323 4 5.67166 5.17156C4.50008 6.34312 4.50007 8.22872 4.50004 11.9999L4.5 15.9999C4.49997 19.7712 4.49995 21.6568 5.67153 22.8284C6.8431 24 8.72873 24 12.5 24H13.4999C17.2712 24 19.1568 24 20.3284 22.8284C21.4999 21.6569 21.4999 19.7712 21.4999 16Z"
                , Attributes.fill "white"
                , Attributes.stroke "currentColor"
                , Attributes.strokeWidth "2"
                , Attributes.strokeLinecap "round"
                , Attributes.strokeLinejoin "round"
                ]
                []
            , Svg.path
                [ Attributes.d "M18.4999 13V9C18.4999 5.22876 18.4999 3.34315 17.3284 2.17157C16.1568 1 14.2712 1 10.4999 1H9.5C5.72883 1 3.84323 1 2.67166 2.17156C1.50008 3.34312 1.50007 5.22872 1.50004 8.99993L1.5 12.9999C1.49997 16.7712 1.49995 18.6568 2.67153 19.8284C3.8431 21 5.72873 21 9.5 21H10.4999C14.2712 21 16.1568 21 17.3284 19.8284C18.4999 18.6569 18.4999 16.7712 18.4999 13Z"
                , Attributes.fill "white"
                , Attributes.stroke "currentColor"
                , Attributes.strokeWidth "2"
                , Attributes.strokeLinecap "round"
                , Attributes.strokeLinejoin "round"
                ]
                []
            , Svg.path
                [ Attributes.d "M6 6H14M6 11H14M6 16H10"
                , Attributes.stroke "currentColor"
                , Attributes.strokeWidth "2"
                , Attributes.strokeLinecap "round"
                , Attributes.strokeLinejoin "round"
                ]
                []
            ]
        ]


{-| -}
null : Nri.Ui.Svg.V1.Svg
null =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.path
            [ Attributes.d "M21 11C21 5.47715 16.5228 1 11 1C5.47715 1 1 5.47715 1 11C1 16.5228 5.47715 21 11 21C16.5228 21 21 16.5228 21 11Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.line
            [ Attributes.x1 "21"
            , Attributes.y1 "1.06066"
            , Attributes.x2 "0.847457"
            , Attributes.y2 "21.2132"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            ]
            []
        ]


{-| -}
mail : Nri.Ui.Svg.V1.Svg
mail =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M3 3H19C20.1046 3 21 3.89543 21 5V17C21 18.1046 20.1046 19 19 19H3C1.89543 19 1 18.1046 1 17V5C1 3.89543 1.89543 3 3 3Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M21 6L11.8944 10.5528C11.3314 10.8343 10.6686 10.8343 10.1056 10.5528L1 6"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
highlighter : Nri.Ui.Svg.V1.Svg
highlighter =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M12.7353 2.5L7.79412 7.44118L15.5588 15.2059L20.5 10.2647M5.67647 15.2059L7.79412 17.3235L5.5 19.5H1.5L5.67647 15.2059ZM4.97059 14.5L8.5 18.0294C10.9706 15.5588 13.0882 16.2647 13.0882 16.2647L14.8529 14.5L8.5 8.14706L6.73529 9.9118C6.73529 9.9118 7.44118 12.0294 4.97059 14.5Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
eraser : Nri.Ui.Svg.V1.Svg
eraser =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M7.73792 6.78021L3.84693 10.7574C2.67722 11.953 2.09236 12.5508 2.01263 13.2802C1.99579 13.4343 1.99579 13.5899 2.01263 13.744C2.09236 14.4733 2.67722 15.0711 3.84693 16.2668L3.99601 16.4191C4.62049 17.0575 4.93274 17.3766 5.30638 17.5911C5.5236 17.7157 5.75482 17.8134 5.99505 17.882C6.40827 18 6.85149 18 7.73792 18C8.62436 18 9.0676 18 9.4808 17.882C9.721 17.8134 9.9522 17.7157 10.1695 17.5911C10.5431 17.3766 10.8554 17.0575 11.4798 16.4191L14.3239 13.5121M7.73792 6.78021L11.3199 3.12313C12.7065 1.70754 13.3997 0.99974 14.2627 1C15.1256 1.00026 15.8185 1.70846 17.2042 3.12487L17.9473 3.8845C19.3159 5.28342 20.0002 5.98288 20 6.85008C19.9997 7.71728 19.315 8.41633 17.9456 9.8144L14.3239 13.5121M7.73792 6.78021L14.3239 13.5121"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M9 21H20"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
scale : Nri.Ui.Svg.V1.Svg
scale =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M11 6C12.1046 6 13 5.10457 13 4C13 2.89543 12.1046 2 11 2C9.89543 2 9 2.89543 9 4C9 5.10457 9.89543 6 11 6Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M9 4H3M13 4H19"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M16 20H6"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M11 6V20"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M21 13C21 14.6569 19.6569 16 18 16C16.3431 16 15 14.6569 15 13M21 13L18.5 7H17.5L15 13M21 13H15"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M7 13C7 14.6569 5.65685 16 4 16C2.34315 16 1 14.6569 1 13M7 13L4.5 7H3.5L1 13M7 13H1"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
microscope : Nri.Ui.Svg.V1.Svg
microscope =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M3 20.5H19"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M11.5 8.5C11.5 9.3284 10.8284 10 10 10C9.1716 10 8.5 9.3284 8.5 8.5C8.5 7.67157 9.1716 7 10 7C10.8284 7 11.5 7.67157 11.5 8.5Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M17 17.5H11.5M8.5 8.5C6.01472 8.5 4 10.5147 4 13C4 15.4853 6.01472 17.5 8.5 17.5H9"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M12.5 20.5L11.2279 16.6838C11.0918 16.2754 10.7097 16 10.2792 16H10.2208C9.7903 16 9.4082 16.2754 9.2721 16.6838L8 20.5"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M14.8023 14.5L17 13.2282M9.3859 7.13107L8.34041 5.31596C7.63965 4.09933 8.05551 2.54364 9.2693 1.84121C10.483 1.13879 12.035 1.55564 12.7358 2.77227L15.7809 8.05924C16.2553 8.88279 16.4925 9.2946 16.4925 9.7035C16.4925 9.9714 16.4221 10.2346 16.2885 10.4666C16.0845 10.8208 15.6737 11.0585 14.8521 11.534C14.0305 12.0095 13.6197 12.2472 13.2117 12.2472C12.9445 12.2472 12.6819 12.1767 12.4504 12.0427C12.0971 11.8383 11.8599 11.4265 11.3856 10.6029L10.8758 9.7179"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
tada : Nri.Ui.Svg.V1.Svg
tada =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M6.81205 5.6438L1.08091 19.5623C0.729211 20.4164 1.58335 21.2705 2.43748 20.9188L16.356 15.1877C17.0416 14.9054 17.2194 14.0163 16.6951 13.492L8.50776 5.30466C7.98342 4.78032 7.09439 4.95812 6.81205 5.6438Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M5.5 9.5L12.5 16.5M3.5 14.5L7.5 18.5"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M15 7L18 4"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M13.1973 1C13.5963 1.66667 13.9156 3.4 12 5"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M21 8.80274C20.3333 8.40365 18.6 8.08438 17 10"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M16.9998 1V1.02"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M20.9998 5V5.02"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M19.9998 12V12.02"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M9.99976 2V2.02"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
count : Nri.Ui.Svg.V1.Svg
count =
    Nri.Ui.Svg.V1.init "0 0 22 23"
        [ Svg.path
            [ Attributes.d "M1.5 11.5C1.5 7.02166 1.5 4.78249 2.89124 3.39124C4.28249 2 6.52166 2 11 2C15.4783 2 17.7175 2 19.1088 3.39124C20.5 4.78249 20.5 7.02166 20.5 11.5C20.5 15.9783 20.5 18.2175 19.1088 19.6088C17.7175 21 15.4783 21 11 21C6.52166 21 4.28249 21 2.89124 19.6088C1.5 18.2175 1.5 15.9783 1.5 11.5Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M9.08331 5.875L7.08331 17.125"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M14.9167 5.875L12.9167 17.125"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M17.1833 8.375H5.75"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M16.1833 14.625H4.75"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
openInNewTab : Nri.Ui.Svg.V1.Svg
openInNewTab =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M18 12V18C18 19.1046 17.1045 20 16 20H4C2.89543 20 2 19.1046 2 18V6C2 4.89543 2.89543 4 4 4H10"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M13 2H20V9M19.5 2.5L10 12"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
sync : Nri.Ui.Svg.V1.Svg
sync =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M19.5 4.5H8.5C4.78672 4.5 2 7.18503 2 11"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M2.5 17.5H13.5C17.2133 17.5 20 14.815 20 11"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M17.5 2C17.5 2 20 3.84122 20 4.50002C20 5.15882 17.5 7 17.5 7"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M4.49998 15C4.49998 15 2.00001 16.8412 2 17.5C1.99999 18.1588 4.5 20 4.5 20"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
delete : Nri.Ui.Svg.V1.Svg
delete =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M18.5 4.5L17.8803 14.5251C17.7219 17.0864 17.6428 18.3671 17.0008 19.2879C16.6833 19.7431 16.2747 20.1273 15.8007 20.416C14.8421 21 13.559 21 10.9927 21C8.42312 21 7.1383 21 6.17905 20.4149C5.7048 20.1257 5.296 19.7408 4.97868 19.2848C4.33688 18.3626 4.25945 17.0801 4.10461 14.5152L3.5 4.5"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M2 4.5H20M15.0557 4.5L14.3731 3.09173C13.9196 2.15626 13.6928 1.68852 13.3017 1.39681C13.215 1.3321 13.1231 1.27454 13.027 1.2247C12.5939 1 12.0741 1 11.0345 1C9.9688 1 9.436 1 8.99568 1.23412C8.8981 1.28601 8.80498 1.3459 8.71729 1.41317C8.32164 1.7167 8.10063 2.20155 7.65861 3.17126L7.05292 4.5"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M8.5 15.5V9.5"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            ]
            []
        , Svg.path
            [ Attributes.d "M13.5 15.5V9.5"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            ]
            []
        ]


{-| -}
addSticker : Nri.Ui.Svg.V1.Svg
addSticker =
    Nri.Ui.Svg.V1.init "0 0 22 23"
        [ Svg.path
            [ Attributes.d "M18 15V21M15 18H21M7 14C7.79811 15.2144 9.06876 16 10.5 16C11.9312 16 13.2019 15.2144 14 14M7.00785 8H7M14 8H13.9921M13.5 20.5166C12.557 20.8302 11.5483 21 10.5 21C5.25329 21 1 16.7467 1 11.5C1 6.25329 5.25329 2 10.5 2C15.7467 2 20 6.25329 20 11.5C20 11.8377 19.9824 12.1714 19.948 12.5"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
circle : Nri.Ui.Svg.V1.Svg
circle =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M11 21C16.5228 21 21 16.5228 21 11C21 5.47715 16.5228 1 11 1C5.47715 1 1 5.47715 1 11C1 16.5228 5.47715 21 11 21Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
flagUs : Nri.Ui.Svg.V1.Svg
flagUs =
    Nri.Ui.Svg.V1.init "0 0 92 64"
        [ Svg.g
            [ Attributes.stroke "none"
            , Attributes.strokeWidth "1"
            , Attributes.fill "none"
            , Attributes.fillRule "evenodd"
            ]
            [ Svg.rect
                [ Attributes.fill "#FFFFFF"
                , Attributes.x "7.15778928"
                , Attributes.y "6.97400533"
                , Attributes.width "32.1931667"
                , Attributes.height "20.18306"
                ]
                []
            , Svg.path
                [ Attributes.d "M85.9427893,0.0592053258 L6.79778928,0.0552993258 C3.11808928,0.0552993258 0.133689276,3.03969933 0.133689276,6.71549933 L0.133689276,57.1494993 C0.133689276,60.8291993 3.11808928,63.8096993 6.79388928,63.8096993 L85.9388893,63.8096993 C88.8997893,63.8096993 91.3138893,61.3955993 91.3138893,58.4346993 L91.3177953,5.43869933 C91.3177953,2.47389933 88.9036953,0.0597993258 85.9427953,0.0597993258 L85.9427893,0.0592053258 Z M30.9697893,10.4852053 L33.4931893,10.2664553 L34.4814693,7.96175533 C34.5908493,7.74300533 34.7002193,7.63363533 34.9189693,7.63363533 C35.1377193,7.63363533 35.3564693,7.74301533 35.3564693,7.96175533 L36.3447493,10.2664553 L38.8681493,10.4852053 C39.0868993,10.4852053 39.1962693,10.5945853 39.3056493,10.8133253 C39.4150293,11.0320653 39.3056493,11.2508253 39.1962693,11.3602053 L37.3290693,13.0047053 L37.8759493,15.4188053 C37.8759493,15.6375553 37.8759493,15.8563053 37.6571993,15.9656853 C37.4384493,16.0750653 37.2196993,16.0750653 37.1103193,15.9656853 L34.9150193,14.6492853 L32.8291193,15.9656853 C32.7197493,15.9656853 32.6103693,16.0750653 32.6103693,16.0750653 C32.5009993,16.0750653 32.3916193,16.0750653 32.2822493,15.9656853 C32.0634993,15.8563053 32.0634993,15.6375653 32.0634993,15.4188053 L32.6103693,13.0047053 L30.7431693,11.3602053 C30.6337893,11.2508253 30.5244193,11.0320853 30.6337893,10.8133253 C30.6416013,10.7039453 30.7509793,10.4852053 30.9697293,10.4852053 L30.9697893,10.4852053 Z M30.9697893,21.5672053 L33.4931893,21.3484553 L34.4814693,19.0437553 C34.5908493,18.8250053 34.7002193,18.7156353 34.9189693,18.7156353 C35.1377193,18.7156353 35.3564693,18.8250153 35.3564693,19.0437553 L36.3447493,21.3484553 L38.8681493,21.5672053 C39.0868993,21.5672053 39.1962693,21.6765853 39.3056493,21.8953353 C39.4150293,22.1140853 39.3056493,22.3328353 39.1962693,22.4422053 L37.3290693,24.0867053 L37.8759493,26.5008053 C37.8759493,26.7195553 37.8759493,26.9383053 37.6571993,27.0476853 C37.4384493,27.1570653 37.2196993,27.1570653 37.1103193,27.0476853 L35.0244193,25.7312853 L32.9385193,27.0476853 C32.8291393,27.0476853 32.7197693,27.1570653 32.7197693,27.1570653 C32.6103893,27.1570653 32.5010193,27.1570653 32.3916393,27.0476853 C32.1728893,26.9383053 32.1728893,26.7195653 32.1728893,26.5008053 L32.7197693,24.0867053 L30.8525693,22.4422053 C30.7431893,22.3328353 30.6338193,22.1140853 30.7431893,21.8953353 C30.6416293,21.6765853 30.7510023,21.5672053 30.9697493,21.5672053 L30.9697893,21.5672053 Z M19.4467893,10.4852053 L21.9701893,10.2664553 L22.9584693,7.96175533 C23.0678493,7.74300533 23.1772193,7.63363533 23.3959693,7.63363533 C23.6147193,7.63363533 23.8334693,7.74301533 23.8334693,7.96175533 L24.8217493,10.2664553 L27.3451493,10.4852053 C27.5638993,10.4852053 27.6732693,10.5945853 27.7826493,10.8133253 C27.8920193,11.0320753 27.7826493,11.2508253 27.6732693,11.3602053 L25.8060693,13.0047053 L26.3529493,15.4188053 C26.3529493,15.6375553 26.3529493,15.8563053 26.1341993,15.9656853 C26.0248193,16.0750653 25.9154493,16.0750653 25.8060793,16.0750653 C25.6967093,16.0750653 25.5873293,16.0750653 25.5873293,15.9656853 L23.5014293,14.6492853 L21.4155293,15.9656853 C21.3061493,16.0750653 20.9780293,16.0750653 20.8686493,15.9656853 C20.6498993,15.8563053 20.6498993,15.6375653 20.6498993,15.4188053 L21.1967793,13.0047053 L19.3295793,11.3602053 C19.2202093,11.2508253 19.1108293,11.0320853 19.2202093,10.8133253 C19.1186493,10.7039453 19.2280213,10.4852053 19.4467693,10.4852053 L19.4467893,10.4852053 Z M19.4467893,21.5672053 L21.9701893,21.3484553 L22.9584693,19.0437553 C23.0678493,18.8250053 23.1772193,18.7156353 23.3959693,18.7156353 C23.6147193,18.7156353 23.8334693,18.8250153 23.8334693,19.0437553 L24.8217493,21.3484553 L27.3451493,21.5672053 C27.5638993,21.5672053 27.6732693,21.6765853 27.7826493,21.8953353 C27.8920193,22.1140853 27.7826493,22.3328353 27.6732693,22.4422053 L25.8060693,24.0867053 L26.3529493,26.5008053 C26.3529493,26.7195553 26.3529493,26.9383053 26.1341993,27.0476853 C26.0248193,27.1570653 25.8060793,27.1570653 25.5873193,27.0476853 L23.5014193,25.7312853 L21.4155193,27.0476853 C21.3061393,27.0476853 21.1967693,27.1570653 21.1967693,27.1570653 C21.0873893,27.1570653 20.9780193,27.1570653 20.8686493,27.0476853 C20.6498993,26.9383053 20.6498993,26.7195653 20.6498993,26.5008053 L21.1967793,24.0867053 L19.3295793,22.4422053 C19.2202093,22.3328353 19.1108293,22.1140853 19.2202093,21.8953353 C19.1186493,21.6765853 19.2280213,21.5672053 19.4467693,21.5672053 L19.4467893,21.5672053 Z M8.03678928,10.4852053 L10.5601893,10.2664553 L11.5484693,7.96175533 C11.6578493,7.74300533 11.7672193,7.63363533 11.9859693,7.63363533 C12.2047193,7.63363533 12.4234693,7.74301533 12.4234693,7.96175533 L13.4117493,10.2664553 L15.9351493,10.4852053 C16.1538993,10.4852053 16.2632693,10.5945853 16.3726493,10.8133253 C16.4820293,11.0320653 16.3726493,11.2508253 16.2632693,11.3602053 L14.3960693,13.0047053 L14.9429493,15.4188053 C14.9429493,15.6375553 14.9429493,15.8563053 14.7241993,15.9656853 C14.5054493,16.0750653 14.2866993,16.0750653 14.1773193,15.9656853 L11.9820193,14.6492853 L9.89611928,15.9656853 C9.78673928,15.9656853 9.78673928,16.0750653 9.67736928,16.0750653 C9.56799928,16.0750653 9.45861928,16.0750653 9.34924928,15.9656853 C9.23987928,15.8563053 9.13049928,15.6375653 9.13049928,15.4188053 L9.67737928,13.0047053 L7.81017928,11.3602053 C7.70079928,11.2508253 7.59142928,11.0320853 7.70079928,10.8133253 C7.59923928,10.7039453 7.81798928,10.4852053 8.03673928,10.4852053 L8.03678928,10.4852053 Z M8.03678928,21.5672053 L10.5601893,21.3484553 L11.5484693,19.0437553 C11.6578493,18.7156353 12.3179993,18.7156353 12.5367493,19.0437553 L13.5250293,21.3484553 L16.0484293,21.5672053 C16.2671793,21.5672053 16.3765593,21.6765853 16.4859293,21.8953353 C16.5953093,22.1140853 16.4859293,22.3328353 16.3765593,22.4422053 L14.5093593,24.0867053 L15.0562393,26.5008053 C15.0562393,26.7195553 15.0562393,26.9383053 14.8374893,27.0476853 C14.7281093,27.1570653 14.5093693,27.1570653 14.2906093,27.0476853 L12.2047093,25.7312853 L10.1188093,27.0476853 C10.0094293,27.0476853 9.90005928,27.1570653 9.90005928,27.1570653 C9.79067928,27.1570653 9.68130928,27.1570653 9.57193928,27.0476853 C9.35318928,26.9383053 9.35318928,26.7195653 9.35318928,26.5008053 L9.90006928,24.0867053 L8.03286928,22.4422053 C7.92348928,22.3328353 7.81411928,22.1140853 7.92348928,21.8953353 C7.59926928,21.6765853 7.81801928,21.5672053 8.03676928,21.5672053 L8.03678928,21.5672053 Z M84.2987893,56.7902053 L7.15778928,56.7902053 L7.15778928,49.5480053 L84.2947893,49.5480053 L84.2987893,56.7902053 Z M84.2987893,42.6342053 L7.15778928,42.6342053 L7.15778928,35.3920053 L84.2947893,35.3920053 L84.2987893,42.6342053 Z M84.2987893,28.4822053 L46.7717893,28.4822053 L46.7717893,21.2400053 L84.2987893,21.2400053 L84.2987893,28.4822053 Z M84.2987893,14.2162053 L46.7717893,14.2162053 L46.7717893,6.97400533 L84.2987893,6.97400533 L84.2987893,14.2162053 Z"
                , Attributes.fill "currentColor"
                , Attributes.fillRule "nonzero"
                ]
                []
            , Svg.rect
                [ Attributes.fill "#FFFFFF"
                , Attributes.x "46.7717893"
                , Attributes.y "6.97400533"
                , Attributes.width "37.527"
                , Attributes.height "7.2422"
                ]
                []
            , Svg.rect
                [ Attributes.fill "#FFFFFF"
                , Attributes.x "46.7717893"
                , Attributes.y "21.2400053"
                , Attributes.width "37.527"
                , Attributes.height "7.2422"
                ]
                []
            , Svg.rect
                [ Attributes.fill "#FFFFFF"
                , Attributes.x "7.15778928"
                , Attributes.y "35.3920053"
                , Attributes.width "77.141"
                , Attributes.height "7.2422"
                ]
                []
            , Svg.rect
                [ Attributes.fill "#FFFFFF"
                , Attributes.x "7.15778928"
                , Attributes.y "49.5480053"
                , Attributes.width "77.141"
                , Attributes.height "7.2422"
                ]
                []
            ]
        ]


{-| -}
school : Nri.Ui.Svg.V1.Svg
school =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M6 21V11.3981C6 10.3299 6 9.7958 6.24458 9.3478C6.48915 8.89983 6.93842 8.61101 7.83697 8.03338L9.9185 6.69526C10.4437 6.35763 10.7063 6.18881 11 6.18881C11.2937 6.18881 11.5563 6.35763 12.0815 6.69526L14.163 8.03338C15.0616 8.61101 15.5108 8.89983 15.7554 9.3478C16 9.7958 16 10.3299 16 11.3981V21"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M11 12H11.009"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M20 21V15.1623C20 12.8707 18.7408 12.6852 16 12"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M2 21V15.1623C2 12.8707 3.25916 12.6852 6 12"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M1 21H21"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M11 21V17"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            ]
            []
        , Svg.path
            [ Attributes.d "M11 6V3.98221M11 3.98221V1.97035C11 1.49615 11 1.25905 11.1464 1.11173C11.6061 0.649394 13.5 1.74303 14.2203 2.18653C14.8285 2.56105 15 3.30914 15 3.98221H11Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
college : Nri.Ui.Svg.V1.Svg
college =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M10.9958 6H11.0048"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M4 8.99512V17.9998M8 8.99512V17.9998"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "square"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M14 8.99512V17.9998M18 8.99512V17.9998"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "square"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M20 18H2L2.0003 20.9999H20V18Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "square"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M2 6L11 1L20 6V8.99502H2V6Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
company : Nri.Ui.Svg.V1.Svg
company =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.path
            [ Attributes.d "M14 1H8C4.69067 1 4 1.69067 4 5V21H18V5C18 1.69067 17.3093 1 14 1Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M2 21H20"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M14 21V18C14 16.3453 13.6547 16 12 16H10C8.34533 16 8 16.3453 8 18V21"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M12.5 5H9.5M12.5 8.5H9.5M12.5 12H9.5"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
homeSchool : Nri.Ui.Svg.V1.Svg
homeSchool =
    home


{-| -}
globe : Nri.Ui.Svg.V1.Svg
globe =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M11 21C16.5228 21 21 16.5228 21 11C21 5.47715 16.5228 1 11 1C5.47715 1 1 5.47715 1 11C1 16.5228 5.47715 21 11 21Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M7 11C7 17 11 21 11 21C11 21 15 17 15 11C15 5 11 1 11 1C11 1 7 5 7 11Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M20 14H2"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M20 8H2"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
graduateCap : Nri.Ui.Svg.V1.Svg
graduateCap =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M19.5 15.5L18.0647 18.8435C17.8369 19.374 18.2391 20 18.8286 20H20.1714C20.7609 20 21.1631 19.374 20.9353 18.8435L19.5 15.5ZM19.5 15.5V8.5"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M5 10L6 16C6 16 8.5 18 11 18C13.5 18 16 16 16 16L17 10"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M10.88 2.00881C7.99494 2.42745 3.62406 4.88446 1.46271 6.18816C0.845762 6.5603 0.845762 7.4397 1.46271 7.81184C3.62406 9.1155 7.99494 11.5725 10.88 11.9912C10.961 12.0029 11.039 12.0029 11.12 11.9912C14.0051 11.5725 18.3759 9.1155 20.5373 7.81184C21.1542 7.4397 21.1542 6.5603 20.5373 6.18816C18.3759 4.88446 14.0051 2.42745 11.12 2.00881C11.039 1.99706 10.961 1.99706 10.88 2.00881Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
apple : Nri.Ui.Svg.V1.Svg
apple =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.path
            [ Attributes.d "M7 3C5.23791 3 3.72385 3.95961 2.68382 5.25872C1.64271 6.55917 1 8.2847 1 10.0278C1 13.6831 1.53001 16.4024 3.66677 19.6224C3.67698 19.6378 3.68762 19.6529 3.69867 19.6677C4.55856 20.8196 5.63796 21.6392 6.9096 21.9071C8.18855 22.1766 9.4998 21.8538 10.7466 21.0369L11 20.8844L11.2534 21.0369C12.5002 21.8538 13.8115 22.1766 15.0904 21.9071C16.362 21.6392 17.4414 20.8196 18.3013 19.6677C18.3124 19.6529 18.323 19.6378 18.3332 19.6224C20.47 16.4024 21 13.6831 21 10.0278C21 8.2847 20.3573 6.55917 19.3162 5.25872C18.2761 3.95961 16.7621 3 15 3C14.0416 3 13.0935 3.37823 12.3207 3.80836C11.8414 4.07511 11.3921 4.38297 11 4.68882C10.6079 4.38297 10.1586 4.07511 9.6793 3.80836C8.90651 3.37823 7.95836 3 7 3ZM7.52854 7.84892C7.99738 7.55702 8.14081 6.94031 7.84891 6.47147C7.557 6.00264 6.9403 5.8592 6.47146 6.1511C4.94812 7.09955 4 9.0883 4 11C4 11.5523 4.44772 12 5 12C5.55228 12 6 11.5523 6 11C6 9.6447 6.69734 8.36643 7.52854 7.84892Z"
            , Attributes.fill "currentColor"
            , Attributes.fillRule "evenodd"
            , Attributes.clipRule "evenodd"
            ]
            []
        , Svg.path
            [ Attributes.d "M12.4799 2.97515C12.1308 3.62157 12 4.44546 12 5C12 5.55228 11.5523 6 11 6C10.4477 6 10 5.55228 10 5C10 4.22121 10.1692 3.0451 10.7201 2.02485C11.2944 0.96128 12.3376 0 14 0C14.5523 0 15 0.44772 15 1C15 1.55228 14.5523 2 14 2C13.2624 2 12.8056 2.37206 12.4799 2.97515Z"
            , Attributes.fill "currentColor"
            , Attributes.fillRule "evenodd"
            , Attributes.clipRule "evenodd"
            ]
            []
        ]


{-| -}
appleOutline : Nri.Ui.Svg.V1.Svg
appleOutline =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M7 4C4.23858 4 2 7.0139 2 10.0278C2 13.544 2.5 16.0556 4.5 19.0695C6.02044 21.1062 8.05026 21.6168 10.2139 20.1903L11 19.7171L11.7861 20.1903C13.9497 21.6168 15.9796 21.1062 17.5 19.0695C19.5 16.0556 20 13.544 20 10.0278C20 7.0139 17.7614 4 15 4C13.5746 4 11.9108 5.15236 11 6C10.0892 5.15236 8.42542 4 7 4Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M5 11C5 9.3665 5.82273 7.73298 7 7"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M11 5C11 3.66667 11.6 1 14 1"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
briefcase : Nri.Ui.Svg.V1.Svg
briefcase =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M11 10H11.009"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M2.50391 11V18.5C2.50391 19.6046 3.39934 20.5 4.50391 20.5H17.501C18.6055 20.5 19.501 19.6046 19.501 18.5V11"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "square"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M14.5 5L14.1226 2.35858C14.0522 1.86593 13.6303 1.5 13.1327 1.5H8.8673C8.36965 1.5 7.94773 1.86593 7.87735 2.35858L7.5 5"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M19.5 5H2.5C1.94772 5 1.5 5.44772 1.5 6V9.4648C1.5 9.7992 1.6671 10.1114 1.9453 10.2969L5.74808 12.8321C5.91234 12.9416 6.10535 13 6.30278 13H15.6972C15.8946 13 16.0877 12.9416 16.2519 12.8321L20.0547 10.2969C20.3329 10.1114 20.5 9.7992 20.5 9.4648V6C20.5 5.44772 20.0523 5 19.5 5Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
brain : Nri.Ui.Svg.V1.Svg
brain =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M6 3.5C4.34315 3.5 3 4.84315 3 6.5C3 7.06866 3.15822 7.60037 3.43304 8.0535C2.04727 8.31855 1 9.537 1 11C1 12.463 2.04727 13.6814 3.43304 13.9465M6 3.5C6 2.11929 7.11929 1 8.5 1C9.8807 1 11 2.11929 11 3.5V18.5C11 19.8807 9.8807 21 8.5 21C7.11929 21 6 19.8807 6 18.5C4.34315 18.5 3 17.1569 3 15.5C3 14.9313 3.15822 14.3996 3.43304 13.9465M6 3.5C6 4.31791 6.39278 5.04408 7 5.50018M3.43304 13.9465C3.78948 13.3588 4.34207 12.9032 5 12.6707"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M16 18.4999C17.6569 18.4999 19 17.1567 19 15.4999C19 14.9312 18.8418 14.3995 18.567 13.9464C19.9527 13.6813 21 12.4629 21 10.9999C21 9.5369 19.9527 8.31843 18.567 8.05338M16 18.4999C16 19.8806 14.8807 20.9999 13.5 20.9999C12.1193 20.9999 11 19.8806 11 18.4999V3.49988C11 2.11917 12.1193 0.999878 13.5 0.999878C14.8807 0.999878 16 2.11917 16 3.49988C17.6569 3.49988 19 4.84302 19 6.49988C19 7.06854 18.8418 7.60024 18.567 8.05338M16 18.4999C16 17.682 15.6072 16.9558 15 16.4997M18.567 8.05338C18.2105 8.64109 17.6579 9.0966 17 9.3292"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
projectorScreen : Nri.Ui.Svg.V1.Svg
projectorScreen =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.line
            [ Attributes.x1 "0.75"
            , Attributes.y1 "1.25"
            , Attributes.x2 "21.25"
            , Attributes.y2 "1.25"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            ]
            []
        , Svg.path
            [ Attributes.d "M2.75 1.75H19.25V12C19.25 12.6904 18.6904 13.25 18 13.25H4C3.30964 13.25 2.75 12.6904 2.75 12V1.75Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M11 13L11 18"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.fill "none"
            ]
            []
        , Svg.line
            [ Attributes.x1 "5.75"
            , Attributes.y1 "4.54999"
            , Attributes.x2 "16.25"
            , Attributes.y2 "4.54999"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            ]
            []
        , Svg.line
            [ Attributes.x1 "5.75"
            , Attributes.y1 "7.54999"
            , Attributes.x2 "16.25"
            , Attributes.y2 "7.54999"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            ]
            []
        , Svg.line
            [ Attributes.x1 "5.75"
            , Attributes.y1 "10.55"
            , Attributes.x2 "12.25"
            , Attributes.y2 "10.55"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            ]
            []
        , Svg.circle
            [ Attributes.cx "11"
            , Attributes.cy "19"
            , Attributes.r "2"
            , Attributes.fill "currentColor"
            ]
            []
        ]


{-| -}
stretch : Nri.Ui.Svg.V1.Svg
stretch =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M15 4.5C15 5.32843 14.3284 6 13.5 6C12.6716 6 12 5.32843 12 4.5C12 3.67157 12.6716 3 13.5 3C14.3284 3 15 3.67157 15 4.5Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            ]
            []
        , Svg.path
            [ Attributes.d "M9.7259 7.21359C7.22588 9.7136 6 16.6324 6 20.0003M9.7259 7.21359C7.87718 5.96577 7.45184 3.69114 8.75097 2M9.7259 7.21359L12.3725 9M12.3725 9L14.9835 10.7994C15.507 11.1603 15.5653 11.9117 15.1036 12.3489L13.3602 14M12.3725 9C11.5697 10.0391 11.0164 11.0207 10.6026 11.8942C10.1636 12.8209 9.9441 13.2843 9.9845 13.8132M14.0002 20.0003C13.0268 18.8647 12.0257 17.3 11.0502 15.8578C10.3666 14.8474 10.0249 14.3422 9.9845 13.8132M9.9845 13.8132L7 13"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
speaker : Nri.Ui.Svg.V1.Svg
speaker =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M13 13.8135V8.18646C13 5.04126 13 3.46866 12.0747 3.0773C11.1494 2.68593 10.0603 3.79793 7.88232 6.02192C6.75439 7.17365 6.11085 7.42869 4.50604 7.42869C3.10257 7.42869 2.40084 7.42869 1.89675 7.77262C0.850353 8.48655 1.00852 9.882 1.00852 11C1.00852 12.118 0.850353 13.5134 1.89675 14.2274C2.40084 14.5713 3.10257 14.5713 4.50604 14.5713C6.11085 14.5713 6.75439 14.8264 7.88232 15.9781C10.0603 18.2021 11.1494 19.3141 12.0747 18.9227C13 18.5313 13 16.9587 13 13.8135Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M16 8C16.6254 8.81968 17 9.8634 17 11C17 12.1366 16.6254 13.1803 16 14"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M19 6C20.2508 7.36613 21 9.1057 21 11C21 12.8943 20.2508 14.6339 19 16"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
mutedSpeaker : Nri.Ui.Svg.V1.Svg
mutedSpeaker =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M13 13.8135V8.18646C13 5.04126 13 3.46866 12.0747 3.0773C11.1494 2.68593 10.0603 3.79793 7.88232 6.02192C6.75439 7.17365 6.11085 7.42869 4.50604 7.42869C3.10257 7.42869 2.40084 7.42869 1.89675 7.77262C0.850353 8.48655 1.00852 9.882 1.00852 11C1.00852 12.118 0.850353 13.5134 1.89675 14.2274C2.40084 14.5713 3.10257 14.5713 4.50604 14.5713C6.11085 14.5713 6.75439 14.8264 7.88232 15.9781C10.0603 18.2021 11.1494 19.3141 12.0747 18.9227C13 18.5313 13 16.9587 13 13.8135Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M17 9L21 13M17 13L21 9"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            ]
            []
        ]


{-| -}
gradingAssistant : Nri.Ui.Svg.V1.Svg
gradingAssistant =
    Nri.Ui.Svg.V1.init "0 0 20 21"
        [ Svg.path
            [ Attributes.d "M18.5705 2.29252C18.455 2.17838 18.2833 2.13859 18.1267 2.18781L14.0992 3.50514L10.6967 1.03596V1.03701C10.5649 0.941714 10.3889 0.927054 10.2421 0.99931C10.0963 1.07156 10.0045 1.21817 10.0056 1.37734L10.0531 5.4968L6.57731 7.87384C6.44342 7.96599 6.37539 8.12411 6.40131 8.28117C6.42722 8.4372 6.54276 8.56705 6.69825 8.61417L10.7529 9.84352L12.0087 13.7818V13.7828C12.0584 13.9357 12.1912 14.0477 12.3542 14.0729C12.5162 14.0969 12.6792 14.031 12.7743 13.9001L15.2362 10.5387L19.4851 10.5995C19.6493 10.6016 19.8004 10.5126 19.8749 10.3701C19.9495 10.2277 19.9343 10.057 19.8361 9.92929L17.3018 6.62022L18.6742 2.72059C18.726 2.57084 18.6861 2.40458 18.5705 2.29252ZM10.0444 10.5368C9.99476 10.4887 9.93429 10.4531 9.86734 10.4332L8.5597 10.0352C8.40637 9.98813 8.23792 10.0279 8.12455 10.1379L0.475262 17.5559C-0.153168 18.1654 -0.160742 19.1644 0.466623 19.7728C1.09397 20.3811 2.12411 20.3728 2.75254 19.7633L10.4245 12.3233V12.3243C10.5379 12.2144 10.5789 12.0531 10.5325 11.9044L10.1524 10.7086H10.1513C10.1308 10.6437 10.0941 10.585 10.0444 10.5368Z"
            , Attributes.fill "currentColor"
            ]
            []
        ]


{-| -}
manuallyGraded : Nri.Ui.Svg.V1.Svg
manuallyGraded =
    Nri.Ui.Svg.V1.init "0 0 20 22"
        [ Svg.g
            [ Attributes.clipPath "url(#clip0_2429_952)"
            ]
            [ Svg.path
                [ Attributes.d "M15.176 6.09566C15.176 8.92054 13.0543 11.2111 10.438 11.2111C7.82069 11.2111 5.69897 8.92054 5.69897 6.09566C5.69897 3.27079 7.82069 0.981155 10.438 0.981155C13.0543 0.981155 15.176 3.27079 15.176 6.09566Z"
                , Attributes.fill "currentColor"
                ]
                []
            , Svg.path
                [ Attributes.d "M19.4058 18.7922C19.4058 18.7467 19.3833 18.7012 19.3833 18.6548C19.3167 18.3355 19.1619 18.0608 18.9629 17.8324C18.719 17.5809 18.4319 17.3757 18.0772 17.2847C17.944 17.2393 17.7892 17.2161 17.6344 17.2161C16.638 17.2161 15.8188 18.0608 15.8188 19.0883C15.8188 20.1159 16.638 20.9606 17.6344 20.9606C18.6308 20.9606 19.4499 20.1159 19.4499 19.0883C19.4499 18.9973 19.4274 18.9064 19.4058 18.7922Z"
                , Attributes.fill "currentColor"
                ]
                []
            , Svg.path
                [ Attributes.d "M4.63579 17.2392C4.63579 16.2116 3.81668 15.3669 2.82026 15.3669H2.62132H2.57721C2.5331 15.3669 2.48899 15.3901 2.44401 15.3901C2.42152 15.3901 2.42152 15.3901 2.3999 15.4133C2.35578 15.4133 2.31167 15.4365 2.26669 15.4588C2.24421 15.4588 2.24421 15.4588 2.24421 15.482C2.20009 15.5052 2.15598 15.5275 2.11101 15.5275H2.08852C2.0444 15.5507 2.00029 15.573 1.93369 15.5962C1.88958 15.6194 1.84547 15.6649 1.77886 15.6872C1.69064 15.7558 1.57993 15.8468 1.51333 15.9387C1.20369 16.2812 1.02637 16.7147 1.02637 17.2178C1.02637 18.2454 1.84547 19.0901 2.84189 19.0901C3.83918 19.0892 4.63579 18.2668 4.63579 17.2392Z"
                , Attributes.fill "currentColor"
                ]
                []
            , Svg.path
                [ Attributes.d "M19.9367 17.6272C19.3832 13.0369 13.6035 11.5527 12.9177 11.3931L10.5046 13.3572L8.09149 11.3931C7.58203 11.5072 4.34982 12.3297 2.4451 14.5666L0.827648 8.69926L0.00854492 8.92761L1.66925 15.0475C2.00139 14.8647 2.40013 14.7505 2.82049 14.7505C4.14905 14.7505 5.2336 15.869 5.2336 17.239C5.2336 18.4949 4.3254 19.5225 3.17415 19.7054C4.17058 20.1389 5.5432 20.5733 7.35868 20.8239V14.8869C7.35868 14.8869 10.3479 14.8869 12.4074 16.2802L12.4299 21.0755H13.0276L13.0051 16.2802C15.0646 14.8869 18.0539 14.8869 18.0539 14.8869V16.6682C18.873 16.8279 19.5373 17.3987 19.8478 18.1756C19.9369 17.9928 19.9592 17.81 19.9367 17.6272Z"
                , Attributes.fill "currentColor"
                ]
                []
            ]
        , Svg.defs []
            [ Svg.clipPath
                [ Attributes.id "clip0_2429_952"
                ]
                [ Svg.rect
                    [ Attributes.width "20"
                    , Attributes.height "20.625"
                    , Attributes.fill "white"
                    , Attributes.transform "translate(0 0.6875)"
                    ]
                    []
                ]
            ]
        ]


{-| -}
retire : Nri.Ui.Svg.V1.Svg
retire =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M11 18C14.866 18 18 14.866 18 11C18 7.13401 14.866 4 11 4C7.13401 4 4 7.13401 4 11C4 14.866 7.13401 18 11 18Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M15 5L14.7276 3.91043C14.3931 2.5724 14.2258 1.90339 13.7499 1.49004C13.6973 1.44433 13.6423 1.40141 13.5852 1.36145C13.0688 1 12.3792 1 11 1C9.6208 1 8.93119 1 8.41476 1.36145C8.35765 1.40141 8.30268 1.44433 8.25006 1.49004C7.77415 1.90339 7.6069 2.5724 7.27239 3.91043L7 5"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M7 17L7.27239 18.0896C7.6069 19.4276 7.77415 20.0966 8.25006 20.51C8.30268 20.5557 8.35765 20.5986 8.41476 20.6386C8.93119 21 9.6208 21 11 21C12.3792 21 13.0688 21 13.5852 20.6386C13.6423 20.5986 13.6973 20.5557 13.7499 20.51C14.2258 20.0966 14.3931 19.4276 14.7276 18.0896L15 17"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M11 9V11.005L12 12"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            ]
            []
        ]


{-| -}
unpublish : Nri.Ui.Svg.V1.Svg
unpublish =
    Nri.Ui.Svg.V1.init "0 0 24 24"
        [ Svg.path
            [ Attributes.d "M17.4776 8.00005C17.4924 7.83536 17.5 7.66856 17.5 7.5C17.5 4.46243 15.0376 2 12 2C9.12324 2 6.76233 4.20862 6.52042 7.0227M17.4776 8.00005C17.395 8.91677 17.0874 9.76862 16.6105 10.5M17.4776 8.00005C19.9675 7.98791 22 10.0072 22 12.5C22 13.9034 21 15.5 20 16M6.52042 7.0227C3.98398 7.26407 2 9.40034 2 12C2 13.6358 2.78555 15.0805 4 16M6.52042 7.0227C6.67826 7.00768 6.83823 7 7 7C8.12582 7 9.16474 7.37209 10.0005 8"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M9 14L15 20M15 14L9 20"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
publish : Nri.Ui.Svg.V1.Svg
publish =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M10.5 11.5L14 8"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M1.74669 7.40628C0.68668 7.78486 0.77814 9.3132 1.87573 9.5627L10.5 11.5L12.4373 20.1243C12.6868 21.2219 14.2151 21.3133 14.5937 20.2533L20.9322 2.50557C21.2514 1.61167 20.3883 0.748557 19.4944 1.06781L1.74669 7.40628Z"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        ]


{-| -}
unretire : Nri.Ui.Svg.V1.Svg
unretire =
    Nri.Ui.Svg.V1.init "0 0 22 22"
        [ Svg.g [ Attributes.fill "none" ] []
        , Svg.path
            [ Attributes.d "M4.04798 7.60657L1.53784 7.45376C3.33712 2.70477 8.503 -8.32379e-05 13.5396 1.34474C18.904 2.77711 22.0904 8.26107 20.6565 13.5935C19.2227 18.926 13.7116 22.0876 8.3472 20.6553C4.36419 19.5917 1.58192 16.2946 1 12.4844"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            , Attributes.fill "none"
            ]
            []
        , Svg.path
            [ Attributes.d "M11 7V11L13 13"
            , Attributes.stroke "currentColor"
            , Attributes.strokeWidth "2"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            ]
            []
        ]


{-| -}
duplicate : Nri.Ui.Svg.V1.Svg
duplicate =
    copyToClipboard
