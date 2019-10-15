module Examples.UiIcon exposing (example)

{-|

@docs example, styles

-}

import Examples.IconExamples as IconExamples
import ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Icon.V5 as Icon
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.UiIcon.V1 as UiIcon


{-| -}
example : ModuleExample msg
example =
    { name = "Nri.Ui.UiIcon.V1"
    , category = Icons
    , content =
        [ (IconExamples.view "Interface" << List.map (Tuple.mapSecond Svg.toHtml))
            [ ( "seeMore", UiIcon.seeMore )
            , ( "openClose", UiIcon.openClose )
            , ( "download", UiIcon.download )
            , ( "sort", UiIcon.sort )
            , ( "gear", UiIcon.gear )
            , ( "sortArrow", UiIcon.sortArrow )
            ]
        , (IconExamples.view "Actions" << List.map (Tuple.mapSecond Svg.toHtml))
            [ ( "unarchive", UiIcon.unarchive )
            , ( "share", UiIcon.share )
            , ( "preview", UiIcon.preview )
            ]
        , (IconExamples.view "Humans & Class" << List.map (Tuple.mapSecond Svg.toHtml))
            [ ( "person", UiIcon.person )
            , ( "class", UiIcon.class )
            , ( "leaderboard", UiIcon.leaderboard )
            , ( "performance", UiIcon.performance )
            ]
        , (IconExamples.view "Time" << List.map (Tuple.mapSecond Svg.toHtml))
            [ ( "calendar", UiIcon.calendar )
            , ( "clock", UiIcon.clock )
            ]
        , (IconExamples.view "Writing Utensils" << List.map (Tuple.mapSecond Svg.toHtml))
            [ ( "edit", UiIcon.edit )
            , ( "pen", UiIcon.pen )
            ]
        , (IconExamples.view "Writing" << List.map (Tuple.mapSecond Svg.toHtml))
            [ ( "document", UiIcon.document )
            , ( "newspaper", UiIcon.newspaper )
            ]
        , (IconExamples.view "Sticky things" << List.map (Tuple.mapSecond Svg.toHtml))
            [ ( "arrowDown", UiIcon.arrowDown )
            , ( "checkmark", UiIcon.checkmark )
            , ( "x", UiIcon.x )
            , ( "attention", UiIcon.attention )
            , ( "exclamation", UiIcon.exclamation )
            ]
        , (IconExamples.view "Settings" << List.map (Tuple.mapSecond Svg.toHtml))
            [ ( "gear", UiIcon.gear )
            ]
        ]
    }
