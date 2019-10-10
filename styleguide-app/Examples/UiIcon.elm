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
            ]
        , (IconExamples.view "Actions" << List.map (Tuple.mapSecond Svg.toHtml))
            [ ( "unarchive", UiIcon.unarchive )
            , ( "share", UiIcon.share )
            , ( "preview", UiIcon.preview )
            , ( "edit", UiIcon.edit )
            ]
        , (IconExamples.view "Class" << List.map (Tuple.mapSecond Svg.toHtml))
            [ ( "class", UiIcon.class )
            , ( "leaderboard", UiIcon.leaderboard )
            , ( "performance", UiIcon.performance )
            ]
        , (IconExamples.view "Time" << List.map (Tuple.mapSecond Svg.toHtml))
            [ ( "calendar", UiIcon.calendar )
            ]
        , (IconExamples.view "Other" << List.map (Tuple.mapSecond Svg.toHtml))
            [ ( "document", UiIcon.document )
            ]
        ]
    }
