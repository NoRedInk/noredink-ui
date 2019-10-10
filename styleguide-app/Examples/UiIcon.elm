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
        [ (IconExamples.view "Actions" << List.map (Tuple.mapSecond Svg.toHtml))
            [ ( "unarchive", UiIcon.unarchive )
            , ( "share", UiIcon.share )
            , ( "seeMore", UiIcon.seeMore )
            , ( "preview", UiIcon.preview )
            , ( "performance", UiIcon.performance )
            , ( "openClose", UiIcon.openClose )
            , ( "download", UiIcon.download )
            ]
        , (IconExamples.view "Edit" << List.map (Tuple.mapSecond Svg.toHtml))
            [ ( "edit", UiIcon.edit )
            ]
        , (IconExamples.view "Settings" << List.map (Tuple.mapSecond Svg.toHtml))
            [ ( "gear", UiIcon.gear )
            ]
        ]
    }
