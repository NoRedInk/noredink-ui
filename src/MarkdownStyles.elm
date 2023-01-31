module MarkdownStyles exposing (..)

import Css exposing (..)
import Css.Global
import Nri.Ui.Colors.V1 as Colors


anchorAndButton : List Css.Style
anchorAndButton =
    [ Css.Global.descendants
        [ Css.Global.a
            [ borderBottom3 (px 1) solid Colors.azure
            , Css.Global.withAttribute "aria-disabled=true" [ borderBottom3 (px 1) solid Colors.gray45 ]
            ]
        , Css.Global.button
            [ borderBottom3 (px 1) solid Colors.azure
            , Css.disabled [ borderBottom3 (px 1) solid Colors.gray45 ]
            ]
        ]
    ]
