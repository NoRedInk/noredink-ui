module MarkdownStyles exposing (..)

import Css exposing (..)
import Css.Global
import Nri.Ui.Colors.V1 as Colors


anchorAndButton : List Css.Style
anchorAndButton =
    [ Css.Global.descendants
        [ Css.Global.a
            [ borderBottom3 (px 1) solid Colors.azure
            , textDecoration none
            , visited [ color Colors.azure ]
            , Css.Global.withAttribute "aria-disabled=true" [ borderBottom3 (px 1) solid Colors.gray45 ]
            ]
        , Css.Global.p
            [ margin4 zero zero (px 5) zero
            , lastChild [ margin zero ]
            ]
        ]
    ]
