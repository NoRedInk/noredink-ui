module Examples.Pennant exposing (example)

{-|

@docs example

-}

import Css exposing (..)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Pennant.V1 as Pennant


{-| -}
example : ModuleExample msg
example =
    { name = "Nri.Ui.Pennant.V1"
    , category = Icons
    , content =
        [ Html.div [ css [ displayFlex, alignItems center ] ]
            [ Html.span [ css [ Fonts.baseFont, Css.fontSize (Css.px 16) ] ]
                [ Html.text "A premiumFlag indicates that content is Premium"
                ]
            , Pennant.premiumFlag
            ]
        ]
    }
