module Examples.Logo exposing (example)

{-|

@docs example, styles

-}

import Css
import Examples.IconExamples as IconExamples
import ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Logo.V1 as Logo


{-| -}
example : ModuleExample msg
example =
    { name = "Nri.Ui.Logo.V1"
    , category = Icons
    , content =
        [ IconExamples.viewWithCustomStyles "NRI"
            [ ( "noredink"
              , Logo.noredink
              , [ Css.height (Css.px 25)
                , Css.width (Css.px 100)
                , Css.margin (Css.px 4)
                ]
              )
            ]
        , IconExamples.viewWithCustomStyles "Social Media & SSO"
            [ ( "facebook"
              , Logo.facebook
              , defaults
              )
            , ( "twitter", Logo.twitter, defaults )
            , ( "clever"
              , Logo.clever
              , [ Css.height (Css.px 25)
                , Css.width (Css.px 100)
                , Css.margin (Css.px 4)
                , Css.color Colors.azure
                ]
              )
            ]
        ]
    }


defaults : List Css.Style
defaults =
    [ Css.height (Css.px 25)
    , Css.width (Css.px 25)
    , Css.margin (Css.px 4)
    , Css.color Colors.azure
    ]
