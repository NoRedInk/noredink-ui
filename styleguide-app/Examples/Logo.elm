module Examples.Logo exposing (example)

{-|

@docs example, styles

-}

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
        [ IconExamples.view "NRI"
            [ ( "noredink", Logo.noredink )
            ]
        , IconExamples.view "Social Media & SSO"
            [ ( "facebook", Logo.facebook )
            , ( "twitter", Logo.twitter )
            , ( "clever", Logo.clever )
            ]
        ]
    }
