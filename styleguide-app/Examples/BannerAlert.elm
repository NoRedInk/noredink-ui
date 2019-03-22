module Examples.BannerAlert exposing (example)

{-|

@docs example

-}

import Assets exposing (Assets, assets)
import Html.Styled exposing (h3, text)
import ModuleExample as ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.BannerAlert.V3 as BannerAlert


example : ModuleExample msg
example =
    { filename = "Nri.Ui.BannerAlert.V3.elm"
    , category = Messaging
    , content =
        [ h3 [] [ text "error" ]
        , BannerAlert.error assets "This is an error message!"
        , h3 [] [ text "neutral" ]
        , BannerAlert.neutral assets "This is a neutral message!"
        , h3 [] [ text "success" ]
        , BannerAlert.success assets
            """This is a success message!
            Let's see what happens if there is a very long message!
            Wow, how successful! You're the biggest success I've ever seen!
            You should feel great about yourself! Give yourself a very big round of applause!
            """
        ]
    }
