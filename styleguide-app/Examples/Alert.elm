module Examples.Alert exposing (example)

{-|

@docs example

-}

import Assets exposing (assets)
import Html.Styled
import ModuleExample as ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.Alert.V2 as Alert


example : ModuleExample msg
example =
    { filename = "Nri/Alerts.elm"
    , category = Messaging
    , content =
        [ Alert.error assets "This is an error"
        , Alert.warning assets "This is a warning"
        , Alert.tip assets "This is a tip"
        , Alert.success assets "This is a success"
        ]
            |> List.map Html.Styled.toUnstyled
    }
