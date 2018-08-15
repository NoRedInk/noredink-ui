module Examples.Alert exposing (example)

{-|

@docs example

-}

import Html
import ModuleExample as ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.Alert.V2 as Alert


example : ModuleExample msg
example =
    { filename = "Nri/Alerts.elm"
    , category = Messaging
    , content =
        [ Alert.error { content = "This is an error" }
        , Alert.warning { content = "This is a warning" }
        , Alert.tip { content = "This is a tip" }
        , Alert.success { content = "This is a success" }
        ]
    }
