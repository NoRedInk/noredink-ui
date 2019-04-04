module Examples.Alert exposing (example)

{-|

@docs example

-}

import ModuleExample as ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.Alert.V3 as Alert


example : ModuleExample msg
example =
    { filename = "Nri.Ui.Alert.V3.elm"
    , category = Messaging
    , content =
        [ Alert.error "This is an error"
        , Alert.warning "This is a warning"
        , Alert.tip "This is a tip"
        , Alert.success "This is a success"
        ]
    }
