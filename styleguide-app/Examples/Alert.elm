module Examples.Alert exposing (example)

{-|

@docs example

-}

import Html.Styled
import ModuleExample as ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.Alert.V2 as Alert


example : ModuleExample msg
example =
    { filename = "Nri/Alerts.elm"
    , category = Messaging
    , content =
        [ Alert.error "This is an error"
        , Alert.warning "This is a warning"
        , Alert.tip "This is a tip"
        , Alert.success "This is a success"
        ]
            |> List.map Html.Styled.toUnstyled
    }
