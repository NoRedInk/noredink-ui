module Examples.BreadCrumbs exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Accessibility.Styled exposing (..)
import Category exposing (Category(..))
import Css
import Example exposing (Example)
import Examples.IconExamples as IconExamples
import Nri.Ui.BreadCrumbs.V1 as BreadCrumbs


{-| -}
type alias State =
    {}


{-| -}
type alias Msg =
    ()


{-| -}
example : Example State Msg
example =
    { name = "BreadCrumbs"
    , version = 1
    , categories = [ Layout ]
    , keyboardSupport = []
    , state = {}
    , update = \_ m -> ( m, Cmd.none )
    , subscriptions = \_ -> Sub.none
    , preview = []
    , view =
        \ellieLinkConfig settings ->
            []
    }
