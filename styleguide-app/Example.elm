module Example exposing (Example)

import Category exposing (Category)
import Html.Styled as Html exposing (Html)


type alias Example state msg =
    { name : String
    , state : state
    , update : msg -> state -> ( state, Cmd msg )
    , subscriptions : state -> Sub msg
    , view : state -> List (Html msg)
    , categories : List Category
    }
