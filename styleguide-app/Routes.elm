module Routes exposing (Route(..), fromLocation)

import Browser.Navigation as Navigation
import ModuleExample exposing (categoryFromString)
import Url exposing (Url)
import Url.Parser as Url exposing ((</>), custom, s, string, top)


type Route
    = Doodad String
    | Category ModuleExample.Category
    | All


route : Url.Parser (Route -> a) a
route =
    Url.oneOf
        [ Url.map Category (s "category" </> category)
        , Url.map Doodad (s "doodad" </> s "Nri" </> string)
        , Url.map All top
        ]


category : Url.Parser (ModuleExample.Category -> a) a
category =
    custom "category" (categoryFromString >> Result.toMaybe)


fromLocation : Url -> Route
fromLocation location =
    Url.parse route location
        |> Maybe.withDefault All
