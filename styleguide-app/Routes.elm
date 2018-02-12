module Routes exposing (Route(..), fromLocation)

import ModuleExample exposing (categoryFromString)
import Navigation
import UrlParser as Url exposing ((</>), custom, s, string, top)


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
    custom "category" categoryFromString


fromLocation : Navigation.Location -> Route
fromLocation location =
    Url.parseHash route location
        |> Maybe.withDefault All
