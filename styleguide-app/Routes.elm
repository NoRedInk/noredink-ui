module Routes exposing (Route(..), fromLocation)

import Browser.Navigation as Navigation
import ModuleExample exposing (categoryFromString)
import Parser exposing ((|.), (|=), Parser)
import Url exposing (Url)


type Route
    = Doodad String
    | Category ModuleExample.Category
    | All


route : Parser Route
route =
    Parser.oneOf
        [ Parser.succeed Category
            |. Parser.token "category/"
            |= (pathComponent |> Parser.andThen category)
        , Parser.succeed Doodad
            |. Parser.token "doodad/Nri/"
            |= pathComponent
        , Parser.succeed All
        ]


pathComponent : Parser String
pathComponent =
    Parser.getChompedString (Parser.chompUntilEndOr "/")


category : String -> Parser ModuleExample.Category
category string =
    case categoryFromString string of
        Ok c ->
            Parser.succeed c

        Err e ->
            Parser.problem e


fromLocation : Url -> Route
fromLocation location =
    Parser.run route (Maybe.withDefault "" location.fragment)
        |> Result.withDefault All
