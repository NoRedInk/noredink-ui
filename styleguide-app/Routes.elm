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
            |= (restOfPath |> Parser.andThen category)
        , Parser.succeed Doodad
            |. Parser.token "doodad/"
            |= restOfPath
        , Parser.succeed All
        ]


restOfPath : Parser String
restOfPath =
    Parser.getChompedString (Parser.chompWhile (always True))


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
