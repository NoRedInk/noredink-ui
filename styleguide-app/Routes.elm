module Routes exposing (Route(..), fromLocation)

import Browser.Navigation as Navigation
import Category
import Parser exposing ((|.), (|=), Parser)
import Url exposing (Url)


type Route
    = Doodad String
    | Category Category.Category
    | All


route : Parser Route
route =
    Parser.oneOf
        [ Parser.succeed Category
            |. Parser.token "/category/"
            |= (restOfPath |> Parser.andThen category)
        , Parser.succeed Doodad
            |. Parser.token "/doodad/"
            |= restOfPath
        , Parser.succeed All
        ]


restOfPath : Parser String
restOfPath =
    Parser.getChompedString (Parser.chompWhile (always True))


category : String -> Parser Category.Category
category string =
    case Category.fromString string of
        Ok c ->
            Parser.succeed c

        Err e ->
            Parser.problem e


fromLocation : Url -> Route
fromLocation location =
    location.fragment
        |> Maybe.withDefault ""
        |> Parser.run route
        |> Result.withDefault All
