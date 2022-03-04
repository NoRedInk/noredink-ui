module Debug.Control.Extra exposing
    ( float, int
    , list, listItem, optionalListItem, optionalListItemDefaultChecked, optionalBoolListItem
    , css
    )

{-|

@docs float, int
@docs list, listItem, optionalListItem, optionalListItemDefaultChecked, optionalBoolListItem
@docs css

-}

import Css
import Css.Global
import Debug.Control as Control exposing (Control)
import Regex


{-| -}
float : Float -> Control Float
float default =
    Control.map (String.toFloat >> Maybe.withDefault default)
        (Control.string (String.fromFloat default))


{-| -}
int : Int -> Control Int
int default =
    Control.map (String.toInt >> Maybe.withDefault default)
        (Control.string (String.fromInt default))


{-| Use with `listItem` and `optionalListItem`

    list
        |> listItem "first name" string
        |> listItem "last name" string

-}
list : Control (List a)
list =
    Control.record []


{-| -}
listItem : String -> Control a -> Control (List a) -> Control (List a)
listItem name accessor accumulator =
    Control.field name
        (Control.map List.singleton accessor)
        (Control.map (++) accumulator)


{-| -}
optionalListItem : String -> Control a -> Control (List a) -> Control (List a)
optionalListItem =
    optionalListItem_ False


{-| -}
optionalListItemDefaultChecked : String -> Control a -> Control (List a) -> Control (List a)
optionalListItemDefaultChecked =
    optionalListItem_ True


{-| -}
optionalListItem_ : Bool -> String -> Control a -> Control (List a) -> Control (List a)
optionalListItem_ default name accessor accumulator =
    Control.field name
        (Control.map (List.singleton >> List.filterMap identity) (Control.maybe default accessor))
        (Control.map (++) accumulator)


{-| -}
optionalBoolListItem : String -> (Bool -> a) -> Control (List a) -> Control (List a)
optionalBoolListItem name f accumulator =
    Control.field name
        (Control.map
            (\value ->
                if value then
                    [ f value ]

                else
                    []
            )
            (Control.bool False)
        )
        (Control.map (++) accumulator)


{-| -}
css : String -> Control ( String, List Css.Style )
css exampleCss =
    Control.map
        (\rawStr ->
            case Regex.find selectorAndStyles rawStr of
                [] ->
                    toCssProps rawStr

                matches ->
                    toCss matches
        )
        (Control.stringTextarea exampleCss)


selectorAndStyles : Regex.Regex
selectorAndStyles =
    Maybe.withDefault Regex.never
        (Regex.fromString "([\\s\\S]+)\\s*{\\s*([\\s\\S]*)\\s*}\\s*")


toCss : List Regex.Match -> ( String, List Css.Style )
toCss matches =
    List.concatMap
        (\match ->
            case Debug.log "Matches" <| List.filterMap identity match.submatches of
                selector :: styles :: [] ->
                    let
                        ( stylesStr, stylesVal ) =
                            toCssProps styles
                    in
                    [ ( "Css.Global.selector \"" ++ String.trim selector ++ "\" " ++ stylesStr
                      , Css.Global.selector selector stylesVal
                      )
                    ]

                _ ->
                    []
        )
        matches
        |> List.unzip
        |> Tuple.mapFirst (\props -> "[ Css.Global.descendants [ " ++ String.join "," props ++ " ] ]")
        |> Tuple.mapSecond (\props -> [ Css.Global.descendants props ])


toCssProps : String -> ( String, List Css.Style )
toCssProps rawStr =
    rawStr
        |> String.split ";"
        |> List.filterMap
            (\segment ->
                case String.split ":" segment of
                    name :: value :: [] ->
                        Just
                            ( "Css.property \"" ++ String.trim name ++ "\" \"" ++ String.trim value ++ "\""
                            , Css.property name value
                            )

                    _ ->
                        -- Unable to parse css
                        Nothing
            )
        |> List.unzip
        |> Tuple.mapFirst (\props -> "[ " ++ String.join "," props ++ " ]")
