module Debug.Control.Extra exposing
    ( float, int
    , list, listItem, optionalListItem, optionalBoolListItem
    , css
    )

{-|

@docs float, int
@docs list, listItem, optionalListItem, optionalBoolListItem
@docs css

-}

import Css
import Debug.Control as Control exposing (Control)


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
optionalListItem name accessor accumulator =
    Control.field name
        (Control.map (List.singleton >> List.filterMap identity) (Control.maybe False accessor))
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
        )
        (Control.stringTextarea exampleCss)
