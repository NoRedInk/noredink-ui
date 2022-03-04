module Debug.Control.Extra exposing
    ( float, int
    , list, listItem, optionalListItem
    , css
    )

{-|

@docs float, int
@docs list, listItem, optionalListItem
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
css : String -> Control (List Css.Style)
css exampleCss =
    Control.map
        (\rawStr ->
            rawStr
                |> String.split ";"
                |> List.map
                    (\segment ->
                        case String.split ":" segment of
                            name :: value :: [] ->
                                Css.property name value

                            _ ->
                                -- Unable to parse css
                                Css.property "" ""
                    )
        )
        (Control.stringTextarea exampleCss)
