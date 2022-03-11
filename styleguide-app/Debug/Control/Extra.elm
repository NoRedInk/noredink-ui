module Debug.Control.Extra exposing
    ( float, int
    , list, listItem, optionalListItem, optionalListItemDefaultChecked, optionalBoolListItem
    , dynamicList
    )

{-|

@docs float, int
@docs list, listItem, optionalListItem, optionalListItemDefaultChecked, optionalBoolListItem
@docs dynamicList

-}

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
optionalBoolListItem : String -> a -> Control (List a) -> Control (List a)
optionalBoolListItem name f accumulator =
    Control.field name
        (Control.map
            (\value ->
                if value then
                    [ f ]

                else
                    []
            )
            (Control.bool False)
        )
        (Control.map (++) accumulator)


{-| -}
dynamicList : Control a -> Control (List a)
dynamicList value =
    list
        |> listItem "Entry" value
        |> addAnother value


addAnother : Control a -> Control (List a) -> Control (List a)
addAnother value accumulator =
    Control.field "Add another?"
        (Control.map
            (\maybe ->
                case maybe of
                    Just l ->
                        l

                    Nothing ->
                        []
            )
            (Control.maybe False (Control.lazy (\() -> dynamicList value)))
        )
        (Control.map (++) accumulator)
