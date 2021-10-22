module Debug.Control.Extra exposing
    ( float
    , list, listItem, optionalListItem
    )

{-|

@docs float
@docs list, listItem, optionalListItem

-}

import Debug.Control as Control exposing (Control)


{-| -}
float : Float -> Control Float
float default =
    Control.map (String.toFloat >> Maybe.withDefault default)
        (Control.string (String.fromFloat default))


{-| Use with `listItem` and `optionalListItem`
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
