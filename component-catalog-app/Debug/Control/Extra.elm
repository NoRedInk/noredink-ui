module Debug.Control.Extra exposing
    ( float, int
    , values, list, listItem, optionalListItem, optionalListItemDefaultChecked
    , optionalBoolListItem
    , bool
    , rotatedChoice, specificChoice
    )

{-|

@docs float, int
@docs values, list, listItem, optionalListItem, optionalListItemDefaultChecked
@docs optionalBoolListItem
@docs bool
@docs rotatedChoice, specificChoice

-}

import Code
import Debug.Control as Control exposing (Control)
import List.Extra


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


{-| -}
values : (a -> String) -> List a -> Control a
values toString nums =
    nums
        |> List.map (\n -> ( toString n, Control.value n ))
        |> Control.choice


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
listItem name accessor =
    listItems name (Control.map List.singleton accessor)


{-| -}
listItems : String -> Control (List a) -> Control (List a) -> Control (List a)
listItems name accessor accumulator =
    Control.field name accessor (Control.map (++) accumulator)


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
bool : Bool -> Control ( String, Bool )
bool default =
    Control.map (\val -> ( Code.bool val, val )) (Control.bool default)


{-| -}
rotatedChoice : Int -> List ( String, Control a ) -> Control a
rotatedChoice rotateWith options =
    let
        ( before, after ) =
            List.Extra.splitAt rotateWith options
    in
    Control.choice (after ++ before)


{-| -}
specificChoice : String -> List ( String, Control a ) -> Control a
specificChoice match options =
    let
        ( before, after ) =
            List.Extra.break (Tuple.first >> (==) match) options
    in
    Control.choice (after ++ before)
