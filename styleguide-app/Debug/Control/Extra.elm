module Debug.Control.Extra exposing
    ( float, int
    , list, listItem, optionalListItem, optionalListItemDefaultChecked
    , optionalBoolListItem, optionalBoolListItemDefaultTrue
    , bool
    , string
    , rotatedChoice
    )

{-|

@docs float, int
@docs list, listItem, optionalListItem, optionalListItemDefaultChecked
@docs optionalBoolListItem, optionalBoolListItemDefaultTrue
@docs bool
@docs string
@docs rotatedChoice

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


optionalBoolListItemDefaultTrue : String -> a -> Control (List a) -> Control (List a)
optionalBoolListItemDefaultTrue name f accumulator =
    Control.field name
        (Control.map
            (\value ->
                if not value then
                    [ f ]

                else
                    []
            )
            (Control.bool True)
        )
        (Control.map (++) accumulator)


{-| -}
bool : Bool -> Control ( String, Bool )
bool default =
    Control.map (\val -> ( Code.bool val, val )) (Control.bool default)


{-| -}
string : String -> Control ( String, String )
string default =
    Control.map (\val -> ( Code.string val, val )) (Control.string default)


{-| -}
rotatedChoice : Int -> List ( String, Control a ) -> Control a
rotatedChoice rotateWith options =
    let
        ( before, after ) =
            List.Extra.splitAt rotateWith options
    in
    Control.choice (after ++ before)
