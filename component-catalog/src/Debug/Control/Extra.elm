module Debug.Control.Extra exposing
    ( values, listItem, optionalListItem, optionalListItemDefaultChecked
    , optionalBoolListItem, optionalBoolListItemDefaultChecked
    , bool
    , rotatedChoice, specificChoice
    )

{-|

@docs values, listItem, optionalListItem, optionalListItemDefaultChecked
@docs optionalBoolListItem, optionalBoolListItemDefaultChecked
@docs bool
@docs rotatedChoice, specificChoice

-}

import Code
import Debug.Control as Control exposing (Control)
import List.Extra


{-| -}
values : (a -> String) -> List a -> Control a
values toString nums =
    nums
        |> List.map (\n -> ( toString n, Control.value n ))
        |> Control.choice


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
    optionalBoolListItem_ False name f accumulator


{-| -}
optionalBoolListItemDefaultChecked : String -> a -> Control (List a) -> Control (List a)
optionalBoolListItemDefaultChecked name f accumulator =
    optionalBoolListItem_ True name f accumulator


{-| -}
optionalBoolListItem_ : Bool -> String -> a -> Control (List a) -> Control (List a)
optionalBoolListItem_ startingValue name f accumulator =
    Control.field name
        (Control.map
            (\value ->
                if value then
                    [ f ]

                else
                    []
            )
            (Control.bool startingValue)
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
