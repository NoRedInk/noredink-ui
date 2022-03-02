module Nri.Ui.Data.PremiumLevel exposing (PremiumLevel(..), allowedFor, highest, lowest)

{-|

@docs PremiumLevel, allowedFor, highest, lowest

-}


{-| -}
type PremiumLevel
    = Free
    | Premium
    | PremiumPromo
    | PremiumWithWriting


{-| Is content of the required premium level accessbile by the actor?
-}
allowedFor : PremiumLevel -> PremiumLevel -> Bool
allowedFor requirement actor =
    case requirement of
        Free ->
            True

        PremiumPromo ->
            True

        Premium ->
            actor /= Free

        PremiumWithWriting ->
            actor == PremiumWithWriting


{-| The highest premium level in a list
-}
highest : List PremiumLevel -> Maybe PremiumLevel
highest privileges =
    privileges
        |> List.sortBy order
        |> List.reverse
        |> List.head


{-| The lowest premium level in a list
-}
lowest : List PremiumLevel -> Maybe PremiumLevel
lowest privileges =
    privileges
        |> List.sortBy order
        |> List.head


order : PremiumLevel -> Int
order privileges =
    case privileges of
        PremiumWithWriting ->
            3

        Premium ->
            2

        PremiumPromo ->
            1

        Free ->
            0
