module Nri.Ui.Data.PremiumLevel exposing (PremiumLevel(..), allowedFor, highest, lowest)

{-|

@docs PremiumLevel, allowedFor, highest, lowest

-}


{-| -}
type PremiumLevel
    = Free
    | Premium
    | PremiumWithWriting


{-| Is content of the required premium level accessbile by the actor?
-}
allowedFor : PremiumLevel -> PremiumLevel -> Bool
allowedFor requirement actor =
    order requirement <= order actor


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
            2

        Premium ->
            1

        Free ->
            0
