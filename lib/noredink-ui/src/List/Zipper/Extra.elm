module List.Zipper.Extra exposing (from)

{-| migration module from List.Zipper 3.2 -> 4. We can drop this when we drop < 3.2 support
-}

import List.Zipper


from : List a -> a -> List a -> List.Zipper.Zipper a
from before current after =
    List.Zipper.singleton current
        |> List.Zipper.mapBefore (always before)
        |> List.Zipper.mapAfter (always after)
