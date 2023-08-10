module ExampleSection exposing (aside)

{-|

@docs aside

-}

import Html.Styled exposing (..)
import Nri.Ui.Container.V2 as Container
import Nri.Ui.Heading.V3 as Heading


{-| -}
aside : String -> (List item -> Html msg) -> List item -> Html msg
aside title renderItems list =
    case list of
        [] ->
            text ""

        _ ->
            Container.view
                [ Container.html
                    [ Heading.h2 [ Heading.plaintext title ]
                    , renderItems list
                    ]
                ]
                |> List.singleton
                |> Html.Styled.aside []
