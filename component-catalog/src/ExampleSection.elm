module ExampleSection exposing (aside, section)

{-|

@docs aside, section

-}

import Html.Styled exposing (..)
import Nri.Ui.Container.V2 as Container
import Nri.Ui.Heading.V3 as Heading


{-| -}
aside : String -> (List item -> Html msg) -> List item -> Html msg
aside title renderItems list =
    Html.Styled.aside [] [ container title renderItems list ]


section : String -> (List item -> Html msg) -> List item -> Html msg
section title renderItems list =
    Html.Styled.section [] [ container title renderItems list ]


container : String -> (List item -> Html msg) -> List item -> Html msg
container title renderItems list =
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
