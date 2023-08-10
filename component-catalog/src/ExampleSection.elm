module ExampleSection exposing
    ( asideWithCss
    , section, sectionWithCss
    )

{-|

@docs asideWithCss
@docs section, sectionWithCss

-}

import Css exposing (Style)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Container.V2 as Container
import Nri.Ui.Heading.V3 as Heading


{-| -}
asideWithCss : String -> List Style -> (List item -> Html msg) -> List item -> Html msg
asideWithCss title styles renderItems list =
    container title (Html.Styled.aside [ css styles ]) renderItems list


{-| -}
section : String -> (List item -> Html msg) -> List item -> Html msg
section title renderItems list =
    container title (Html.Styled.section []) renderItems list


{-| -}
sectionWithCss : String -> List Style -> (List item -> Html msg) -> List item -> Html msg
sectionWithCss title styles renderItems list =
    container title (Html.Styled.section [ css styles ]) renderItems list


container :
    String
    -> (List (Html msg) -> Html msg)
    -> (List item -> Html msg)
    -> List item
    -> Html msg
container title node renderItems list =
    case list of
        [] ->
            text ""

        _ ->
            node
                [ Container.view
                    [ Container.html
                        [ Heading.h2 [ Heading.plaintext title ]
                        , renderItems list
                        ]
                    , Container.css [ Css.height (Css.pct 100) ]
                    ]
                ]
