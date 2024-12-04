module ExampleSection exposing
    ( aboutSection
    , asideWithCss, sectionWithCss
    )

{-|

@docs aboutSection
@docs asideWithCssm sectionWithCss

-}

import Css exposing (Style)
import Css.Global
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Container.V2 as Container
import Nri.Ui.Heading.V3 as Heading


aboutSection : List (Html Never) -> Html msg
aboutSection =
    sectionWithCss "About"
        [ Css.flex (Css.int 1) ]
        viewAbout


viewAbout : List (Html Never) -> Html msg
viewAbout about =
    div
        [ css
            [ Css.margin2 (Css.px 10) Css.zero
            , Css.Global.descendants
                [ Css.Global.code
                    [ Css.fontSize (Css.px 13.5) ]
                ]
            ]
        ]
        about
        |> map never


{-| -}
asideWithCss : String -> List Style -> (List item -> Html msg) -> List item -> Html msg
asideWithCss title styles renderItems list =
    container title (Html.Styled.aside [ css styles ]) renderItems list


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
