module Nri.Ui.Container.V1 exposing
    ( alternate, general, disabled, invalid, interactable, interactableWithLabel
    , Attribute, paddingPx, fullHeight, css
    )

{-| Common NoRedInk Containers

@docs alternate, general, disabled, invalid, interactable, interactableWithLabel
@docs Attribute, paddingPx, fullHeight, css

-}

import Css exposing (..)
import Css.Media exposing (withMedia)
import Html.Styled exposing (..)
import Html.Styled.Attributes
import Nri.Ui
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.MediaQuery.V1 exposing (mobile)
import Nri.Ui.Text.V6 as Text


{-| -}
type Attribute
    = CustomPadding Float
    | FullHeight
    | Css (List Css.Style)


{-| Changes the padding inside the container border around the content.
-}
paddingPx : Float -> Attribute
paddingPx =
    CustomPadding


{-| Makes the container occupy the full height of the parent.
-}
fullHeight : Attribute
fullHeight =
    FullHeight


{-| -}
css : List Css.Style -> Attribute
css =
    Css


{-| PRIVATE
-}
type alias Settings =
    { padding : Float
    , fullHeight : Bool
    , css : List Css.Style
    }


{-| PRIVATE
-}
applyAttribute : Attribute -> Settings -> Settings
applyAttribute attribute styles =
    case attribute of
        CustomPadding padding ->
            { styles | padding = padding }

        FullHeight ->
            { styles | fullHeight = True }

        Css css_ ->
            { styles | css = css_ }


{-| PRIVATE
-}
renderContainer : String -> Settings -> List Style -> List Attribute -> List (Html msg) -> Html msg
renderContainer name defaultStyles baseStyles attributes content =
    let
        settings : Settings
        settings =
            List.foldl applyAttribute defaultStyles attributes
    in
    Nri.Ui.styled div
        name
        (baseStyles
            ++ [ padding (px settings.padding)
               , if settings.fullHeight then
                    height (pct 100)

                 else
                    batch []
               ]
            ++ settings.css
        )
        []
        content


{-| Used for the general container case.
-}
general : List Attribute -> Html msg -> Html msg
general attributes content =
    renderContainer "general-container"
        { padding = 20
        , fullHeight = False
        , css = []
        }
        [ borderRadius (px 8)
        , border3 (px 1) solid Colors.gray92
        , boxShadow5 zero (px 1) (px 1) zero (rgba 0 0 0 0.25)
        , backgroundColor Colors.white
        ]
        attributes
        [ content ]


{-| Used when there are a lot of containers.
-}
alternate : List Attribute -> Html msg -> Html msg
alternate attributes content =
    renderContainer "alternate-container"
        { padding = 20
        , fullHeight = False
        , css = []
        }
        [ borderRadius (px 8)
        , backgroundColor Colors.gray96
        ]
        attributes
        [ content ]


{-| -}
disabled : List Attribute -> Html msg -> Html msg
disabled attributes content =
    renderContainer "disabled-container"
        { padding = 20
        , fullHeight = False
        , css = []
        }
        [ borderRadius (px 8)
        , border3 (px 1) solid Colors.gray92
        , backgroundColor Colors.white
        , color Colors.gray45
        ]
        attributes
        [ content ]


{-| -}
invalid : List Attribute -> Html msg -> Html msg
invalid attributes content =
    renderContainer "invalid-container"
        { padding = 20
        , fullHeight = False
        , css = []
        }
        [ borderRadius (px 8)
        , border3 (px 1) solid Colors.purpleLight
        , boxShadow5 zero (px 1) (px 1) zero Colors.purple
        , backgroundColor Colors.purpleLight
        ]
        attributes
        [ content ]


{-| Used for containers of interactive elements.
-}
interactable : List Attribute -> Html msg -> Html msg
interactable attributes content =
    renderContainer "interactable-container"
        { padding = 40
        , fullHeight = False
        , css = []
        }
        interactableStyles
        attributes
        [ content ]


{-| Used for containers of with a label elements.
-}
interactableWithLabel : String -> Html msg -> Html msg
interactableWithLabel label content =
    renderContainer "interactable-container-with-label"
        { padding = 40
        , fullHeight = False
        , css = []
        }
        (interactableStyles
            ++ [ paddingTop (Css.px 50)
               , position relative
               ]
        )
        []
        [ Nri.Ui.styled div
            "label"
            labelStyles
            []
            [ Text.smallBody
                [ Text.html
                    [ div
                        [ Html.Styled.Attributes.css
                            [ margin (px 7)
                            ]
                        ]
                        [ text label
                        ]
                    ]
                ]
            ]
        , content
        ]


interactableStyles : List Style
interactableStyles =
    [ borderRadius (px 20)
    , border3 (px 1) solid Colors.gray92
    , boxShadow5 zero (px 2) (px 4) zero (rgba 0 0 0 0.25)
    , backgroundColor Colors.white
    , withMedia [ mobile ]
        [ borderRadius (px 8)
        , padding (px 20)
        ]
    ]


labelStyles : List Style
labelStyles =
    [ display inlineBlock
    , position absolute
    , left zero
    , top zero
    , height (px 40)
    , lineHeight (px 40)
    , borderRadius4 (px 20) (px 0) (px 8) (px 0)
    , padding2 (px 0) (px 20)
    , backgroundColor Colors.gray96
    ]
