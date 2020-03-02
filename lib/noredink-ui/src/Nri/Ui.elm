module Nri.Ui exposing (styled)

{-| A collection of helpers for working with NoRedInk projects

@docs styled

-}

import Css exposing (Style)
import Html.Styled exposing (Attribute, Html)
import Html.Styled.Attributes exposing (attribute, css)


{-| Wrapper around [`Html.Styled.style`](http://package.elm-lang.org/packages/rtfeldman/elm-css/13.1.1/Html-Styled#styled) which adds a data-nri-description attribute to make it easier to tell from Inspect Element where in our code that element was defined.

Takes a function that creates an element, and pre-applies styles and a `data-nri-description` attribution to it.

    bigButton : List (Attribute msg) -> List (Html msg) -> Html msg
    bigButton =
        styled button
            "big button"
            [ padding (px 30)
            , fontWeight bold
            ]

    view : Model -> Html msg
    view model =
        [ text "These two buttons are identical:"
        , bigButton [] [ text "Hi!" ]
        , button [ css [ padding (px 30), fontWeight bold ] ] [] [ text "Hi!" ]
        ]

Here, the bigButton function we've defined using styled button is identical to the normal button function, except that it has pre-applied the attribute of css [ padding (px 30), fontWeight bold ], as well as `(attribute "data-nri-description" "big button")`.

You can pass more attributes to bigButton as usual (including other css attributes). They will be applied after the pre-applied styles.

Note: normally `attributeMsg` will be the same as `msg`, but we need them to be different types for special cases when `fn` needs to do tricky things. For example, some elements from the Accessibility.Styled package use the following type signature:

    div : List (Attribute Never) -> List (Html msg) -> Html msg

-}
styled : (List (Attribute attributeMsg) -> List (Html msg) -> Html msg) -> String -> List Style -> List (Attribute attributeMsg) -> List (Html msg) -> Html msg
styled fn description styles =
    -- Cache the computed css style so we only have to do the hashing once.
    -- Just like in https://github.com/rtfeldman/elm-css/pull/456
    let
        descriptionAttr =
            attribute "data-nri-description" description

        cssAttr =
            css styles
    in
    \attrs children ->
        fn (descriptionAttr :: cssAttr :: attrs) children
