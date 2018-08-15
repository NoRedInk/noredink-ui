module Nri.Ui.Alert.V2
    exposing
        ( error
        , success
        , tip
        , viewResult
        , warning
        )

{-| UI components that highlight information to the user.

@docs error
@docs success
@docs tip
@docs viewResult
@docs warning

-}

import Accessibility.Styled as Html exposing (Html)
import Css
import Css.Foreign exposing (Snippet, children, descendants, everything, selector)
import Html as RootHtml
import Html.Styled exposing (fromUnstyled)
import Html.Styled.Attributes exposing (css)
import Markdown
import Nri.Ui.Colors.V1


{-| Show either an error or success alert depending on the given Result
-}
viewResult : Result String String -> Html msg
viewResult result =
    case result of
        Ok msg ->
            success msg

        Err msg ->
            error msg


{-| -}
error : String -> Html msg
error =
    alert
        [ Css.color Nri.Ui.Colors.V1.purple
        , Css.after [ Css.backgroundColor Nri.Ui.Colors.V1.purple ]
        ]


{-| -}
success : String -> Html msg
success =
    alert
        [ Css.color Nri.Ui.Colors.V1.greenDarkest
        , Css.after [ Css.backgroundColor Nri.Ui.Colors.V1.green ]
        ]


{-| -}
tip : String -> Html msg
tip =
    alert
        [ Css.color Nri.Ui.Colors.V1.navy
        , Css.after [ Css.backgroundColor Nri.Ui.Colors.V1.white ]
        ]


{-| -}
warning : String -> Html msg
warning =
    alert
        [ Css.color Nri.Ui.Colors.V1.red
        , Css.after [ Css.backgroundColor Nri.Ui.Colors.V1.red ]
        ]


alert : List Css.Style -> String -> Html msg
alert styles content =
    Html.div [ css (alertStyles ++ styles) ]
        [ RootHtml.div [] (Markdown.toHtml Nothing content)
            |> fromUnstyled
        ]


alertStyles : List Css.Style
alertStyles =
    [ Css.displayFlex
    , Css.fontSize (Css.px 13)
    , Css.lineHeight (Css.num 1.2)
    , Css.listStyleType Css.none
    , Css.overflow Css.hidden
    , Css.padding4 (Css.px 6) (Css.px 8) (Css.px 8) (Css.px 30)
    , Css.position Css.relative
    , Css.Foreign.children
        [ Css.Foreign.div
            [ Css.Foreign.children
                [ Css.Foreign.p
                    [ Css.margin Css.zero
                    ]
                ]
            ]
        ]
    , Css.after
        [ Css.backgroundPosition Css.center
        , Css.backgroundRepeat Css.noRepeat
        , Css.borderRadius (Css.px 13)
        , Css.height (Css.px 25)
        , Css.left Css.zero
        , Css.position Css.absolute
        , Css.property "content" "\"\""
        , Css.top Css.zero
        , Css.width (Css.px 25)
        ]
    ]
