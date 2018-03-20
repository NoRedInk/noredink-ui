module Nri.Ui.Alert.V1
    exposing
        ( Model
        , error
        , styles
        , success
        , tip
        , viewResult
        , warning
        )

{-| UI components that highlight information to the user.

@docs Model
@docs error
@docs styles
@docs success
@docs tip
@docs viewResult
@docs warning

-}

import Accessibility
import Css
import Css.Foreign exposing (Snippet, children, descendants, everything, selector)
import DEPRECATED.Css.File exposing (Stylesheet, compile, stylesheet)
import Html exposing (Html)
import Markdown
import Nri.Ui.Colors.V1
import Nri.Ui.Styles.V1


{-| -}
type alias Model =
    { content : String }


alert : CssClasses -> Model -> Html msg
alert className { content } =
    Accessibility.div
        [ styles.class [ Alert, className ]
        ]
        [ Accessibility.div [] (Markdown.toHtml Nothing content)
        ]


{-| Show either an error or success alert depending on the given Result
-}
viewResult : Result String String -> Html msg
viewResult result =
    case result of
        Ok msg ->
            success { content = msg }

        Err msg ->
            error { content = msg }


{-| -}
error : Model -> Html msg
error =
    alert Error


{-| -}
success : Model -> Html msg
success =
    alert Success


{-| -}
tip : Model -> Html msg
tip =
    alert Tip


{-| -}
warning : Model -> Html msg
warning =
    alert Warning


type CssClasses
    = Alert
    | Error
    | Success
    | Tip
    | Warning


{-| -}
styles : Nri.Ui.Styles.V1.Styles Never CssClasses msg
styles =
    Nri.Ui.Styles.V1.styles "Nri-Ui-Alert-"
        [ Css.Foreign.class Alert
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
        , Css.Foreign.class Error
            [ Css.color Nri.Ui.Colors.V1.purple
            , Css.after
                [ Css.backgroundColor Nri.Ui.Colors.V1.purple
                ]
            ]
        , Css.Foreign.class Success
            [ Css.color Nri.Ui.Colors.V1.greenDarkest
            , Css.after
                [ Css.backgroundColor Nri.Ui.Colors.V1.green
                ]
            ]
        , Css.Foreign.class Tip
            [ Css.color Nri.Ui.Colors.V1.navy
            , Css.after
                [ Css.backgroundColor Nri.Ui.Colors.V1.white
                ]
            ]
        , Css.Foreign.class Warning
            [ Css.color Nri.Ui.Colors.V1.red
            , Css.after
                [ Css.backgroundColor Nri.Ui.Colors.V1.red
                ]
            ]
        ]
