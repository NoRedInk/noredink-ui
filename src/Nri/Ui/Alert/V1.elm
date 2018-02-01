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
import Css.Elements
import Html exposing (Html)
import Markdown
import Nri.Colors
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
        [ Css.class Alert
            [ Css.displayFlex
            , Css.fontSize (Css.px 13)
            , Css.lineHeight (Css.num 1.2)
            , Css.listStyleType Css.none
            , Css.overflow Css.hidden
            , Css.padding4 (Css.px 6) (Css.px 8) (Css.px 8) (Css.px 30)
            , Css.position Css.relative
            , Css.children
                [ Css.Elements.div
                    [ Css.children
                        [ Css.Elements.p
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
        , Css.class Error
            [ Css.color Nri.Colors.purple
            , Css.after
                [ Css.backgroundColor Nri.Colors.purple
                ]
            ]
        , Css.class Success
            [ Css.color Nri.Colors.greenDarkest
            , Css.after
                [ Css.backgroundColor Nri.Colors.green
                ]
            ]
        , Css.class Tip
            [ Css.color Nri.Colors.navy
            , Css.after
                [ Css.backgroundColor Nri.Colors.white
                ]
            ]
        , Css.class Warning
            [ Css.color Nri.Colors.red
            , Css.after
                [ Css.backgroundColor Nri.Colors.red
                ]
            ]
        ]
