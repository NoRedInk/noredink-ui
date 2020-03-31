module Examples.Icon exposing (example)

{-|

@docs example, styles

-}

import Category exposing (Category(..))
import Css
import Css.Global
import Example exposing (Example)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.AssetPath as AssetPath exposing (Asset(..))
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.Icon.V5 as Icon
import Nri.Ui.Text.V4 as Text


{-| -}
example : Example () ()
example =
    { name = "Nri.Ui.Icon.V5"
    , categories = List.singleton Icons
    , state = ()
    , update = \_ state -> ( state, Cmd.none )
    , subscriptions = \_ -> Sub.none
    , view =
        \_ ->
            [ viewLarge "Bulbs and Tips"
                [ deprecatedIcon { icon = Icon.lightBulb { hint_png = Asset "assets/images/hint.png" }, background = Colors.frost, alt = "LightBulb" }
                ]
            ]
    }


viewLarge :
    String
    -> List ( String, Html msg )
    -> Html msg
viewLarge headerText icons =
    Html.section [ css [ Css.marginTop (Css.px 16) ] ]
        [ Heading.h2 [] [ Html.text headerText ]
        , Html.div [ css [ Css.displayFlex, Css.flexWrap Css.wrap ] ]
            (List.map viewIcon icons)
        ]


viewIcon : ( String, Html msg ) -> Html msg
viewIcon ( name, assignmentIcon ) =
    Html.div
        [ css
            [ Css.margin (Css.px 10)
            , Css.width (Css.px 160)
            , Css.boxShadow4 (Css.px 10) (Css.px 5) (Css.px 5) Colors.navy
            , Css.displayFlex
            , Css.flexDirection Css.column
            , Css.alignItems Css.center
            , Css.justifyContent Css.flexStart
            ]
        ]
        [ Html.div
            [ css
                [ Css.height (Css.px 80)
                , Css.width (Css.px 80)
                , Css.margin (Css.px 10)
                , Css.color Colors.green
                ]
            ]
            [ assignmentIcon
            ]
        , Text.mediumBody [ Html.text name ]
        ]


deprecatedIcon : { alt : String, background : Css.Color, icon : Icon.IconType } -> ( String, Html msg )
deprecatedIcon { alt, background, icon } =
    ( alt
    , Html.div
        [ css
            [ Css.backgroundColor background
            , Css.height (Css.px 80)
            , Css.width (Css.px 80)
            , Css.displayFlex
            , Css.alignItems Css.center
            , Css.justifyContent Css.center
            , Css.Global.descendants
                [ Css.Global.img
                    [ Css.maxWidth (Css.pct 100)
                    , Css.maxHeight (Css.pct 100)
                    ]
                ]
            ]
        ]
        [ Icon.icon { alt = alt, icon = icon }
        ]
    )
