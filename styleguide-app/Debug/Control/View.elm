module Debug.Control.View exposing (codeFromList, view)

import Css exposing (..)
import Css.Media exposing (withMedia)
import Debug.Control as Control exposing (Control)
import EllieLink
import Example
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.ClickableText.V3 as ClickableText
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.MediaQuery.V1 exposing (mobile)
import Nri.Ui.Text.V6 as Text


{-| -}
view :
    { name : String
    , version : Int
    , update : Control a -> msg
    , settings : Control a
    , toExampleCode : a -> List { sectionName : String, code : String }
    }
    -> Html msg
view config =
    let
        value =
            Control.currentValue config.settings
    in
    div
        [ css
            [ displayFlex
            , Css.flexWrap Css.wrap
            , Css.property "gap" "10px"
            , withMedia [ mobile ] [ flexDirection column, alignItems stretch ]
            ]
        ]
        [ viewSection "Settings" <|
            [ fromUnstyled (Control.view config.update config.settings) ]
        , viewExampleCode config (config.toExampleCode value)
        ]


viewExampleCode :
    { component | name : String, version : Int }
    -> List { sectionName : String, code : String }
    -> Html msg
viewExampleCode component values =
    viewSection "Code Sample" <|
        Text.smallBodyGray
            [ Text.plaintext "😎 Configure the \"Settings\" on this page to update the code sample, then paste it into your editor!"
            ]
            :: List.concatMap
                (\example ->
                    [ details
                        []
                        [ summary []
                            [ Heading.h4
                                [ Heading.css [ Css.display Css.inline ]
                                ]
                                [ text example.sectionName ]
                            ]
                        , EllieLink.view
                            { name = component.name
                            , version = component.version
                            , sectionName = example.sectionName
                            , code = example.code
                            }
                        , code
                            [ css
                                [ display block
                                , whiteSpace preWrap
                                , Css.marginTop (px 8)
                                ]
                            ]
                            [ text example.code ]
                        ]
                    ]
                )
                values


viewSection : String -> List (Html msg) -> Html msg
viewSection name children =
    section [ css [ flex (int 1) ] ]
        (Heading.h3 [] [ text name ]
            :: children
        )


codeFromList : List ( String, a ) -> String
codeFromList list =
    "\n\t[ "
        ++ String.join "\n\t, " (List.map Tuple.first list)
        ++ "\n\t] "
