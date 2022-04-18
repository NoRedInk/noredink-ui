module Debug.Control.View exposing (codeFromList, codeFromListWithIndentLevel, view)

import Css exposing (..)
import Css.Media exposing (withMedia)
import Debug.Control as Control exposing (Control)
import EllieLink
import Example
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.MediaQuery.V1 exposing (mobile)
import Nri.Ui.Text.V6 as Text


{-| -}
view :
    { ellieLinkConfig : EllieLink.Config
    , name : String
    , version : Int
    , update : Control a -> msg
    , settings : Control a
    , mainType : String
    , extraImports : List String
    , toExampleCode : a -> List { sectionName : String, code : String }
    }
    -> Html msg
view config =
    let
        value =
            Control.currentValue config.settings

        ellieLink =
            EllieLink.view config.ellieLinkConfig
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
        , viewExampleCode ellieLink config (config.toExampleCode value)
        ]


viewExampleCode :
    (EllieLink.SectionExample -> Html msg)
    -> { component | name : String, version : Int, mainType : String, extraImports : List String }
    -> List { sectionName : String, code : String }
    -> Html msg
viewExampleCode ellieLink component values =
    viewSection "Code Sample" <|
        Text.smallBodyGray
            [ Text.plaintext "😎 Configure the \"Settings\" on this page to update the code sample, then paste it into your editor!"
            ]
            :: List.concatMap
                (\example ->
                    [ details
                        []
                        [ summary []
                            [ Heading.h3
                                [ Heading.css [ Css.display Css.inline ]
                                , Heading.style Heading.Small
                                ]
                                [ text example.sectionName ]
                            ]
                        , ellieLink
                            { fullModuleName = Example.fullName component
                            , name = component.name
                            , sectionName = example.sectionName
                            , mainType = component.mainType
                            , extraImports = component.extraImports
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
        (Heading.h2 [ Heading.style Heading.Subhead ] [ text name ]
            :: children
        )


codeFromList : List ( String, a ) -> String
codeFromList =
    codeFromListWithIndentLevel 1


codeFromListWithIndentLevel : Int -> List ( String, a ) -> String
codeFromListWithIndentLevel indent list =
    let
        indents =
            String.repeat indent "\t"
    in
    "\n"
        ++ indents
        ++ "[ "
        ++ String.join ("\n" ++ indents ++ ", ") (List.map Tuple.first list)
        ++ "\n"
        ++ indents
        ++ "] "
