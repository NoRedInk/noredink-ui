module Debug.Control.View exposing (view)

{-|

@docs view

-}

import Css exposing (..)
import Css.Global
import Css.Media exposing (withMedia)
import Debug.Control as Control exposing (Control)
import EllieLink
import Example
import ExampleSection
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Html.V3 exposing (viewIf)
import Nri.Ui.MediaQuery.V1 exposing (mobile)
import Nri.Ui.Spacing.V1 as Spacing
import Nri.Ui.Text.V6 as Text


{-| -}
view :
    { ellieLinkConfig : EllieLink.Config
    , name : String
    , version : Int
    , update : Control a -> msg
    , settings : Control a
    , mainType : Maybe String
    , extraCode : List String
    , renderExample : String -> String
    , toExampleCode : a -> List { sectionName : String, code : String }
    }
    -> Html msg
view config =
    let
        value =
            Control.currentValue config.settings

        ellieLink =
            EllieLink.view config.ellieLinkConfig

        exampleCodes =
            config.toExampleCode value
    in
    div
        [ css
            [ displayFlex
            , alignItems stretch
            , Css.flexWrap Css.wrap
            , Css.property "gap" "10px"
            , withMedia [ mobile ] [ flexDirection column, alignItems stretch ]
            , Spacing.pageTopWhitespace
            ]
        ]
        [ viewSection "Settings"
            [ div
                [ css
                    [ Css.Global.descendants
                        [ Css.Global.everything [ Fonts.baseFont ]
                        ]
                    ]
                ]
                [ Control.view config.update config.settings ]
            ]
        , viewIf
            (\_ -> viewExampleCode ellieLink config exampleCodes)
            (not (List.isEmpty exampleCodes))
        ]


viewExampleCode :
    (EllieLink.SectionExample -> Html msg)
    ->
        { component
            | name : String
            , version : Int
            , mainType : Maybe String
            , extraCode : List String
            , renderExample : String -> String
        }
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
                                , Heading.plaintext example.sectionName
                                ]
                            ]
                        , ellieLink
                            { fullModuleName = Example.fullName component
                            , name = component.name
                            , sectionName = example.sectionName
                            , mainType = component.mainType
                            , extraCode = component.extraCode
                            , renderExample = component.renderExample
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
viewSection name =
    ExampleSection.sectionWithCss name [ flex (int 1) ] (div [])
