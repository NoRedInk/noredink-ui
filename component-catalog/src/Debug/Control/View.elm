module Debug.Control.View exposing (view)

{-|

@docs view

-}

import Css exposing (..)
import Css.Media exposing (withMedia)
import Debug.Control as Control exposing (Control)
import EllieLink
import Example
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Container.V2 as Container
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.MediaQuery.V1 exposing (mobile, notMobile)
import Nri.Ui.Spacing.V1 as Spacing


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

        ellieLink example =
            EllieLink.view config.ellieLinkConfig
                { fullModuleName = Example.fullName config
                , name = config.name
                , sectionName = example.sectionName
                , mainType = config.mainType
                , extraCode = config.extraCode
                , renderExample = config.renderExample
                , code = example.code
                }

        exampleCodes =
            config.toExampleCode value
    in
    div
        [ css
            [ marginTop Spacing.verticalSpacerPx
            , displayFlex
            , withMedia [ mobile ] [ flexDirection column ]
            ]
        ]
        [ Container.view
            [ Container.html
                [ Heading.h2 [ Heading.plaintext "Settings" ]
                , Control.view config.update config.settings
                ]
            , Container.css
                [ withMedia [ mobile ]
                    [ borderBottomLeftRadius zero
                    , borderBottomRightRadius zero
                    ]
                , withMedia [ notMobile ]
                    [ flexBasis (pct 50)
                    , flexGrow zero
                    , flexShrink zero
                    , paddingRight (px 30)
                    , borderTopRightRadius zero
                    , borderBottomRightRadius zero
                    ]
                ]
            ]
        , Container.view
            [ Container.html
                (case exampleCodes of
                    singular :: [] ->
                        [ div
                            [ css
                                [ displayFlex
                                , flexWrap wrap
                                , property "gap" "5px"
                                , justifyContent spaceBetween
                                ]
                            ]
                            [ codeSampleHeading
                            , ellieLink singular
                            ]
                        , viewCode singular.code
                        ]

                    _ ->
                        codeSampleHeading
                            :: List.map (\example -> viewCodeDetails (ellieLink example) example)
                                exampleCodes
                )
            , Container.css
                [ padding (px 20)
                , flexGrow (num 1)
                , backgroundColor Colors.gray20
                , withMedia [ mobile ]
                    [ borderTopLeftRadius zero
                    , borderTopRightRadius zero
                    ]
                , withMedia [ notMobile ]
                    [ borderTopLeftRadius zero
                    , borderBottomLeftRadius zero
                    ]
                ]
            ]
        ]


codeSampleHeading : Html msg
codeSampleHeading =
    Heading.h2
        [ Heading.plaintext "Code Sample"
        , Heading.css [ color Colors.white ]
        ]


viewCodeDetails : Html msg -> { sectionName : String, code : String } -> Html msg
viewCodeDetails ellieLink example =
    details
        [ css
            [ paddingTop (px 5)
            , paddingBottom (px 5)
            , borderBottom3 (px 1) solid Colors.gray45
            , firstOfType [ marginTop (px 10) ]
            , lastChild [ borderWidth zero, paddingBottom zero ]
            ]
        ]
        [ summary [ css [ color Colors.yellow ] ]
            [ Heading.h3
                [ Heading.css [ display inline, color Colors.yellow ]
                , Heading.plaintext example.sectionName
                ]
            ]
        , div
            [ css
                [ displayFlex
                , flexWrap wrap
                , property "gap" "5px"
                , alignItems flexStart
                , justifyContent spaceBetween
                ]
            ]
            [ viewCode example.code, ellieLink ]
        ]


viewCode : String -> Html msg
viewCode code_ =
    code
        [ css
            [ display block
            , whiteSpace preWrap
            , marginTop (px 8)
            , color Colors.yellow
            ]
        ]
        [ text code_ ]
