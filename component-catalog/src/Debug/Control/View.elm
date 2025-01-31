module Debug.Control.View exposing
    ( view, viewWithCustomControls
    , viewCode
    )

{-|

@docs view, viewWithCustomControls

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
    viewWithCustomControls
        { ellieLinkConfig = config.ellieLinkConfig
        , name = config.name
        , version = config.version
        , controls = Control.view config.update config.settings
        , mainType = config.mainType
        , extraCode = config.extraCode
        , renderExample = config.renderExample
        , exampleCode = config.toExampleCode (Control.currentValue config.settings)
        }


viewWithCustomControls :
    { ellieLinkConfig : EllieLink.Config
    , name : String
    , version : Int
    , controls : Html msg
    , mainType : Maybe String
    , extraCode : List String
    , renderExample : String -> String
    , exampleCode : List { sectionName : String, code : String }
    }
    -> Html msg
viewWithCustomControls config =
    let
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
                , config.controls
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
                (case config.exampleCode of
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
                                config.exampleCode
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
        [ summary [ css [ color Colors.mustard ] ]
            [ Heading.h3
                [ Heading.css [ display inline, color Colors.mustard ]
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
            , color Colors.mustard
            ]
        ]
        [ text code_ ]
