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
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Container.V2 as Container
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Html.V3 exposing (viewIf)
import Nri.Ui.MediaQuery.V1 exposing (mobile, notMobile)
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
                    , paddingRight (px 30)
                    , borderTopRightRadius zero
                    , borderBottomRightRadius zero
                    ]
                ]
            ]
        , Container.view
            [ Container.html
                (Heading.h2
                    [ Heading.plaintext "Code Sample"
                    , Heading.css [ color Colors.white ]
                    ]
                    :: List.map (viewExampleCode ellieLink config) exampleCodes
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
    -> { sectionName : String, code : String }
    -> Html msg
viewExampleCode ellieLink component example =
    details
        []
        [ summary [ css [ color Colors.yellow ] ]
            [ Heading.h3
                [ Heading.css [ display inline, color Colors.yellow ]
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
                , marginTop (px 8)
                , color Colors.yellow
                ]
            ]
            [ text example.code ]
        ]
