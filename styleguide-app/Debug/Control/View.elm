module Debug.Control.View exposing
    ( view, viewWithCodeGen
    , codeFromList, codeFromListWithIndentLevel
    , withIndentLevel
    )

{-|

@docs view, viewWithCodeGen
@docs codeFromList, codeFromListWithIndentLevel
@docs withIndentLevel

-}

import Code
import Css exposing (..)
import Css.Global
import Css.Media exposing (withMedia)
import Debug.Control as Control exposing (Control)
import EllieLink
import Elm exposing (Expression)
import Elm.ToString
import Example
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Html.V3 exposing (viewIf)
import Nri.Ui.MediaQuery.V1 exposing (mobile)
import Nri.Ui.Text.V6 as Text


{-| -}
viewWithCodeGen :
    { ellieLinkConfig : EllieLink.Config
    , name : String
    , version : Int
    , update : Control a -> msg
    , settings : Control a
    , mainType : String
    , extraCode : List String
    , exampleCode : List { sectionName : String, code : Expression }
    }
    -> Html msg
viewWithCodeGen config =
    div
        [ css
            [ displayFlex
            , Css.flexWrap Css.wrap
            , Css.property "gap" "10px"
            , withMedia [ mobile ] [ flexDirection column, alignItems stretch ]
            ]
        ]
        [ viewSection "Settings"
            [ Css.Global.descendants [ Css.Global.everything [ Fonts.baseFont ] ] ]
            [ fromUnstyled (Control.view config.update config.settings) ]
        , viewIf
            (\_ ->
                viewExampleCodeWithCodeGen
                    (EllieLink.view config.ellieLinkConfig)
                    config
                    config.exampleCode
            )
            (not (List.isEmpty config.exampleCode))
        ]


viewExampleCodeWithCodeGen :
    (EllieLink.SectionExample -> Html msg)
    -> { component | name : String, version : Int, mainType : String, extraCode : List String }
    -> List { sectionName : String, code : Expression }
    -> Html msg
viewExampleCodeWithCodeGen ellieLink component values =
    viewSection "Code Sample" [] <|
        Text.smallBodyGray
            [ Text.plaintext "😎 Configure the \"Settings\" on this page to update the code sample, then paste it into your editor!"
            ]
            :: List.concatMap
                (\example ->
                    let
                        codeString : String
                        codeString =
                            (Elm.ToString.expression example.code).body
                    in
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
                            , code = codeString
                            }
                        , code
                            [ css
                                [ display block
                                , whiteSpace preWrap
                                , Css.marginTop (px 8)
                                ]
                            ]
                            [ text codeString ]
                        ]
                    ]
                )
                values


{-| -}
view :
    { ellieLinkConfig : EllieLink.Config
    , name : String
    , version : Int
    , update : Control a -> msg
    , settings : Control a
    , mainType : String
    , extraCode : List String
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
            , Css.flexWrap Css.wrap
            , Css.property "gap" "10px"
            , withMedia [ mobile ] [ flexDirection column, alignItems stretch ]
            ]
        ]
        [ viewSection "Settings"
            [ Css.Global.descendants [ Css.Global.everything [ Fonts.baseFont ] ] ]
            [ fromUnstyled (Control.view config.update config.settings) ]
        , viewIf
            (\_ -> viewExampleCode ellieLink config exampleCodes)
            (not (List.isEmpty exampleCodes))
        ]


viewExampleCode :
    (EllieLink.SectionExample -> Html msg)
    -> { component | name : String, version : Int, mainType : String, extraCode : List String }
    -> List { sectionName : String, code : String }
    -> Html msg
viewExampleCode ellieLink component values =
    viewSection "Code Sample" [] <|
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


viewSection : String -> List Css.Style -> List (Html msg) -> Html msg
viewSection name styles children =
    section
        [ css (flex (int 1) :: styles) ]
        (Heading.h2 [ Heading.plaintext name ]
            :: children
        )


codeFromList : List ( String, a ) -> String
codeFromList list =
    Code.list (List.map Tuple.first list)


codeFromListWithIndentLevel : Int -> List ( String, a ) -> String
codeFromListWithIndentLevel indent list =
    Code.listMultiline (List.map Tuple.first list) indent


withIndentLevel : Int -> String
withIndentLevel indent =
    String.repeat indent "    "
