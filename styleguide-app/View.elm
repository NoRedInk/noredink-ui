module View exposing (view)

import Css exposing (..)
import Css.Foreign exposing (Snippet)
import DEPRECATED.Css.File exposing (Stylesheet, compile, stylesheet)
import DEPRECATED.Css.Namespace
import Html exposing (Html, img)
import Html.Attributes exposing (..)
import Html.CssHelpers
import Model exposing (..)
import ModuleExample as ModuleExample exposing (Category(..), ModuleExample, categoryForDisplay)
import Nri.Colors as Colors
import Nri.Fonts as Fonts
import Nri.Ui.Css.VendorPrefixed as VendorPrefixed
import NriModules as NriModules exposing (nriThemedModules)
import Routes as Routes exposing (Route)
import Update exposing (..)


view : Model -> Html Msg
view model =
    Html.div []
        [ attachElmCssStyles
        , Html.div [ class [ StyleGuideLayout ] ]
            [ navigation model.route
            , Html.div [ class [ StyleGuideContent ] ]
                (case model.route of
                    Routes.Doodad doodad ->
                        [ Html.h2 []
                            [ Html.a [ Html.Attributes.href "#" ] [ Html.text "(see all)" ] ]
                        , nriThemedModules model.moduleStates
                            |> List.filter (\m -> m.filename == ("Nri/" ++ doodad))
                            |> List.map (ModuleExample.view True)
                            |> Html.div []
                            |> Html.map UpdateModuleStates
                        ]

                    Routes.Category category ->
                        [ Html.section [ class [ Section ] ]
                            [ newComponentsLink
                            , Html.h2 [] [ Html.text (toString category) ]
                            , nriThemedModules model.moduleStates
                                |> List.filter (\doodad -> category == doodad.category)
                                |> List.map (ModuleExample.view True)
                                |> Html.div []
                                |> Html.map UpdateModuleStates
                            ]
                        ]

                    Routes.All ->
                        [ Html.section [ class [ Section ] ]
                            [ newComponentsLink
                            , Html.h2 [] [ Html.text "NRI-Themed Modules" ]
                            , Html.h3 [] [ Html.text "All Categories" ]
                            , nriThemedModules model.moduleStates
                                |> List.map (ModuleExample.view True)
                                |> Html.div []
                                |> Html.map UpdateModuleStates
                            ]
                        ]
                )
            ]
        ]


newComponentsLink : Html Msg
newComponentsLink =
    Html.div []
        [ Html.h2 [] [ Html.text "New Styleguide Components" ]
        , Html.div []
            [ Html.text "Future styleguide components can be found in "
            , Html.a [ href "https://app.zeplin.io/project/5973fb495395bdc871ebb055" ] [ Html.text "this Zepplin" ]
            , Html.text "."
            ]
        ]


navigation : Route -> Html Msg
navigation route =
    let
        isActive category =
            case route of
                Routes.Category routeCategory ->
                    category == routeCategory

                _ ->
                    False

        navLink category =
            Html.li []
                [ Html.a
                    [ classList
                        [ ( ActiveCategory, isActive category )
                        , ( NavLink, True )
                        ]
                    , Html.Attributes.href <| "#category/" ++ toString category
                    ]
                    [ Html.text (categoryForDisplay category) ]
                ]
    in
    Html.div [ class [ CategoryMenu ] ]
        [ Html.h4 []
            [ Html.text "Categories" ]
        , Html.ul [ class [ CategoryLinks ] ] <|
            Html.li []
                [ Html.a
                    [ Html.Attributes.href "#"
                    , classList
                        [ ( ActiveCategory, route == Routes.All )
                        , ( NavLink, True )
                        ]
                    ]
                    [ Html.text "All" ]
                ]
                :: List.map
                    navLink
                    [ Text
                    , TextWriting
                    , Colors
                    , Layout
                    , Inputs
                    , Buttons
                    , Icons
                    , Behaviors
                    , Messaging
                    , Modals
                    , Writing
                    , DynamicSymbols
                    , Pages
                    , QuestionTypes
                    ]
        ]


type Classes
    = Section
    | StyleGuideLayout
    | StyleGuideContent
    | CategoryMenu
    | CategoryLinks
    | ActiveCategory
    | NavLink


layoutFixer : List Snippet
layoutFixer =
    -- TODO: remove when universal header seizes power
    [ Css.Foreign.selector "#header-menu"
        [ Css.property "float" "none"
        ]
    , Css.Foreign.selector "#page-container"
        [ maxWidth (px 1400)
        ]
    , Css.Foreign.selector ".anonymous .log-in-button"
        [ Css.property "float" "none"
        , right zero
        , top zero
        ]
    , Css.Foreign.selector ".l-inline-blocks"
        [ textAlign right
        ]
    , Css.Foreign.everything
        [ Fonts.baseFont
        ]
    ]


styles : Stylesheet
styles =
    (stylesheet << DEPRECATED.Css.Namespace.namespace "Page-StyleGuide-") <|
        List.concat
            [ [ Css.Foreign.class Section
                    [ margin2 (px 40) zero
                    ]
              , Css.Foreign.class StyleGuideLayout
                    [ displayFlex
                    , alignItems flexStart
                    ]
              , Css.Foreign.class StyleGuideContent
                    [ flexGrow (int 1)
                    ]
              , Css.Foreign.class CategoryMenu
                    [ flexBasis (px 300)
                    , backgroundColor Colors.gray92
                    , marginRight (px 40)
                    , padding (px 25)
                    , VendorPrefixed.value "position" "sticky"
                    , top (px 150)
                    , flexShrink zero
                    ]
              , Css.Foreign.class CategoryLinks
                    [ margin4 zero zero (px 40) zero
                    , Css.Foreign.children
                        [ Css.Foreign.selector "li"
                            [ margin2 (px 10) zero
                            ]
                        ]
                    ]
              , Css.Foreign.class NavLink
                    [ backgroundColor transparent
                    , borderStyle none
                    , color Colors.azure
                    ]
              , Css.Foreign.class ActiveCategory
                    [ color Colors.navy
                    ]
              ]
            , layoutFixer
            ]


{ id, class, classList } =
    Html.CssHelpers.withNamespace "Page-StyleGuide-"


attachElmCssStyles : Html msg
attachElmCssStyles =
    Html.CssHelpers.style <|
        .css <|
            compile <|
                List.concat
                    [ [ styles ]
                    , NriModules.styles
                    ]
