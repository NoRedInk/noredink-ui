module View exposing (view)

import Browser exposing (Document)
import Css exposing (..)
import Headings
import Html as RootHtml
import Html.Attributes
import Html.Styled as Html exposing (Html, img)
import Html.Styled.Attributes as Attributes exposing (..)
import Model exposing (..)
import ModuleExample as ModuleExample exposing (Category(..), ModuleExample, categoryForDisplay)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Css.VendorPrefixed as VendorPrefixed
import Nri.Ui.Fonts.V1 as Fonts
import NriModules as NriModules exposing (nriThemedModules)
import Routes as Routes exposing (Route)
import Update exposing (..)


view : Model -> Document Msg
view model =
    { title = "Style Guide"
    , body = [ view_ model |> Html.toUnstyled ]
    }


view_ : Model -> Html Msg
view_ model =
    Html.styled Html.div
        [ displayFlex
        , alignItems flexStart
        ]
        []
        [ navigation model.route
        , Html.styled Html.div
            [ flexGrow (int 1) ]
            []
            (case model.route of
                Routes.Doodad doodad ->
                    [ Headings.h2
                        [ Html.a [ Attributes.href "#" ] [ Html.text "(see all)" ] ]
                    , nriThemedModules model.moduleStates
                        |> List.filter (\m -> m.name == doodad)
                        |> List.map (ModuleExample.view True)
                        |> Html.div []
                        |> Html.map UpdateModuleStates
                    ]

                Routes.Category category ->
                    [ Html.styled Html.section
                        [ sectionStyles ]
                        []
                        [ Headings.h2 [ Html.text (categoryForDisplay category) ]
                        , nriThemedModules model.moduleStates
                            |> List.filter (\doodad -> category == doodad.category)
                            |> List.map (ModuleExample.view True)
                            |> Html.div []
                            |> Html.map UpdateModuleStates
                        ]
                    ]

                Routes.All ->
                    [ Html.styled Html.section
                        [ sectionStyles ]
                        []
                        [ Headings.h2 [ Html.text "NRI-Themed Modules" ]
                        , Headings.h3 [ Html.text "All Categories" ]
                        , nriThemedModules model.moduleStates
                            |> List.map (ModuleExample.view True)
                            |> Html.div []
                            |> Html.map UpdateModuleStates
                        ]
                    ]
            )
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

        categoryLink active hash displayName =
            Html.styled Html.a
                [ backgroundColor transparent
                , borderStyle none
                , textDecoration none
                , if active then
                    color Colors.navy

                  else
                    color Colors.azure
                , Fonts.baseFont
                ]
                [ Attributes.href hash ]
                [ Html.text displayName ]

        navLink category =
            categoryLink (isActive category)
                ("#category/" ++ Debug.toString category)
                (categoryForDisplay category)

        toNavLi element =
            Html.li
                [ css
                    [ margin2 (px 10) zero
                    , listStyle none
                    , textDecoration none
                    ]
                ]
                [ element ]
    in
    Html.styled Html.div
        [ flexBasis (px 200)
        , backgroundColor Colors.gray92
        , marginRight (px 40)
        , padding (px 25)
        , VendorPrefixed.value "position" "sticky"
        , top (px 150)
        , flexShrink zero
        ]
        []
        [ Headings.h4
            [ Html.text "Categories" ]
        , (categoryLink (route == Routes.All) "#" "All"
            :: List.map
                navLink
                [ Messaging
                , Animations
                , Buttons
                , Colors
                , Pages
                , Icons
                , Inputs
                , Modals
                , Tables
                , Text
                , Widgets
                ]
          )
            |> List.map toNavLi
            |> Html.styled Html.ul
                [ margin4 zero zero (px 40) zero
                , padding zero
                ]
                []
        ]


sectionStyles : Css.Style
sectionStyles =
    Css.batch [ margin2 (px 40) zero ]
