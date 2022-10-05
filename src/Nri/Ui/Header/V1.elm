module Nri.Ui.Header.V1 exposing
    ( view
    , Attribute, aTagAttributes, extraContent, description, extraSubheadContent
    )

{-|

@docs view
@docs Attribute, aTagAttributes, extraContent, description, extraSubheadContent

-}

import Accessibility.Styled as Html exposing (Html)
import Css
import Css.Media as Media
import Html.Styled.Attributes exposing (css)
import Nri.Ui.BreadCrumbs.V2 as BreadCrumbs exposing (BreadCrumbs)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Html.Attributes.V2 as AttributesExtra
import Nri.Ui.Html.V3 exposing (viewJust)
import Nri.Ui.MediaQuery.V1 as MediaQuery
import Nri.Ui.Spacing.V1 as Spacing
import Nri.Ui.Text.V6 as Text


{-| -}
type Attribute route msg
    = ATagAttributes (route -> List (Html.Attribute msg))
    | ExtraContent (List (Html msg))
    | ExtraSubheadContent (List (Html msg))
    | Description String


{-| -}
aTagAttributes : (route -> List (Html.Attribute msg)) -> Attribute route msg
aTagAttributes =
    ATagAttributes


{-| -}
extraContent : List (Html msg) -> Attribute route msg
extraContent =
    ExtraContent


{-| -}
extraSubheadContent : List (Html msg) -> Attribute route msg
extraSubheadContent =
    ExtraSubheadContent


{-| -}
description : String -> Attribute route msg
description =
    Description


type alias Config route msg =
    { aTagAttributes : route -> List (Html.Attribute msg)
    , containerAttributes : List (Html.Attribute Never)
    , extraContent : List (Html msg)
    , extraSubheadContent : List (Html msg)
    , description : Maybe String
    }


defaultConfig : Config route msg
defaultConfig =
    { aTagAttributes = \_ -> []
    , containerAttributes = []
    , extraContent = []
    , extraSubheadContent = []
    , description = Nothing
    }


customize : Config route msg -> List (Attribute route msg) -> Config route msg
customize =
    List.foldl
        (\attr soFar ->
            case attr of
                ATagAttributes aTagAttributes_ ->
                    { soFar | aTagAttributes = aTagAttributes_ }

                ExtraContent extraContent_ ->
                    { soFar | extraContent = extraContent_ }

                ExtraSubheadContent extraSubheadContent_ ->
                    { soFar | extraSubheadContent = extraSubheadContent_ }

                Description description_ ->
                    { soFar | description = Just description_ }
        )


{-| -}
view :
    List (Attribute route msg)
    ->
        { breadcrumbs : BreadCrumbs route
        , isCurrentRoute : route -> Bool
        }
    -> Html msg
view attrs { breadcrumbs, isCurrentRoute } =
    let
        config =
            customize defaultConfig attrs
    in
    Html.div
        [ css
            [ Css.backgroundColor Colors.gray96
            , Css.borderBottom3 (Css.px 1) Css.solid Colors.gray92
            ]
        , AttributesExtra.nriDescription "Nri-Header"
        ]
        [ Html.div
            (css
                [ Spacing.centeredContentWithSidePadding
                , Css.alignItems Css.center
                , Css.displayFlex
                , Css.paddingTop (Css.px 30)
                , Css.paddingBottom (Css.px 20)
                , Media.withMedia [ MediaQuery.mobile ]
                    [ Css.important (Css.padding2 (Css.px 20) (Css.px 15))
                    , Css.flexDirection Css.column
                    ]
                ]
                :: config.containerAttributes
            )
            (Html.div [ css [ Css.flexGrow (Css.num 1) ] ]
                [ let
                    breadcrumbsView =
                        BreadCrumbs.view
                            { isCurrentRoute = isCurrentRoute
                            , aTagAttributes = config.aTagAttributes
                            , label = "breadcrumbs"
                            }
                            breadcrumbs
                  in
                  case config.extraSubheadContent of
                    [] ->
                        breadcrumbsView

                    _ ->
                        Html.div [] (breadcrumbsView :: config.extraSubheadContent)
                ]
                :: config.extraContent
            )
        , viewJust viewDescription config.description
        ]


viewDescription : String -> Html msg
viewDescription description_ =
    Text.mediumBody
        [ Text.css
            [ Spacing.centeredContentWithSidePadding
            , Css.color Colors.gray45
            , Css.important (Css.margin Css.auto)
            , Css.important (Css.paddingBottom (Css.px 20))
            ]
        , Text.plaintext description_
        ]
