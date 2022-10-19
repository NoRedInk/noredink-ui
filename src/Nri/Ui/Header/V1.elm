module Nri.Ui.Header.V1 exposing
    ( view
    , Attribute, aTagAttributes, extraContent, description, extraSubheadContent, customPageWidth, breadCrumbsLabel
    )

{-|

@docs view
@docs Attribute, aTagAttributes, extraContent, description, extraSubheadContent, customPageWidth, breadCrumbsLabel

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
    = Attribute (Config route msg -> Config route msg)


{-| -}
aTagAttributes : (route -> List (Html.Attribute msg)) -> Attribute route msg
aTagAttributes aTagAttributes_ =
    Attribute (\soFar -> { soFar | aTagAttributes = aTagAttributes_ })


{-| -}
extraContent : List (Html msg) -> Attribute route msg
extraContent value =
    Attribute (\soFar -> { soFar | extraContent = value })


{-| -}
extraSubheadContent : List (Html msg) -> Attribute route msg
extraSubheadContent value =
    Attribute (\soFar -> { soFar | extraSubheadContent = value })


{-| -}
description : String -> Attribute route msg
description description_ =
    Attribute (\soFar -> { soFar | description = Just description_ })


{-| By default, the content within the header will expand up to 1000px (the mobile breakpoint value).

For some views, you may want to use MediaQuery.quizEngineBreakpoint instead.

-}
customPageWidth : Css.Px -> Attribute route msg
customPageWidth pageWidth =
    Attribute (\soFar -> { soFar | pageWidth = pageWidth })


{-| Default label is "breadcrumbs". You will seldom need override the default.
-}
breadCrumbsLabel : String -> Attribute route msg
breadCrumbsLabel label =
    Attribute (\soFar -> { soFar | breadCrumbsLabel = label })


type alias Config route msg =
    { aTagAttributes : route -> List (Html.Attribute msg)
    , containerAttributes : List (Html.Attribute Never)
    , extraContent : List (Html msg)
    , extraSubheadContent : List (Html msg)
    , description : Maybe String
    , pageWidth : Css.Px
    , breadCrumbsLabel : String
    }


customize : List (Attribute route msg) -> Config route msg
customize =
    List.foldl (\(Attribute f) -> f)
        { aTagAttributes = \_ -> []
        , containerAttributes = []
        , extraContent = []
        , extraSubheadContent = []
        , description = Nothing
        , pageWidth = MediaQuery.mobileBreakpoint
        , breadCrumbsLabel = "breadcrumbs"
        }


{-| -}
view :
    List (Attribute route msg)
    ->
        { breadCrumbs : BreadCrumbs route
        , isCurrentRoute : route -> Bool
        }
    -> Html msg
view attrs { breadCrumbs, isCurrentRoute } =
    let
        config =
            customize attrs
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
                [ Spacing.centeredContentWithSidePaddingAndCustomWidth config.pageWidth
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
                            , label = config.breadCrumbsLabel
                            }
                            breadCrumbs
                  in
                  case config.extraSubheadContent of
                    [] ->
                        breadcrumbsView

                    _ ->
                        Html.div [] (breadcrumbsView :: config.extraSubheadContent)
                ]
                :: config.extraContent
            )
        , viewJust (viewDescription config.pageWidth) config.description
        ]


viewDescription : Css.Px -> String -> Html msg
viewDescription pageWidth description_ =
    Text.mediumBody
        [ Text.css
            [ Spacing.centeredContentWithSidePaddingAndCustomWidth pageWidth
            , Css.color Colors.gray45
            , Css.important (Css.margin Css.auto)
            , Css.important (Css.paddingBottom (Css.px 20))
            ]
        , Text.plaintext description_
        ]
