module Nri.Ui.Header.V1 exposing
    ( view, Attribute
    , aTagAttributes, customPageWidth, breadCrumbsLabel
    , extraContent, description, extraNav
    )

{-|


## Changelog


### Patch

  - reduced the top-padding on the description from 20 to 5px


### Major release adjustments

  - removed extraSubheadContent

@docs view, Attribute


## Customize the header

@docs aTagAttributes, customPageWidth, breadCrumbsLabel


## Add additional content to the header

@docs extraContent, description, extraNav

-}

import Accessibility.Styled as Html exposing (Html)
import Accessibility.Styled.Aria as Aria
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
extraNav : String -> List (Html msg) -> Attribute route msg
extraNav label value =
    Attribute
        (\soFar ->
            { soFar
                | extraNav =
                    if List.isEmpty value then
                        Nothing

                    else
                        Just ( label, value )
            }
        )


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
    , description : Maybe String
    , pageWidth : Css.Px
    , breadCrumbsLabel : String
    , extraNav : Maybe ( String, List (Html msg) )
    }


customize : List (Attribute route msg) -> Config route msg
customize =
    List.foldl (\(Attribute f) -> f)
        { aTagAttributes = \_ -> []
        , containerAttributes = []
        , extraContent = []
        , description = Nothing
        , pageWidth = MediaQuery.mobileBreakpoint
        , breadCrumbsLabel = "breadcrumbs"
        , extraNav = Nothing
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

        ( extraContent_, extraNav_ ) =
            -- when there's no content in the "extra content" hole to the right of the breadcrumbs,
            -- put the extra nav  there. If there is content there, put the links directly above the description.
            case config.extraContent of
                [] ->
                    ( [ viewJust (viewExtraNav []) config.extraNav ]
                    , Html.text ""
                    )

                _ ->
                    ( config.extraContent
                    , viewJust
                        (viewExtraNav
                            [ Spacing.centeredContentWithSidePaddingAndCustomWidth config.pageWidth
                            ]
                        )
                        config.extraNav
                    )
    in
    Html.div
        [ css
            [ Css.backgroundColor Colors.gray96
            , Css.borderBottom3 (Css.px 1) Css.solid Colors.gray92
            , Css.paddingTop (Css.px 30)
            , Css.paddingBottom (Css.px 20)
            , Media.withMedia [ MediaQuery.mobile ]
                [ Css.important (Css.padding2 (Css.px 20) (Css.px 15))
                ]
            ]
        , AttributesExtra.nriDescription "Nri-Header"
        ]
        [ Html.div
            (css
                [ Spacing.centeredContentWithCustomWidth config.pageWidth
                , Css.alignItems Css.center
                , Css.displayFlex
                , Media.withMedia [ MediaQuery.mobile ] [ Css.flexDirection Css.column ]
                ]
                :: config.containerAttributes
            )
            (Html.div [ css [ Css.flexGrow (Css.num 1) ] ]
                [ BreadCrumbs.view
                    { isCurrentRoute = isCurrentRoute
                    , aTagAttributes = config.aTagAttributes
                    , label = config.breadCrumbsLabel
                    }
                    breadCrumbs
                ]
                :: extraContent_
            )
        , extraNav_
        , viewJust (viewDescription config.pageWidth) config.description
        ]


viewDescription : Css.Px -> String -> Html msg
viewDescription pageWidth description_ =
    Text.mediumBody
        [ Text.css
            [ Spacing.centeredContentWithCustomWidth pageWidth
            , Css.color Colors.gray45
            , Css.important (Css.margin Css.auto)
            , Css.important (Css.paddingTop (Css.px 5))
            ]
        , Text.plaintext description_
        ]


viewExtraNav : List Css.Style -> ( String, List (Html msg) ) -> Html msg
viewExtraNav styles ( label, values ) =
    Html.nav [ Aria.label label, css styles ]
        [ Html.ul
            [ css
                [ Css.margin Css.zero
                , Css.padding Css.zero
                , Css.displayFlex
                , Css.alignItems Css.center
                , Css.justifyContent Css.flexStart
                , Css.flexWrap Css.wrap
                , Css.property "column-gap" (.value Spacing.horizontalSpacerPx)
                ]
            ]
            (List.map
                (\i -> Html.li [ css [ Css.listStyle Css.none ] ] [ i ])
                values
            )
        ]
