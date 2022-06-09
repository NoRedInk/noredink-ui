module Nri.Ui.SlideModal.V2 exposing
    ( Config, Panel
    , State, closed, open
    , view
    )

{-| DEPRECATED -- do not use! The next major version of noredink-ui will remove this component.

@docs Config, Panel
@docs State, closed, open
@docs view

-}

import Accessibility.Styled as Html exposing (..)
import Accessibility.Styled.Aria as Aria exposing (labelledBy)
import Accessibility.Styled.Role as Role
import Accessibility.Styled.Style
import Css
import Css.Animations
import Css.Global
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Keyed as Keyed
import Nri.Ui
import Nri.Ui.Button.V10 as Button
import Nri.Ui.Colors.Extra
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.Slide.V1 as Slide exposing (AnimationDirection(..))
import SolidColor


{-| -}
type alias Config msg =
    { panels : List Panel
    , height : Css.Px
    , parentMsg : State -> msg
    }


{-| -}
type State
    = State
        { currentPanelIndex : Maybe Int
        , previousPanel : Maybe ( AnimationDirection, Panel )
        }


{-| Create the open state for the modal (the first panel will show).
-}
open : State
open =
    State
        { currentPanelIndex = Just 0
        , previousPanel = Nothing
        }


{-| Close the modal.
-}
closed : State
closed =
    State
        { currentPanelIndex = Nothing
        , previousPanel = Nothing
        }


{-| View the modal (includes the modal backdrop).
-}
view : Config msg -> State -> Html msg
view config ((State { currentPanelIndex }) as state) =
    case Maybe.andThen (summarize config.panels) currentPanelIndex of
        Just summary ->
            viewBackdrop
                (viewModal config state summary)

        Nothing ->
            Html.text ""


type alias Summary =
    { current : Panel
    , upcoming : List ( State, String )
    , previous : List ( State, String )
    }


summarize : List Panel -> Int -> Maybe Summary
summarize panels current =
    let
        indexedPanels =
            List.indexedMap (\i { title } -> ( i, title )) panels

        toOtherPanel direction currentPanel ( i, title ) =
            ( State
                { currentPanelIndex = Just i
                , previousPanel =
                    Just
                        ( direction
                        , { currentPanel | content = currentPanel.content }
                        )
                }
            , title
            )
    in
    case List.drop current panels of
        currentPanel :: rest ->
            Just
                { current = currentPanel
                , upcoming =
                    indexedPanels
                        |> List.drop (current + 1)
                        |> List.map (toOtherPanel FromRTL currentPanel)
                , previous =
                    indexedPanels
                        |> List.take current
                        |> List.map (toOtherPanel FromLTR currentPanel)
                }

        [] ->
            Nothing


viewModal : Config msg -> State -> Summary -> Html msg
viewModal config (State { previousPanel }) summary =
    Keyed.node "div"
        [ css
            [ Css.boxSizing Css.borderBox
            , Css.margin2 (Css.px 75) Css.auto
            , Css.backgroundColor Colors.white
            , Css.borderRadius (Css.px 20)
            , Css.property "box-shadow" "0 1px 10px 0 rgba(0, 0, 0, 0.35)"
            , Slide.withSlidingContents
            ]
        , Role.dialog
        , Aria.modal True
        , labelledBy (panelId summary.current)
        ]
        (case previousPanel of
            Just ( direction, panelView ) ->
                ( panelId panelView
                , panelContainer config.height
                    [ Slide.animateOut direction ]
                    [ viewIcon panelView.icon
                    , Heading.h3 []
                        [ span [ Html.Styled.Attributes.id (panelId panelView) ] [ Html.text panelView.title ]
                        ]
                    , viewContent panelView.content
                    ]
                )
                    :: viewModalContent config summary [ Slide.animateIn direction ]

            Nothing ->
                viewModalContent config summary []
        )


viewModalContent : Config msg -> Summary -> List Css.Style -> List ( String, Html msg )
viewModalContent config summary styles =
    [ ( panelId summary.current
      , panelContainer config.height
            styles
            [ viewIcon summary.current.icon
            , Heading.h3 []
                [ span
                    [ Html.Styled.Attributes.id (panelId summary.current)
                    , css [ Css.margin2 Css.zero (Css.px 21) ]
                    ]
                    [ Html.text summary.current.title ]
                ]
            , viewContent summary.current.content
            ]
      )
    , ( panelId summary.current ++ "-footer"
      , viewActiveFooter summary
            |> Html.map config.parentMsg
      )
    ]


viewBackdrop : Html msg -> Html msg
viewBackdrop modal =
    Nri.Ui.styled div
        "modal-backdrop-container"
        (Css.backgroundColor (Nri.Ui.Colors.Extra.withAlpha 0.9 Colors.navy)
            :: [ Css.height (Css.vh 100)
               , Css.left Css.zero
               , Css.overflowX Css.hidden
               , -- allow the user to scroll when the content doesn't fit the
                 -- viewport, but don't display a scrollbar if we don't need it
                 Css.overflowY Css.visible
               , Css.position Css.fixed
               , Css.top Css.zero
               , Css.width (Css.pct 100)
               , Css.zIndex (Css.int 200)
               , Css.displayFlex
               , Css.alignItems Css.center
               , Css.justifyContent Css.center
               ]
        )
        []
        [ -- This global <style> node sets overflow to hidden on the body element,
          -- thereby preventing the page from scrolling behind the backdrop when the modal is
          -- open (and this node is present on the page).
          Css.Global.global [ Css.Global.body [ Css.overflow Css.hidden ] ]
        , modal
        ]


{-| Configuration for a single modal view in the sequence of modal views.
-}
type alias Panel =
    { icon : Html Never
    , title : String
    , content : Html Never
    , buttonLabel : String
    }


panelContainer : Css.Px -> List Css.Style -> List (Html msg) -> Html msg
panelContainer height animationStyles panel =
    div
        [ css
            [ -- Layout
              Css.minHeight (Css.px 400)
            , Css.minHeight (Css.px 360)
            , Css.maxHeight <| Css.calc (Css.vh 100) Css.minus (Css.px 100)
            , Css.height height
            , Css.width (Css.px 600)

            -- Interior positioning
            , Css.displayFlex
            , Css.alignItems Css.center
            , Css.flexDirection Css.column
            , Css.flexWrap Css.noWrap

            -- Styles
            , Fonts.baseFont
            , Css.batch animationStyles
            ]
        ]
        panel


panelId : Panel -> String
panelId { title } =
    "modal-header__" ++ String.replace " " "-" title


viewContent : Html Never -> Html msg
viewContent content =
    Nri.Ui.styled div
        "modal-content"
        [ Css.overflowY Css.auto
        , Css.margin2 Css.zero (Css.px 21)
        , Css.paddingTop (Css.px 30)
        , Css.paddingRight (Css.px 40)
        , Css.paddingLeft (Css.px 40)

        -- , Css.paddingBottom Css.zero
        -- padding bottom used to be 30px. We removed it because it caused
        -- scrolling where we didn't want any
        , Css.width (Css.pct 100)
        , Css.height (Css.pct 100)
        , Css.marginBottom Css.auto
        , Css.boxSizing Css.borderBox

        -- Shadows for indicating that the content is scrollable
        , Css.property "background"
            """
            /* TOP shadow */

            top linear-gradient(to top, rgb(255, 255, 255), rgb(255, 255, 255)) local,
            top linear-gradient(to top, rgba(255, 255, 255, 0), rgba(0, 0, 0, 0.15)) scroll,

            /* BOTTOM shadow */

            bottom linear-gradient(to bottom, rgb(255, 255, 255), rgb(255, 255, 255)) local,
            bottom linear-gradient(to bottom, rgba(255, 255, 255, 0), rgba(0, 0, 0, 0.15)) scroll
            """
        , Css.backgroundSize2 (Css.pct 100) (Css.px 10)
        , Css.backgroundRepeat Css.noRepeat
        ]
        []
        [ Html.map never content ]


viewIcon : Html Never -> Html msg
viewIcon svg =
    div
        [ css
            [ Css.width (Css.px 100)
            , Css.height (Css.px 100)
            , Css.marginTop (Css.px 35)
            , Css.flexShrink Css.zero
            , Css.displayFlex
            , Css.alignItems Css.center
            , Css.justifyContent Css.center
            , Css.Global.children
                [ Css.Global.svg
                    [ Css.maxHeight (Css.px 100)
                    , Css.width (Css.px 100)
                    ]
                ]
            ]
        ]
        [ svg ]
        |> Html.map never


viewActiveFooter : Summary -> Html State
viewActiveFooter { previous, current, upcoming } =
    let
        nextPanel =
            List.head upcoming
                |> Maybe.map Tuple.first
                |> Maybe.withDefault closed

        dots =
            List.map (uncurry Inactive) previous
                ++ Active
                :: List.map (uncurry InactiveDisabled) upcoming
    in
    viewFlexibleFooter
        { buttonLabel = current.buttonLabel
        , buttonMsg = nextPanel
        , buttonState = Button.enabled
        }
        dots


viewFlexibleFooter :
    { buttonLabel : String
    , buttonMsg : msg
    , buttonState : Button.Attribute msg
    }
    -> List (Dot msg)
    -> Html msg
viewFlexibleFooter { buttonLabel, buttonMsg, buttonState } dotList =
    Nri.Ui.styled div
        "modal-footer"
        [ Css.flexShrink Css.zero
        , Css.displayFlex
        , Css.flexDirection Css.column
        , Css.alignItems Css.center
        , Css.margin4 (Css.px 20) Css.zero (Css.px 25) Css.zero
        , Css.minHeight (Css.px 125) -- so the footer doesn't compress on Safari
        ]
        []
        [ Button.button buttonLabel
            [ Button.onClick buttonMsg
            , Button.large
            , Button.exactWidth 230
            , buttonState
            ]
        , dotList
            |> List.map dot
            |> div [ css [ Css.marginTop (Css.px 16) ] ]
        ]


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( a, b ) =
    f a b


type Dot msg
    = Active
    | Inactive msg String
    | InactiveDisabled msg String


dot : Dot msg -> Html.Html msg
dot type_ =
    let
        styles ( startColor, endColor ) cursor =
            css
                [ Css.height (Css.px 10)
                , Css.width (Css.px 10)
                , Css.borderRadius (Css.px 5)
                , Css.margin2 Css.zero (Css.px 2)
                , Css.display Css.inlineBlock
                , Css.verticalAlign Css.middle
                , Css.cursor cursor

                -- Color
                , Css.animationDuration (Css.ms 600)
                , Css.property "animation-timing-function" "linear"
                , Css.animationName
                    (Css.Animations.keyframes
                        [ ( 0, [ animateBackgroundColor startColor ] )
                        , ( 100, [ animateBackgroundColor endColor ] )
                        ]
                    )
                , Css.backgroundColor endColor

                -- resets
                , Css.borderWidth Css.zero
                , Css.padding Css.zero
                , Css.hover [ Css.outline Css.none ]
                ]

        animateBackgroundColor color =
            Nri.Ui.Colors.Extra.fromCssColor color
                |> SolidColor.toRGBString
                |> Css.Animations.property "background-color"
    in
    case type_ of
        Active ->
            Html.div
                [ styles ( Colors.gray75, Colors.azure ) Css.auto
                ]
                []

        Inactive goTo title ->
            Html.button
                [ styles ( Colors.gray75, Colors.gray75 ) Css.pointer
                , onClick goTo
                ]
                [ span Accessibility.Styled.Style.invisible
                    [ text ("Go to " ++ title) ]
                ]

        InactiveDisabled goTo title ->
            Html.button
                [ styles ( Colors.gray75, Colors.gray75 ) Css.auto
                , Html.Styled.Attributes.disabled True
                ]
                [ span Accessibility.Styled.Style.invisible
                    [ text ("Go to " ++ title) ]
                ]
