module Nri.Ui.SlideModal.V1 exposing
    ( Config, Panel
    , State, closed, open
    , view
    )

{-|

@docs Config, Panel
@docs State, closed, open
@docs view

-}

import Accessibility.Styled as Html exposing (..)
import Accessibility.Styled.Aria exposing (labelledBy)
import Accessibility.Styled.Role as Role
import Accessibility.Styled.Widget as Widget
import Css
import Css.Global
import Html.Styled
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Nri.Ui
import Nri.Ui.AssetPath exposing (Asset(..))
import Nri.Ui.Button.V8 as Button
import Nri.Ui.Colors.Extra
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Icon.V3 as Icon
import Nri.Ui.Text.V2 as Text


{-| -}
type alias Config msg =
    { panels : List (Panel msg)
    , height : Css.Vh
    , parentMsg : State -> msg
    }


{-| -}
type State
    = State (Maybe Int)


{-| Create the open state for the modal (the first panel will show).
-}
open : State
open =
    State (Just 0)


{-| Close the modal.
-}
closed : State
closed =
    State Nothing


{-| View the modal (includes the modal backdrop).
-}
view : Config msg -> State -> Html msg
view config (State state) =
    Maybe.andThen (viewPanels config.parentMsg config.panels) state
        |> Maybe.map (viewModal config.height >> viewBackdrop)
        |> Maybe.withDefault (Html.text "")


viewModal : Css.Vh -> ( String, List (Html msg) ) -> Html msg
viewModal height ( labelledById, panels ) =
    Nri.Ui.styled div
        "modal-container"
        [ Css.width (Css.px 600)
        , Css.height height
        , Css.maxHeight <| Css.calc (Css.vh 100) Css.minus (Css.px 100)
        , Css.padding4 (Css.px 35) Css.zero (Css.px 25) Css.zero
        , Css.margin2 (Css.px 75) Css.auto
        , Css.backgroundColor Colors.white
        , Css.borderRadius (Css.px 20)
        , Css.property "box-shadow" "0 1px 10px 0 rgba(0, 0, 0, 0.35)"
        , Css.displayFlex
        , Css.alignItems Css.center
        , Css.flexDirection Css.column
        , Css.flexWrap Css.noWrap
        , Fonts.baseFont
        ]
        [ Role.dialog
        , Widget.modal True
        , labelledBy labelledById
        ]
        panels


viewBackdrop : Html msg -> Html msg
viewBackdrop modal =
    Nri.Ui.styled div
        "modal-backdrop-container"
        (Css.backgroundColor (Nri.Ui.Colors.Extra.withAlpha 0.9 Colors.navy)
            :: [ Css.height (Css.vh 100)
               , Css.left Css.zero
               , Css.overflow Css.hidden
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


viewPanels : (State -> msg) -> List (Panel msg) -> Int -> Maybe ( String, List (Html msg) )
viewPanels parentMsg panels current =
    case List.drop current panels of
        [] ->
            Nothing

        head :: [] ->
            Just (viewPanel (parentMsg (State Nothing)) head)

        head :: _ ->
            Just (viewPanel (parentMsg (State (Just (current + 1)))) head)


{-| Configuration for a single modal view in the sequence of modal views.
-}
type alias Panel msg =
    { icon : Html Never
    , title : String
    , content : Html msg
    , buttonLabel : String
    }


viewPanel : msg -> Panel msg -> ( String, List (Html msg) )
viewPanel msg { icon, title, content, buttonLabel } =
    let
        id =
            "modal-header__" ++ String.replace " " "-" title
    in
    ( id
    , [ viewIcon icon
      , Text.subHeading [ span [ Html.Styled.Attributes.id id ] [ Html.text title ] ]
      , viewContent content
      , viewFooter { label = buttonLabel, msg = msg }
      ]
    )


viewContent : Html msg -> Html msg
viewContent content =
    Nri.Ui.styled div
        "modal-content"
        [ Css.overflowY Css.auto
        , Css.padding2 (Css.px 30) (Css.px 45)
        , Css.width (Css.pct 100)
        , Css.minHeight (Css.px 150)
        , Css.marginBottom Css.auto
        , Css.boxSizing Css.borderBox
        ]
        []
        [ content ]


viewIcon : Html Never -> Html msg
viewIcon svg =
    div
        [ css
            [ Css.width (Css.px 100)
            , Css.height (Css.px 100)
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


viewFooter : { label : String, msg : msg } -> Html msg
viewFooter button =
    Nri.Ui.styled div
        "modal-footer"
        [ Css.displayFlex
        , Css.flexDirection Css.column
        , Css.alignItems Css.center
        , Css.margin4 (Css.px 20) Css.zero Css.zero Css.zero
        ]
        []
        [ viewFooterButton button
        ]


viewFooterButton : { label : String, msg : msg } -> Html msg
viewFooterButton { label, msg } =
    Button.button
        { onClick = msg
        , size = Button.Large
        , style = Button.Primary
        , width = Button.WidthExact 230
        }
        { label = label
        , state = Button.Enabled
        , icon = Nothing
        }
