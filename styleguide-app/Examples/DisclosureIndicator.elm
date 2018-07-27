module Examples.DisclosureIndicator exposing (Msg, State, example, init, update)

{- \
   @docs Msg, State, example, init, update,
-}

import Assets exposing (Assets, assets)
import Css
import Html.Styled as Html
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import ModuleExample as ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.DisclosureIndicator.V1 as DisclosureIndicatorV1


{-| -}
type Msg
    = DisclosureIndicatorToggle Bool
    | InlineDisclosureIndicatorToggle Bool


{-| -}
type alias State =
    { disclosed : Bool
    , inlineDisclosed : Bool
    }


{-| -}
example : (Msg -> msg) -> State -> ModuleExample msg
example parentMessage state =
    { filename = "ui/src/Nri/Ui/DisclosureIndicator/V1..elm"
    , category = Behaviors
    , content =
        [ Html.h3 [] [ Html.text "Panel indicator" ]
        , Html.div
            [ onClick
                (DisclosureIndicatorToggle <| not state.disclosed)
            ]
            [ DisclosureIndicatorV1.view assets
                { isOpen = state.disclosed
                , label = "Details"
                }
            ]
        , Html.h3 [] [ Html.text "Inline indicator" ]
        , Html.p []
            [ Html.text
                """
                The inline variant of the indicator is smaller and occupies
                less vertical space so it can be inlined in lists or tables
                without breaking text flow. Also, it rotates from right to
                down direction when expanding.
                """
            ]
        , Html.div
            [ css [ inlineIndicatorContainer ]
            , onClick (InlineDisclosureIndicatorToggle <| not state.inlineDisclosed)
            ]
            [ DisclosureIndicatorV1.viewInline assets
                { isOpen = state.inlineDisclosed
                , label = "Details"
                }
            , Html.span [] [ Html.text "list item with detail" ]
            ]
        ]
            |> List.map (Html.map parentMessage >> Html.toUnstyled)
    }


{-| -}
init : State
init =
    { disclosed = False
    , inlineDisclosed = False
    }


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        DisclosureIndicatorToggle disclosed ->
            ( { state | disclosed = disclosed }, Cmd.none )

        InlineDisclosureIndicatorToggle disclosed ->
            ( { state | inlineDisclosed = disclosed }, Cmd.none )


inlineIndicatorContainer : Css.Style
inlineIndicatorContainer =
    Css.batch
        [ Css.displayFlex
        , Css.alignItems Css.center
        ]
