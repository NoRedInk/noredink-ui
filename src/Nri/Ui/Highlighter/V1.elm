module Nri.Ui.Highlighter.V1 exposing
    ( Model, Msg(..), PointerMsg(..), OnClickAction(..)
    , init, update, view, static
    , Intent, emptyIntent, hasChanged, HasChanged(..)
    , removeHighlights
    , asFragmentTuples, usedMarkers, text
    )

{-| Highlighter provides a view/model/update to display a view to highlight text.


## Current usage in NoRedInk

Currently, highlighter is used in the following places:

  - QuizEngine (see `staging.noredink.com/admin/grammar_questions/types`)
      - Highlighting Grammar Questions
      - Multi Highlighter Grammar Questions
      - Interface tutorial - parallel structure
      - Try similar for your answer vs correct answer
  - Peer Reviews (Thesis Statement)
      - Step 2: Rate Peers
  - Self Reviews (Thesis Statement)
      - On the "Highlight Your Writing" step.
  - Guided Drafts
      - When a teacher is grading a student's draft (leaving comments).


# Types

@docs Model, Msg, PointerMsg, OnClickAction


# Init/View/Update

@docs init, update, view, static


## Intents

@docs Intent, emptyIntent, hasChanged, HasChanged


# Setters

@docs removeHighlights


# Getters

@docs asFragmentTuples, usedMarkers, text


# Events

TODO: Add documentation about how to wire in event listeners and subscriptions so the highlighter is functional!

-}

import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Key as Key
import Accessibility.Styled.Role as Role
import Css
import Highlighter.Grouping as Grouping
import Highlighter.Internal as Internal
import Highlighter.Style as Style
import Html.Styled as Html exposing (Attribute, Html, div, p, span)
import Html.Styled.Attributes exposing (attribute, class, css, style)
import Html.Styled.Events
import Html.Styled.Lazy
import Json.Decode
import List.Extra
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Highlightable.V1 as Highlightable exposing (Highlightable)
import Nri.Ui.HighlighterTool.V1 as Tool
import Nri.Ui.Html.Attributes.V2 as AttributesExtra
import Sort exposing (Sorter)
import Sort.Set
import String.Extra



-- Model


{-| Model of a highlighter
-}
type alias Model marker =
    { -- Used to identify a highlighter. This is necessary when there are
      -- multiple highlighters on the same page because we add listeners
      -- in javascript (see ./highlighter.js).
      id : String
    , highlightables : List (Highlightable marker) -- The actual highlightable elements
    , marker : Tool.Tool marker -- Currently used marker
    , onClickAction : OnClickAction -- What happens when a user clicks on a highlight

    -- Internal state to track user's interactions
    , mouseDownIndex : Maybe Int
    , mouseOverIndex : Maybe Int
    , isInitialized : Initialized
    , hasChanged : HasChanged
    }


{-| -}
type HasChanged
    = Changed
    | NotChanged


{-| -}
type OnClickAction
    = ToggleOnClick
    | SaveOnClick


type Initialized
    = Initialized
    | NotInitialized


{-| Setup initial model
-}
init :
    { id : String
    , highlightables : List (Highlightable marker)
    , marker : Tool.Tool marker
    , onClickAction : OnClickAction
    }
    -> Model marker
init config =
    { id = config.id
    , highlightables = config.highlightables
    , marker = config.marker
    , onClickAction = config.onClickAction
    , mouseDownIndex = Nothing
    , mouseOverIndex = Nothing
    , isInitialized = NotInitialized
    , hasChanged = NotChanged
    }


{-| Get unique markers that have been used.
-}
usedMarkers : Sorter marker -> Model marker -> Sort.Set.Set marker
usedMarkers sorter { highlightables } =
    highlightables
        |> List.filterMap
            (\highlightable ->
                if String.Extra.isBlank highlightable.text then
                    Nothing

                else
                    highlightable.marked
                        |> Maybe.map .kind
            )
        |> Sort.Set.fromList sorter


{-| Get a list of fragment texts and whether or not they are marked.
Useful for encoding answers.
-}
asFragmentTuples : List (Highlightable marker) -> List ( Maybe marker, String )
asFragmentTuples highlightables =
    highlightables
        |> List.Extra.groupWhile (\a b -> a.groupIndex == b.groupIndex)
        |> List.map
            (\( first, rest ) ->
                ( first.marked
                    |> Maybe.map .kind
                , text (first :: rest)
                )
            )


{-| Fetch the text from a series of highlightables.
-}
text : List (Highlightable marker) -> String
text highlightables =
    List.map .text highlightables
        |> String.concat



-- UPDATE


{-| -}
type Msg marker
    = Pointer PointerMsg
    | Keyboard Int
    | NoOp


{-| Messages used by highlighter when interacting with a mouse or finger.
-}
type PointerMsg
    = Down Int
    | Out Int
    | Over Int
      -- the `Maybe String`s here are for detecting touchend events via
      -- subscription--we listen at the document level but get the id associated
      -- with the subscription when it fires messages. Mouse-triggered events
      -- will not have this info!
    | Move (Maybe String) Int
    | Up (Maybe String)


{-| Possible intents or "external effects" that the Highlighter can request (see `perform`).
-}
type Intent
    = Intent
        { listenTo : ListenTo
        , changed : HasChanged
        }


type alias ListenTo =
    Maybe String


{-| -}
emptyIntent : Intent
emptyIntent =
    Intent
        { listenTo = Nothing
        , changed = NotChanged
        }


{-| Get intent based on the resulting model from `update`.

  - This ensures that we initialize the highlighter in JS exactly once.
  - Sets the `hasChanged` flag if the model has changed. This is used by the user of `Highlighter` to
    determine whether they want to execute follow up actions.

-}
withIntent : Model m -> ( Model m, Intent )
withIntent new =
    ( { new | isInitialized = Initialized, hasChanged = NotChanged }
    , Intent
        { listenTo =
            case new.isInitialized of
                Initialized ->
                    Nothing

                NotInitialized ->
                    Just new.id
        , changed = new.hasChanged
        }
    )


{-| Check if the highlighter has changed.
-}
hasChanged : Intent -> HasChanged
hasChanged (Intent { changed }) =
    changed


{-| Actions are used as an intermediate algebra from pointer events to actual changes to the model.
-}
type Action marker
    = Blur Int
    | Hint Int Int
    | Hover Int
    | MouseDown Int
    | MouseOver Int
    | MouseUp
    | Remove
    | Save (Tool.MarkerModel marker)
    | Toggle Int (Tool.MarkerModel marker)


{-| Update for highlighter returning additional info about whether there was a change
-}
update : Msg marker -> Model marker -> ( Model marker, Intent )
update msg model =
    withIntent <|
        case msg of
            Pointer pointerMsg ->
                pointerEventToActions pointerMsg model
                    |> performActions model

            Keyboard index ->
                performActions model <|
                    case model.marker of
                        Tool.Marker marker ->
                            [ Toggle index marker ]

                        Tool.Eraser _ ->
                            [ MouseOver index
                            , Hint index index
                            , MouseUp
                            , Remove
                            ]

            NoOp ->
                model


{-| Pointer events to actions.
-}
pointerEventToActions : PointerMsg -> Model marker -> List (Action marker)
pointerEventToActions msg model =
    case msg of
        Move _ eventIndex ->
            case model.mouseDownIndex of
                Just downIndex ->
                    [ MouseOver eventIndex
                    , Hint downIndex eventIndex
                    ]

                Nothing ->
                    -- We're dealing with a touch move that hasn't been where
                    -- the initial touch down was not over a highlightable
                    -- region. We need to pretend like the first move into the
                    -- highlightable region was actually a touch down.
                    pointerEventToActions (Down eventIndex) model

        Over eventIndex ->
            case model.mouseDownIndex of
                Just downIndex ->
                    [ MouseOver eventIndex
                    , Hint downIndex eventIndex
                    ]

                Nothing ->
                    [ MouseOver eventIndex
                    , Hover eventIndex
                    ]

        Down eventIndex ->
            [ MouseOver eventIndex
            , MouseDown eventIndex
            , Hint eventIndex eventIndex
            ]

        Up _ ->
            let
                onClick index marker =
                    case model.onClickAction of
                        ToggleOnClick ->
                            [ Toggle index marker ]

                        SaveOnClick ->
                            [ Save marker ]

                save marker =
                    case ( model.mouseOverIndex, model.mouseDownIndex ) of
                        ( Just overIndex, Just downIndex ) ->
                            if overIndex == downIndex then
                                onClick downIndex marker

                            else
                                [ Save marker ]

                        ( Nothing, Just downIndex ) ->
                            [ Save marker ]

                        _ ->
                            []
            in
            case model.marker of
                Tool.Marker marker ->
                    MouseUp :: save marker

                Tool.Eraser _ ->
                    [ MouseUp, Remove ]

        Out eventIndex ->
            [ Blur eventIndex ]


{-| We fold over actions using (Model marker) as the accumulator.
-}
performActions : Model marker -> List (Action marker) -> Model marker
performActions model actions =
    List.foldl performAction model actions


{-| Performs actual changes to the model, or emit a command.
-}
performAction : Action marker -> Model marker -> Model marker
performAction action model =
    case action of
        Blur index ->
            { model | highlightables = Internal.blurAt index model.highlightables }

        Hover index ->
            { model | highlightables = Internal.hoverAt index model.highlightables }

        Hint start end ->
            { model | highlightables = Internal.hintBetween start end model.highlightables }

        Save marker ->
            { model
                | highlightables = Internal.saveHinted marker model.highlightables
                , hasChanged = Changed
            }

        Toggle index marker ->
            { model
                | highlightables = Internal.toggleHinted index marker model.highlightables
                , hasChanged = Changed
            }

        Remove ->
            { model
                | highlightables = Internal.removeHinted model.highlightables
                , hasChanged = Changed
            }

        MouseDown index ->
            { model | mouseDownIndex = Just index }

        MouseOver index ->
            { model | mouseOverIndex = Just index }

        MouseUp ->
            { model | mouseDownIndex = Nothing }


{-| -}
removeHighlights : List (Highlightable marker) -> List (Highlightable marker)
removeHighlights =
    Internal.removeHighlights



-- VIEWS


{-| -}
view : Model marker -> Html (Msg marker)
view =
    Html.Styled.Lazy.lazy <|
        \model ->
            model.highlightables
                |> Grouping.buildGroups
                |> List.map (viewHighlightable model)
                |> container model.id


{-| -}
static : { config | id : String, highlightables : List (Highlightable marker) } -> Html msg
static { id, highlightables } =
    highlightables
        |> Grouping.buildGroups
        |> List.map viewStaticHighlightable
        |> container id


container : String -> List (Html msg) -> Html msg
container id_ content =
    Html.div
        [ Html.Styled.Attributes.id id_
        , class "highlighter-container"
        , Role.grid
        , Aria.label "highlightable paragraph"
        , Aria.multiSelectable True
        ]
        [ div [ Role.row ] content ]


viewHighlightable : Model marker -> ( Grouping.Position, Highlightable marker ) -> Html (Msg marker)
viewHighlightable model ( groupPos, highlightable ) =
    case highlightable.type_ of
        Highlightable.Interactive ->
            viewMaybeMarker True
                [ on "mouseover" (Pointer <| Over highlightable.groupIndex)
                , on "mouseleave" (Pointer <| Out highlightable.groupIndex)
                , on "mouseup" (Pointer <| Up Nothing)
                , on "mousedown" (Pointer <| Down highlightable.groupIndex)
                , on "touchstart" (Pointer <| Down highlightable.groupIndex)
                , attribute "data-interactive" ""
                , Key.onKeyDownPreventDefault [ Key.space (Keyboard highlightable.groupIndex) ]
                ]
                (Just model.marker)
                ( groupPos, highlightable )

        Highlightable.Static ->
            viewMaybeMarker False
                -- Static highlightables need listeners as well.
                -- because otherwise we miss mouseup events
                [ on "mouseup" (Pointer <| Up Nothing)
                , on "mousedown" (Pointer <| Down highlightable.groupIndex)
                , on "touchstart" (Pointer <| Down highlightable.groupIndex)
                , attribute "data-static" ""
                ]
                (Just model.marker)
                ( groupPos, highlightable )


viewStaticHighlightable : ( Grouping.Position, Highlightable marker ) -> Html msg
viewStaticHighlightable =
    viewMaybeMarker False [] Nothing


viewMaybeMarker : Bool -> List (Attribute msg) -> Maybe (Tool.Tool marker) -> ( Grouping.Position, Highlightable marker ) -> Html msg
viewMaybeMarker isInteractive eventListeners maybeTool ( groupPos, highlightable ) =
    let
        whitespaceClass txt =
            -- we need to override whitespace styles in order to support
            -- student-provided paragraph indents in essay writing
            -- (specifically in Self Reviews)
            --
            -- TODO: there *has* to be a better way to do this, but what is it?
            -- Ideally we would be able to provide `List Css.Style` for these
            -- cases, since they'll probably be different for the quiz engine
            -- and essay writing.
            if txt == "\t" then
                [ class "highlighter-whitespace-tab" ]

            else if txt == " " then
                [ class "highlighter-whitespace-single-space" ]

            else if txt == "\n" then
                [ class "highlighter-whitespace-newline" ]

            else
                []

        ( spanOrMark, selected ) =
            case highlightable.marked of
                Just markedWith ->
                    ( Html.mark, True )

                Nothing ->
                    ( span, False )
    in
    span [ Role.gridCell, Aria.selected selected ]
        [ spanOrMark
            (eventListeners
                ++ customToHtmlAttributes highlightable.customAttributes
                ++ whitespaceClass highlightable.text
                ++ [ attribute "data-highlighter-item-index" <| String.fromInt highlightable.groupIndex
                   , Maybe.andThen .name highlightable.marked
                        |> Maybe.map (\name -> Aria.roleDescription (name ++ " highlight"))
                        |> Maybe.withDefault AttributesExtra.none
                   , css (highlightableStyle maybeTool highlightable isInteractive groupPos)
                   , class "highlighter-highlightable"
                   ]
            )
            [ Html.text highlightable.text ]
        ]


highlightableStyle : Maybe (Tool.Tool kind) -> Highlightable kind -> Bool -> Grouping.Position -> List Css.Style
highlightableStyle tool ({ uiState, marked } as highlightable) interactive groupPos =
    case tool of
        Nothing ->
            [ Style.staticHighlighted groupPos highlightable ]

        Just (Tool.Marker marker) ->
            [ Css.property "user-select" "none", Style.dynamicHighlighted marker groupPos interactive uiState marked ]

        Just (Tool.Eraser eraser_) ->
            case marked of
                Just markedWith ->
                    [ Css.property "user-select" "none"
                    , Css.batch markedWith.highlightClass
                    , Style.groupPosition groupPos markedWith
                    , Css.batch
                        (case uiState of
                            Highlightable.Hinted ->
                                [ Css.batch eraser_.hintClass, Style.groupPosition groupPos eraser_ ]

                            Highlightable.Hovered ->
                                [ Css.batch eraser_.hoverClass, Style.groupPosition groupPos eraser_ ]

                            _ ->
                                []
                        )
                    ]

                Nothing ->
                    [ Css.property "user-select" "none", Css.backgroundColor Css.transparent ]


{-| Helper for `on` to preventDefault.
-}
on : String -> msg -> Attribute msg
on name msg =
    let
        -- If we attempt to preventDefault on an event which is not cancelable
        -- Chrome will blow up and complain that:
        --
        -- Ignored attempt to cancel a touchmove event with cancelable=false,
        -- for example because scrolling is in progress and cannot be interrupted.
        --
        -- So instead we only preventDefault when it is safe to do so.
        checkIfCancelable =
            Json.Decode.field "cancelable" Json.Decode.bool
                |> Json.Decode.map (\result -> ( msg, result ))
    in
    Html.Styled.Events.preventDefaultOn name
        checkIfCancelable


customToHtmlAttributes : List Highlightable.Attribute -> List (Attribute msg)
customToHtmlAttributes =
    List.map
        (\attr ->
            case attr of
                Highlightable.Class name ->
                    class name

                Highlightable.Data name value ->
                    attribute ("data-" ++ name) value
        )
