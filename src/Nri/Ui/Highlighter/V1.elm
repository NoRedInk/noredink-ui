module Nri.Ui.Highlighter.V1 exposing
    ( Model, Msg(..), PointerMsg(..)
    , init, update, view, static, staticWithTags
    , Intent(..), emptyIntent, hasChanged, HasChanged(..)
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

@docs Model, Msg, PointerMsg


# Init/View/Update

@docs init, update, view, static, staticWithTags


## Intents

@docs Intent, emptyIntent, hasChanged, HasChanged


# Setters

@docs removeHighlights


# Getters

@docs asFragmentTuples, usedMarkers, text


# Events

TODO: Add documentation about how to wire in event listeners and subscriptions so the highlighter is functional!

-}

import Accessibility.Styled.Key as Key
import Browser.Dom as Dom
import Css
import Highlighter.Grouping as Grouping
import Highlighter.Internal as Internal
import Html.Styled as Html exposing (Attribute, Html, p, span)
import Html.Styled.Attributes exposing (attribute, class, css)
import Html.Styled.Events as Events
import Json.Decode
import List.Extra
import Nri.Ui.Highlightable.V1 as Highlightable exposing (Highlightable)
import Nri.Ui.HighlighterTool.V1 as Tool
import Nri.Ui.Html.Attributes.V2 as AttributesExtra
import Nri.Ui.Mark.V1 as Mark
import Sort exposing (Sorter)
import Sort.Set
import String.Extra
import Task



-- Model


{-| Model of a highlighter
-}
type alias Model marker =
    { -- Used to identify a highlighter. This is necessary when there are
      -- multiple highlighters on the same page because we add listeners
      -- in javascript (see ./highlighter.js) and because we move focus by id for keyboard users.
      id : String
    , highlightables : List (Highlightable marker) -- The actual highlightable elements
    , marker : Tool.Tool marker -- Currently used marker

    -- Internal state to track user's interactions
    , mouseDownIndex : Maybe Int
    , mouseOverIndex : Maybe Int
    , isInitialized : Initialized
    , hasChanged : HasChanged
    , selectionStartIndex : Maybe Int
    , selectionEndIndex : Maybe Int
    , focusIndex : Maybe Int
    }


{-| -}
type HasChanged
    = Changed
    | NotChanged


type Initialized
    = Initialized
    | NotInitialized


{-| Setup initial model
-}
init :
    { id : String
    , highlightables : List (Highlightable marker)
    , marker : Tool.Tool marker
    }
    -> Model marker
init config =
    { id = config.id
    , highlightables = config.highlightables
    , marker = config.marker
    , mouseDownIndex = Nothing
    , mouseOverIndex = Nothing
    , isInitialized = NotInitialized
    , hasChanged = NotChanged
    , selectionStartIndex = Nothing
    , selectionEndIndex = Nothing
    , focusIndex =
        List.Extra.findIndex (\highlightable -> .type_ highlightable == Highlightable.Interactive) config.highlightables
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
    | Keyboard KeyboardMsg
    | Focused (Result Dom.Error ())


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
    | Ignored


type KeyboardMsg
    = MoveLeft Int
    | MoveRight Int
    | SelectionExpandLeft Int
    | SelectionExpandRight Int
    | SelectionApplyTool Int
    | SelectionReset Int
    | ToggleHighlight Int


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
withIntent : ( Model m, Cmd (Msg m) ) -> ( Model m, Cmd (Msg m), Intent )
withIntent ( new, cmd ) =
    ( { new | isInitialized = Initialized, hasChanged = NotChanged }
    , cmd
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
    = Focus Int
    | Blur Int
    | Hint Int Int
    | Hover Int
    | MouseDown Int
    | MouseOver Int
    | MouseUp
    | Remove
    | Save (Tool.MarkerModel marker)
    | Toggle Int (Tool.MarkerModel marker)
    | StartSelection Int
    | ExpandSelection Int
    | ResetSelection


{-| Update for highlighter returning additional info about whether there was a change
-}
update : Msg marker -> Model marker -> ( Model marker, Cmd (Msg marker), Intent )
update msg model =
    withIntent <|
        case msg of
            Pointer pointerMsg ->
                pointerEventToActions pointerMsg model
                    |> performActions model

            Keyboard keyboardMsg ->
                keyboardEventToActions keyboardMsg model
                    |> performActions model

            Focused _ ->
                ( model, Cmd.none )


nextInteractiveIndex : Int -> List (Highlightable marker) -> Maybe Int
nextInteractiveIndex index highlightables =
    let
        isInteractive highlightable =
            .type_ highlightable == Highlightable.Interactive

        interactiveHighlightables =
            List.filter isInteractive highlightables
    in
    List.foldl
        (\x ( maybeNextIndex, hasIndexMatched ) ->
            if hasIndexMatched then
                ( Just x.groupIndex, False )

            else
                ( maybeNextIndex, x.groupIndex == index )
        )
        ( Nothing, False )
        interactiveHighlightables
        |> Tuple.first


previousInteractiveIndex : Int -> List (Highlightable marker) -> Maybe Int
previousInteractiveIndex index highlightables =
    let
        isInteractive highlightable =
            .type_ highlightable == Highlightable.Interactive

        interactiveHighlightables =
            List.filter isInteractive highlightables
    in
    List.foldr
        (\x ( maybeNextIndex, hasIndexMatched ) ->
            if hasIndexMatched then
                ( Just x.groupIndex, False )

            else
                ( maybeNextIndex, x.groupIndex == index )
        )
        ( Nothing, False )
        interactiveHighlightables
        |> Tuple.first


keyboardEventToActions : KeyboardMsg -> Model marker -> List (Action marker)
keyboardEventToActions msg model =
    case msg of
        MoveLeft index ->
            case previousInteractiveIndex index model.highlightables of
                Nothing ->
                    []

                Just i ->
                    [ Focus i ]

        MoveRight index ->
            case nextInteractiveIndex index model.highlightables of
                Nothing ->
                    []

                Just i ->
                    [ Focus i ]

        SelectionExpandLeft index ->
            case previousInteractiveIndex index model.highlightables of
                Nothing ->
                    []

                Just i ->
                    Focus i
                        :: (case model.selectionStartIndex of
                                Just startIndex ->
                                    [ ExpandSelection i, Hint startIndex i ]

                                Nothing ->
                                    [ StartSelection index, ExpandSelection i, Hint index i ]
                           )

        SelectionExpandRight index ->
            case nextInteractiveIndex index model.highlightables of
                Nothing ->
                    []

                Just i ->
                    Focus i
                        :: (case model.selectionStartIndex of
                                Just startIndex ->
                                    [ ExpandSelection i, Hint startIndex i ]

                                Nothing ->
                                    [ StartSelection index, ExpandSelection i, Hint index i ]
                           )

        SelectionApplyTool index ->
            case model.marker of
                Tool.Marker marker ->
                    [ Save marker, ResetSelection, Focus index ]

                Tool.Eraser _ ->
                    [ Remove, ResetSelection, Focus index ]

        SelectionReset index ->
            [ ResetSelection, Remove, Focus index ]

        ToggleHighlight index ->
            case model.marker of
                Tool.Marker marker ->
                    [ Toggle index marker
                    , Focus index
                    ]

                Tool.Eraser _ ->
                    [ MouseOver index
                    , Hint index index
                    , MouseUp
                    , Remove
                    , Focus index
                    ]


{-| Pointer events to actions.
-}
pointerEventToActions : PointerMsg -> Model marker -> List (Action marker)
pointerEventToActions msg model =
    case msg of
        Ignored ->
            []

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
                save marker =
                    case ( model.mouseOverIndex, model.mouseDownIndex ) of
                        ( Just overIndex, Just downIndex ) ->
                            if overIndex == downIndex then
                                [ Toggle downIndex marker ]

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
performActions : Model marker -> List (Action marker) -> ( Model marker, Cmd (Msg m) )
performActions model actions =
    List.foldl performAction ( model, [] ) actions
        |> Tuple.mapSecond Cmd.batch


{-| Performs actual changes to the model, or emit a command.
-}
performAction : Action marker -> ( Model marker, List (Cmd (Msg m)) ) -> ( Model marker, List (Cmd (Msg m)) )
performAction action ( model, cmds ) =
    case action of
        Focus index ->
            ( { model | focusIndex = Just index }, Task.attempt Focused (Dom.focus (highlightableId model.id index)) :: cmds )

        Blur index ->
            ( { model | highlightables = Internal.blurAt index model.highlightables }, cmds )

        Hover index ->
            ( { model | highlightables = Internal.hoverAt index model.highlightables }, cmds )

        Hint start end ->
            ( { model | highlightables = Internal.hintBetween start end model.highlightables }, cmds )

        Save marker ->
            ( { model
                | highlightables = Internal.saveHinted marker model.highlightables
                , hasChanged = Changed
              }
            , cmds
            )

        Toggle index marker ->
            ( { model
                | highlightables = Internal.toggleHinted index marker model.highlightables
                , hasChanged = Changed
              }
            , cmds
            )

        Remove ->
            ( { model
                | highlightables = Internal.removeHinted model.highlightables
                , hasChanged = Changed
              }
            , cmds
            )

        MouseDown index ->
            ( { model | mouseDownIndex = Just index }, cmds )

        MouseOver index ->
            ( { model | mouseOverIndex = Just index }, cmds )

        MouseUp ->
            ( { model | mouseDownIndex = Nothing }, cmds )

        StartSelection index ->
            ( { model | selectionStartIndex = Just index }, cmds )

        ExpandSelection index ->
            ( { model | selectionEndIndex = Just index }, cmds )

        ResetSelection ->
            ( { model | selectionStartIndex = Nothing, selectionEndIndex = Nothing }, cmds )


{-| -}
removeHighlights : Model marker -> Model marker
removeHighlights model =
    { model | highlightables = Internal.removeHighlights model.highlightables }



-- VIEWS


{-| -}
view : { config | id : String, highlightables : List (Highlightable marker), focusIndex : Maybe Int, marker : Tool.Tool marker } -> Html (Msg marker)
view config =
    view_ { showTagsInline = False, maybeTool = Just config.marker }
        (viewHighlightable config.id config.marker config.focusIndex)
        config


{-| -}
static : { config | id : String, highlightables : List (Highlightable marker) } -> Html msg
static config =
    view_ { showTagsInline = False, maybeTool = Nothing }
        viewStaticHighlightable
        config


viewStaticHighlightable : Highlightable marker -> List Css.Style -> Html msg
viewStaticHighlightable =
    viewHighlightableSegment
        { interactiveHighlighterId = Nothing
        , focusIndex = Nothing
        , eventListeners = []
        , maybeTool = Nothing
        }


{-| -}
staticWithTags : { config | id : String, highlightables : List (Highlightable marker) } -> Html msg
staticWithTags config =
    let
        viewStaticHighlightableWithTags : Highlightable marker -> List Css.Style -> Html msg
        viewStaticHighlightableWithTags =
            viewHighlightableSegment
                { interactiveHighlighterId = Nothing
                , focusIndex = Nothing
                , eventListeners = []
                , maybeTool = Nothing
                }
    in
    view_ { showTagsInline = True, maybeTool = Nothing }
        viewStaticHighlightableWithTags
        config


view_ :
    { showTagsInline : Bool, maybeTool : Maybe (Tool.Tool marker) }
    -> (Highlightable marker -> List Css.Style -> Html msg)
    -> { config | id : String, highlightables : List (Highlightable marker) }
    -> Html msg
view_ groupConfig viewSegment { id, highlightables } =
    p [ Html.Styled.Attributes.id id, class "highlighter-container" ]
        (viewSegments groupConfig viewSegment highlightables)


viewSegments :
    { showTagsInline : Bool, maybeTool : Maybe (Tool.Tool marker) }
    -> (Highlightable marker -> List Css.Style -> Html msg)
    -> List (Highlightable marker)
    -> List (Html msg)
viewSegments groupConfig viewSegment highlightables =
    highlightables
        |> Grouping.buildGroups
        |> List.concatMap (groupContainer groupConfig viewSegment)


{-| When elements are marked, wrap them in a single `mark` html node.
-}
groupContainer :
    { showTagsInline : Bool
    , maybeTool : Maybe (Tool.Tool marker)
    }
    -> (Highlightable marker -> List Css.Style -> Html msg)
    -> List (Highlightable marker)
    -> List (Html msg)
groupContainer config viewSegment highlightables =
    let
        toMark : Highlightable marker -> Tool.MarkerModel marker -> Mark.Mark
        toMark highlightable marker =
            { name = marker.name
            , startStyles = marker.startGroupClass
            , styles = highlightableStyle config.maybeTool highlightable
            , endStyles = marker.endGroupClass
            }

        viewMark =
            if config.showTagsInline then
                Mark.viewWithInlineTags

            else
                Mark.view
    in
    highlightables
        |> List.map (\highlightable -> ( highlightable, Maybe.map (toMark highlightable) highlightable.marked ))
        |> viewMark viewSegment


viewHighlightable : String -> Tool.Tool marker -> Maybe Int -> Highlightable marker -> List Css.Style -> Html (Msg marker)
viewHighlightable highlighterId marker focusIndex highlightable =
    case highlightable.type_ of
        Highlightable.Interactive ->
            viewHighlightableSegment
                { interactiveHighlighterId = Just highlighterId
                , focusIndex = focusIndex
                , eventListeners =
                    [ onPreventDefault "mouseover" (Pointer <| Over highlightable.groupIndex)
                    , onPreventDefault "mouseleave" (Pointer <| Out highlightable.groupIndex)
                    , onPreventDefault "mouseup" (Pointer <| Up Nothing)
                    , onPreventDefault "mousedown" (Pointer <| Down highlightable.groupIndex)
                    , onPreventDefault "touchstart" (Pointer <| Down highlightable.groupIndex)
                    , attribute "data-interactive" ""
                    , Key.onKeyDownPreventDefault
                        [ Key.space (Keyboard <| ToggleHighlight highlightable.groupIndex)
                        , Key.right (Keyboard <| MoveRight highlightable.groupIndex)
                        , Key.left (Keyboard <| MoveLeft highlightable.groupIndex)
                        , Key.shiftRight (Keyboard <| SelectionExpandRight highlightable.groupIndex)
                        , Key.shiftLeft (Keyboard <| SelectionExpandLeft highlightable.groupIndex)
                        ]
                    , Key.onKeyUpPreventDefault
                        [ Key.shiftRight (Keyboard <| SelectionApplyTool highlightable.groupIndex)
                        , Key.shiftLeft (Keyboard <| SelectionApplyTool highlightable.groupIndex)
                        , Key.shift (Keyboard <| SelectionReset highlightable.groupIndex)
                        ]
                    ]
                , maybeTool = Just marker
                }
                highlightable

        Highlightable.Static ->
            viewHighlightableSegment
                { interactiveHighlighterId = Nothing
                , focusIndex = focusIndex
                , eventListeners =
                    -- Static highlightables need listeners as well.
                    -- because otherwise we miss mouseup events
                    [ onPreventDefault "mouseup" (Pointer <| Up Nothing)
                    , onPreventDefault "mousedown" (Pointer <| Down highlightable.groupIndex)
                    , onPreventDefault "touchstart" (Pointer <| Down highlightable.groupIndex)
                    , attribute "data-static" ""
                    ]
                , maybeTool = Just marker
                }
                highlightable


viewHighlightableSegment :
    { interactiveHighlighterId : Maybe String
    , focusIndex : Maybe Int
    , eventListeners : List (Attribute msg)
    , maybeTool : Maybe (Tool.Tool marker)
    }
    -> Highlightable marker
    -> List Css.Style
    -> Html msg
viewHighlightableSegment { interactiveHighlighterId, focusIndex, eventListeners, maybeTool } highlightable markStyles =
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

        isInteractive =
            interactiveHighlighterId /= Nothing
    in
    span
        (eventListeners
            ++ customToHtmlAttributes highlightable.customAttributes
            ++ whitespaceClass highlightable.text
            ++ [ attribute "data-highlighter-item-index" <| String.fromInt highlightable.groupIndex
               , case interactiveHighlighterId of
                    Just highlighterId ->
                        Html.Styled.Attributes.id (highlightableId highlighterId highlightable.groupIndex)

                    Nothing ->
                        AttributesExtra.none
               , css
                    (Css.focus [ Css.zIndex (Css.int 1), Css.position Css.relative ]
                        :: highlightableStyle maybeTool highlightable
                        ++ markStyles
                    )
               , class "highlighter-highlightable"
               , case highlightable.marked of
                    Just markedWith ->
                        class "highlighter-highlighted"

                    _ ->
                        AttributesExtra.none
               , if isInteractive then
                    Key.tabbable
                        (case focusIndex of
                            Nothing ->
                                False

                            Just i ->
                                highlightable.groupIndex == i
                        )

                 else
                    AttributesExtra.none
               ]
        )
        [ Html.text highlightable.text ]


highlightableId : String -> Int -> String
highlightableId highlighterId groupIndex =
    "highlighter-" ++ highlighterId ++ "-highlightable-" ++ String.fromInt groupIndex


highlightableStyle : Maybe (Tool.Tool kind) -> Highlightable kind -> List Css.Style
highlightableStyle tool ({ uiState, marked } as highlightable) =
    case tool of
        Nothing ->
            [ case marked of
                Just markedWith ->
                    Css.batch markedWith.highlightClass

                Nothing ->
                    Css.backgroundColor Css.transparent
            ]

        Just (Tool.Marker marker) ->
            [ Css.property "user-select" "none"
            , case ( uiState, marked ) of
                ( Highlightable.Hovered, Just markedWith ) ->
                    -- Override marking with selected tool
                    Css.batch marker.hoverHighlightClass

                ( Highlightable.Hovered, Nothing ) ->
                    [ marker.hoverClass
                    , marker.startGroupClass
                    , marker.endGroupClass
                    ]
                        |> List.concat
                        |> Css.batch

                ( Highlightable.Hinted, _ ) ->
                    Css.batch marker.hintClass

                ( Highlightable.None, Just markedWith ) ->
                    Css.batch markedWith.highlightClass

                ( Highlightable.None, Nothing ) ->
                    Css.backgroundColor Css.transparent
            ]

        Just (Tool.Eraser eraser_) ->
            case marked of
                Just markedWith ->
                    [ Css.property "user-select" "none"
                    , Css.batch markedWith.highlightClass
                    , Css.batch
                        (case uiState of
                            Highlightable.Hinted ->
                                eraser_.hintClass

                            Highlightable.Hovered ->
                                eraser_.hoverClass

                            _ ->
                                []
                        )
                    ]

                Nothing ->
                    [ Css.property "user-select" "none", Css.backgroundColor Css.transparent ]


{-| Helper for `on` to preventDefault.
-}
onPreventDefault : String -> msg -> Attribute msg
onPreventDefault name msg =
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
    Events.preventDefaultOn name
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
