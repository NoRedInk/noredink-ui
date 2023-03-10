module Nri.Ui.Highlighter.V3 exposing
    ( Model, Msg(..), PointerMsg(..)
    , init, update
    , view, static, staticWithTags
    , viewMarkdown, staticMarkdown, staticMarkdownWithTags
    , viewWithOverlappingHighlights
    , Intent(..), hasChanged, HasChanged(..)
    , removeHighlights
    )

{-| Changes from V2:

  - support overlapping highlights (by way of removing the underlying mark element)
  - move asFragmentTuples, usedMarkers, and text to the Highlightable module

Highlighter provides a view/model/update to display a view to highlight text and show marks.


# Types

@docs Model, Msg, PointerMsg


# Init/View/Update

@docs init, update

@docs view, static, staticWithTags
@docs viewMarkdown, staticMarkdown, staticMarkdownWithTags
@docs viewWithOverlappingHighlights


## Intents

@docs Intent, hasChanged, HasChanged


# Setters

@docs removeHighlights

-}

import Accessibility.Styled.Key as Key
import Browser.Dom as Dom
import Css
import Html.Styled as Html exposing (Attribute, Html, p, span)
import Html.Styled.Attributes exposing (attribute, class, css)
import Html.Styled.Events as Events
import Json.Decode
import List.Extra
import Markdown.Block
import Markdown.Inline
import Nri.Ui.Highlightable.V2 as Highlightable exposing (Highlightable)
import Nri.Ui.HighlighterTool.V1 as Tool
import Nri.Ui.Html.Attributes.V2 as AttributesExtra
import Nri.Ui.Mark.V2 as Mark exposing (Mark)
import Set exposing (Set)
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
    , joinAdjacentInteractiveHighlights : Bool

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

joinAdjacentInteractiveHighlights - When true, and static highlightables are sandwiched by highlighted interactive highlightables of the same type, apply the highlight to the static highlightable as well.

-}
init :
    { id : String
    , highlightables : List (Highlightable marker)
    , marker : Tool.Tool marker
    , joinAdjacentInteractiveHighlights : Bool
    }
    -> Model marker
init config =
    { id = config.id
    , highlightables =
        if config.joinAdjacentInteractiveHighlights then
            Highlightable.joinAdjacentInteractiveHighlights config.highlightables

        else
            config.highlightables
    , marker = config.marker
    , joinAdjacentInteractiveHighlights = config.joinAdjacentInteractiveHighlights
    , mouseDownIndex = Nothing
    , mouseOverIndex = Nothing
    , isInitialized = NotInitialized
    , hasChanged = NotChanged
    , selectionStartIndex = Nothing
    , selectionEndIndex = Nothing
    , focusIndex =
        List.Extra.findIndex (\highlightable -> .type_ highlightable == Highlightable.Interactive) config.highlightables
    }



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
    | RemoveHint
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
                    |> Tuple.mapFirst maybeJoinAdjacentInteractiveHighlights

            Keyboard keyboardMsg ->
                keyboardEventToActions keyboardMsg model
                    |> performActions model
                    |> Tuple.mapFirst maybeJoinAdjacentInteractiveHighlights

            Focused _ ->
                ( model, Cmd.none )


maybeJoinAdjacentInteractiveHighlights : Model m -> Model m
maybeJoinAdjacentInteractiveHighlights model =
    if model.joinAdjacentInteractiveHighlights then
        { model | highlightables = Highlightable.joinAdjacentInteractiveHighlights model.highlightables }

    else
        model


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
                ( Just x.index, False )

            else
                ( maybeNextIndex, x.index == index )
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
                ( Just x.index, False )

            else
                ( maybeNextIndex, x.index == index )
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
                    [ Focus i, ResetSelection, RemoveHint ]

        MoveRight index ->
            case nextInteractiveIndex index model.highlightables of
                Nothing ->
                    []

                Just i ->
                    [ Focus i, ResetSelection, RemoveHint ]

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
                    [ RemoveHint, ResetSelection, Focus index ]

        SelectionReset index ->
            [ ResetSelection, RemoveHint, Focus index ]

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
                    , RemoveHint
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
                    [ MouseUp, RemoveHint ]

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
            ( { model | highlightables = blurAt index model.highlightables }, cmds )

        Hover index ->
            ( { model | highlightables = hoverAt index model.highlightables }, cmds )

        Hint start end ->
            ( { model | highlightables = hintBetween start end model.highlightables }, cmds )

        Save marker ->
            ( { model
                | highlightables = saveHinted marker model.highlightables
                , hasChanged = Changed
              }
            , cmds
            )

        Toggle index marker ->
            ( { model
                | highlightables = toggleHinted index marker model.highlightables
                , hasChanged = Changed
              }
            , cmds
            )

        RemoveHint ->
            ( { model
                | highlightables = removeHinted model.highlightables
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


blurAt : Int -> List (Highlightable marker) -> List (Highlightable marker)
blurAt index =
    List.map
        (\highlightable ->
            if highlightable.index == index then
                Highlightable.blur highlightable

            else
                highlightable
        )


hoverAt : Int -> List (Highlightable marker) -> List (Highlightable marker)
hoverAt index =
    List.map
        (\highlightable ->
            if highlightable.index == index then
                Highlightable.hover highlightable

            else
                highlightable
        )


hintBetween : Int -> Int -> List (Highlightable marker) -> List (Highlightable marker)
hintBetween beginning end =
    List.map
        (\highlightable ->
            if between beginning end highlightable then
                Highlightable.hint highlightable

            else
                Highlightable.clearHint highlightable
        )


between : Int -> Int -> Highlightable marker -> Bool
between from to { index } =
    if from < to then
        from <= index && index <= to

    else
        to <= index && index <= from


saveHinted : Tool.MarkerModel marker -> List (Highlightable marker) -> List (Highlightable marker)
saveHinted marker =
    List.map
        (\highlightable ->
            case highlightable.uiState of
                Highlightable.Hinted ->
                    highlightable
                        |> Highlightable.set (Just marker)
                        |> Highlightable.clearHint

                _ ->
                    Highlightable.clearHint highlightable
        )


toggleHinted : Int -> Tool.MarkerModel marker -> List (Highlightable marker) -> List (Highlightable marker)
toggleHinted index marker highlightables =
    let
        hintedRange =
            inSameRange index highlightables

        inClickedRange highlightable =
            Set.member highlightable.index hintedRange

        toggle highlightable =
            if inClickedRange highlightable && Just marker == List.head highlightable.marked then
                Highlightable.set Nothing highlightable

            else if highlightable.index == index then
                Highlightable.set (Just marker) highlightable

            else
                highlightable
    in
    List.map (toggle >> Highlightable.clearHint) highlightables


{-| Finds the group indexes of the groups which are in the same highlighting as the group index
passed in the first argument.
-}
inSameRange : Int -> List (Highlightable marker) -> Set Int
inSameRange index highlightables =
    List.Extra.groupWhile (\a b -> a.marked == b.marked) highlightables
        |> List.map (\( first, rest ) -> first.index :: List.map .index rest)
        |> List.Extra.find (List.member index)
        |> Maybe.withDefault []
        |> Set.fromList


removeHinted : List (Highlightable marker) -> List (Highlightable marker)
removeHinted =
    List.map
        (\highlightable ->
            case highlightable.uiState of
                Highlightable.Hinted ->
                    highlightable
                        |> Highlightable.set Nothing
                        |> Highlightable.clearHint

                _ ->
                    Highlightable.clearHint highlightable
        )


{-| -}
removeHighlights : Model marker -> Model marker
removeHighlights model =
    { model | highlightables = List.map (Highlightable.set Nothing) model.highlightables }



-- VIEWS


{-| -}
view : { config | id : String, highlightables : List (Highlightable marker), focusIndex : Maybe Int, marker : Tool.Tool marker } -> Html (Msg marker)
view config =
    view_ { showTagsInline = False, maybeTool = Just config.marker, overlaps = False }
        (viewHighlightable False config)
        config


{-| -}
viewWithOverlappingHighlights : { config | id : String, highlightables : List (Highlightable marker), focusIndex : Maybe Int, marker : Tool.Tool marker } -> Html (Msg marker)
viewWithOverlappingHighlights config =
    view_ { showTagsInline = False, maybeTool = Just config.marker, overlaps = True }
        (viewHighlightable False config)
        config


{-| Same as `view`, but will render strings like "_blah_" inside of emphasis tags.

WARNING: the version of markdown used here is extremely limited, as the highlighter content needs to be entirely in-line content. Lists & other block-level elements will _not_ render as they usually would!

WARNING: markdown is rendered highlightable by highlightable, so be sure to provide highlightables like ["_New York Times_"]["*New York Times*"], NOT like ["_New ", "York ", "Times_"]["*New ", "York ", "Times*"]

-}
viewMarkdown : { config | id : String, highlightables : List (Highlightable marker), focusIndex : Maybe Int, marker : Tool.Tool marker } -> Html (Msg marker)
viewMarkdown config =
    view_ { showTagsInline = False, maybeTool = Just config.marker, overlaps = False }
        (viewHighlightable True config)
        config


{-| -}
static : { config | id : String, highlightables : List (Highlightable marker) } -> Html msg
static config =
    view_ { showTagsInline = False, maybeTool = Nothing, overlaps = False }
        (viewHighlightableSegment
            { interactiveHighlighterId = Nothing
            , focusIndex = Nothing
            , eventListeners = []
            , maybeTool = Nothing
            , renderMarkdown = False
            }
        )
        config


{-| Same as `static`, but will render strings like "_blah_" inside of emphasis tags.

WARNING: the version of markdown used here is extremely limited, as the highlighter content needs to be entirely in-line content. Lists & other block-level elements will _not_ render as they usually would!

WARNING: markdown is rendered highlightable by highlightable, so be sure to provide highlightables like ["_New York Times_"]["*New York Times*"], NOT like ["_New ", "York ", "Times_"]["*New ", "York ", "Times*"]

-}
staticMarkdown : { config | id : String, highlightables : List (Highlightable marker) } -> Html msg
staticMarkdown config =
    view_ { showTagsInline = False, maybeTool = Nothing, overlaps = False }
        (viewHighlightableSegment
            { interactiveHighlighterId = Nothing
            , focusIndex = Nothing
            , eventListeners = []
            , maybeTool = Nothing
            , renderMarkdown = True
            }
        )
        config


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
                , renderMarkdown = False
                }
    in
    view_ { showTagsInline = True, maybeTool = Nothing, overlaps = False }
        viewStaticHighlightableWithTags
        config


{-| Same as `staticWithTags`, but will render strings like "_blah_" inside of emphasis tags.

WARNING: the version of markdown used here is extremely limited, as the highlighter content needs to be entirely in-line content. Lists & other block-level elements will _not_ render as they usually would!

WARNING: markdown is rendered highlightable by highlightable, so be sure to provide highlightables like ["_New York Times_"]["*New York Times*"], NOT like ["_New ", "York ", "Times_"]["*New ", "York ", "Times*"]

-}
staticMarkdownWithTags : { config | id : String, highlightables : List (Highlightable marker) } -> Html msg
staticMarkdownWithTags config =
    let
        viewStaticHighlightableWithTags : Highlightable marker -> List Css.Style -> Html msg
        viewStaticHighlightableWithTags =
            viewHighlightableSegment
                { interactiveHighlighterId = Nothing
                , focusIndex = Nothing
                , eventListeners = []
                , maybeTool = Nothing
                , renderMarkdown = True
                }
    in
    view_ { showTagsInline = True, maybeTool = Nothing, overlaps = False }
        viewStaticHighlightableWithTags
        config


view_ :
    { showTagsInline : Bool, maybeTool : Maybe (Tool.Tool marker), overlaps : Bool }
    -> (Highlightable marker -> List Css.Style -> Html msg)
    -> { config | id : String, highlightables : List (Highlightable marker) }
    -> Html msg
view_ groupConfig viewSegment { id, highlightables } =
    p [ Html.Styled.Attributes.id id, class "highlighter-container" ]
        (viewSegments groupConfig viewSegment highlightables)


viewSegments :
    { showTagsInline : Bool, maybeTool : Maybe (Tool.Tool marker), overlaps : Bool }
    -> (Highlightable marker -> List Css.Style -> Html msg)
    -> List (Highlightable marker)
    -> List (Html msg)
viewSegments groupConfig viewSegment highlightables =
    highlightables
        |> buildGroups
        |> List.concatMap (groupContainer groupConfig viewSegment)


{-| Groups highlightables with the same state together.
-}
buildGroups : List (Highlightable marker) -> List (List (Highlightable marker))
buildGroups =
    List.Extra.groupWhile groupHighlightables
        >> List.map (\( elem, list ) -> elem :: list)


groupHighlightables : Highlightable marker -> Highlightable marker -> Bool
groupHighlightables x y =
    ((x.uiState == y.uiState)
        && (List.head x.marked == Nothing)
        && (List.head y.marked == Nothing)
    )
        || (List.head x.marked == List.head y.marked && List.head x.marked /= Nothing)
        || (List.head x.marked /= Nothing && y.uiState == Highlightable.Hinted)
        || (List.head y.marked /= Nothing && x.uiState == Highlightable.Hinted)


{-| When elements are marked, wrap them in a single `mark` html node.
-}
groupContainer :
    { showTagsInline : Bool
    , maybeTool : Maybe (Tool.Tool marker)
    , overlaps : Bool
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

        withoutOverlaps : List ( Highlightable marker, Maybe Mark )
        withoutOverlaps =
            List.map
                (\highlightable ->
                    ( highlightable
                    , Maybe.map (toMark highlightable) (List.head highlightable.marked)
                    )
                )
                highlightables

        withOverlaps : List ( Highlightable marker, List Mark )
        withOverlaps =
            List.map
                (\highlightable ->
                    ( highlightable
                    , List.map (toMark highlightable) highlightable.marked
                    )
                )
                highlightables
    in
    if config.showTagsInline then
        Mark.viewWithInlineTags viewSegment withoutOverlaps

    else if config.overlaps then
        Mark.viewWithOverlaps viewSegment withOverlaps

    else
        Mark.view viewSegment withoutOverlaps


viewHighlightable :
    Bool
    -> { config | id : String, focusIndex : Maybe Int, marker : Tool.Tool marker }
    -> Highlightable marker
    -> List Css.Style
    -> Html (Msg marker)
viewHighlightable renderMarkdown config highlightable =
    case highlightable.type_ of
        Highlightable.Interactive ->
            viewHighlightableSegment
                { interactiveHighlighterId = Just config.id
                , focusIndex = config.focusIndex
                , eventListeners =
                    [ onPreventDefault "mouseover" (Pointer <| Over highlightable.index)
                    , onPreventDefault "mouseleave" (Pointer <| Out highlightable.index)
                    , onPreventDefault "mouseup" (Pointer <| Up Nothing)
                    , onPreventDefault "mousedown" (Pointer <| Down highlightable.index)
                    , onPreventDefault "touchstart" (Pointer <| Down highlightable.index)
                    , attribute "data-interactive" ""
                    , Key.onKeyDownPreventDefault
                        [ Key.space (Keyboard <| ToggleHighlight highlightable.index)
                        , Key.right (Keyboard <| MoveRight highlightable.index)
                        , Key.left (Keyboard <| MoveLeft highlightable.index)
                        , Key.shiftRight (Keyboard <| SelectionExpandRight highlightable.index)
                        , Key.shiftLeft (Keyboard <| SelectionExpandLeft highlightable.index)
                        ]
                    , Key.onKeyUpPreventDefault
                        [ Key.shiftRight (Keyboard <| SelectionApplyTool highlightable.index)
                        , Key.shiftLeft (Keyboard <| SelectionApplyTool highlightable.index)
                        , Key.shift (Keyboard <| SelectionReset highlightable.index)
                        ]
                    ]
                , maybeTool = Just config.marker
                , renderMarkdown = renderMarkdown
                }
                highlightable

        Highlightable.Static ->
            viewHighlightableSegment
                { interactiveHighlighterId = Nothing
                , focusIndex = config.focusIndex
                , eventListeners =
                    -- Static highlightables need listeners as well.
                    -- because otherwise we miss mouseup events
                    [ onPreventDefault "mouseup" (Pointer <| Up Nothing)
                    , onPreventDefault "mousedown" (Pointer <| Down highlightable.index)
                    , onPreventDefault "touchstart" (Pointer <| Down highlightable.index)
                    , attribute "data-static" ""
                    ]
                , maybeTool = Just config.marker
                , renderMarkdown = renderMarkdown
                }
                highlightable


viewHighlightableSegment :
    { interactiveHighlighterId : Maybe String
    , focusIndex : Maybe Int
    , eventListeners : List (Attribute msg)
    , maybeTool : Maybe (Tool.Tool marker)
    , renderMarkdown : Bool
    }
    -> Highlightable marker
    -> List Css.Style
    -> Html msg
viewHighlightableSegment { interactiveHighlighterId, focusIndex, eventListeners, maybeTool, renderMarkdown } highlightable markStyles =
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
            ++ [ attribute "data-highlighter-item-index" <| String.fromInt highlightable.index
               , case interactiveHighlighterId of
                    Just highlighterId ->
                        Html.Styled.Attributes.id (highlightableId highlighterId highlightable.index)

                    Nothing ->
                        AttributesExtra.none
               , css
                    (Css.focus [ Css.zIndex (Css.int 1), Css.position Css.relative ]
                        :: highlightableStyle maybeTool highlightable
                        ++ markStyles
                    )
               , class "highlighter-highlightable"
               , case List.head highlightable.marked of
                    Just _ ->
                        class "highlighter-highlighted"

                    _ ->
                        AttributesExtra.none
               , if isInteractive then
                    Key.tabbable
                        (case focusIndex of
                            Nothing ->
                                False

                            Just i ->
                                highlightable.index == i
                        )

                 else
                    AttributesExtra.none
               ]
        )
        (if renderMarkdown then
            renderInlineMarkdown highlightable.text

         else
            [ Html.text highlightable.text ]
        )


renderInlineMarkdown : String -> List (Html msg)
renderInlineMarkdown text_ =
    let
        ( leftWhitespace, inner, rightWhitespace ) =
            String.foldr
                (\char ( l, i, r ) ->
                    if char == ' ' then
                        if i == "" then
                            ( l, i, String.cons char r )

                        else
                            ( String.cons char l, i, r )

                    else
                        ( "", String.cons char l ++ i, r )
                )
                ( "", "", "" )
                text_

        innerMarkdown =
            Markdown.Block.parse Nothing inner
                |> List.map
                    (Markdown.Block.walk
                        (inlinifyMarkdownBlock
                            >> Markdown.Block.PlainInlines
                        )
                    )
                |> List.concatMap Markdown.Block.toHtml
                |> List.map Html.fromUnstyled
    in
    Html.text leftWhitespace :: innerMarkdown ++ [ Html.text rightWhitespace ]


inlinifyMarkdownBlock : Markdown.Block.Block a b -> List (Markdown.Inline.Inline b)
inlinifyMarkdownBlock block =
    case block of
        Markdown.Block.BlankLine str ->
            [ Markdown.Inline.Text str ]

        Markdown.Block.ThematicBreak ->
            []

        Markdown.Block.Heading _ _ inlines ->
            inlines

        Markdown.Block.CodeBlock _ str ->
            [ Markdown.Inline.Text str ]

        Markdown.Block.Paragraph _ inlines ->
            inlines

        Markdown.Block.BlockQuote blocks ->
            List.concatMap inlinifyMarkdownBlock blocks

        Markdown.Block.List _ blocks ->
            List.concatMap inlinifyMarkdownBlock (List.concat blocks)

        Markdown.Block.PlainInlines inlines ->
            inlines

        Markdown.Block.Custom b blocks ->
            List.concatMap inlinifyMarkdownBlock blocks


highlightableId : String -> Int -> String
highlightableId highlighterId index =
    "highlighter-" ++ highlighterId ++ "-highlightable-" ++ String.fromInt index


highlightableStyle : Maybe (Tool.Tool kind) -> Highlightable kind -> List Css.Style
highlightableStyle tool ({ uiState, marked } as highlightable) =
    case tool of
        Nothing ->
            [ case List.head marked of
                Just markedWith ->
                    Css.batch markedWith.highlightClass

                Nothing ->
                    Css.backgroundColor Css.transparent
            ]

        Just (Tool.Marker marker) ->
            [ Css.property "user-select" "none"
            , case ( uiState, List.head marked ) of
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
            case List.head marked of
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
