module Nri.Ui.Highlighter.V4 exposing
    ( Model, Msg(..), PointerMsg(..)
    , init, update
    , view, static, staticWithTags
    , viewMarkdown, staticMarkdown, staticMarkdownWithTags
    , viewWithOverlappingHighlights, staticWithOverlappingHighlights
    , Intent(..), hasChanged, HasChanged(..)
    , removeHighlights
    , clickedHighlightable, hoveredHighlightable
    )

{-| Changes from V3:

  - Highlighter.update now takes a sorter for the markers

Highlighter provides a view/model/update to display a view to highlight text and show marks.


# Types

@docs Model, Msg, PointerMsg


# Init/View/Update

@docs init, update

@docs view, static, staticWithTags
@docs viewMarkdown, staticMarkdown, staticMarkdownWithTags
@docs viewWithOverlappingHighlights, staticWithOverlappingHighlights


## Intents

@docs Intent, hasChanged, HasChanged


## Setters

@docs removeHighlights


## Getters

@docs clickedHighlightable, hoveredHighlightable

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
import Nri.Ui.Highlightable.V3 as Highlightable exposing (Highlightable)
import Nri.Ui.HighlighterTool.V1 as Tool
import Nri.Ui.Html.Attributes.V2 as AttributesExtra
import Nri.Ui.Mark.V2 as Mark exposing (Mark)
import Set exposing (Set)
import Sort exposing (Sorter)
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
    , hoveringIndex : Maybe Int
    , hintingIndices : Maybe ( Int, Int )
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

    -- Internal state to track user's interactions
    , hoveringIndex = Nothing
    , hintingIndices = Nothing
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
update : Sorter marker -> Msg marker -> Model marker -> ( Model marker, Cmd (Msg marker), Intent )
update sorter msg model =
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
            ( { model | focusIndex = Just index }
            , Task.attempt Focused (Dom.focus (highlightableId model.id index)) :: cmds
            )

        Blur index ->
            ( { model | hoveringIndex = Nothing }, cmds )

        Hover index ->
            ( { model | hoveringIndex = Just index }, cmds )

        Hint start end ->
            ( { model | hintingIndices = Just ( start, end ) }, cmds )

        Save marker ->
            case model.hintingIndices of
                Just hinting ->
                    ( { model
                        | highlightables = saveHinted marker hinting model.highlightables
                        , hasChanged = Changed
                      }
                    , cmds
                    )

                Nothing ->
                    ( model, cmds )

        Toggle index marker ->
            ( { model
                | highlightables = toggleHinted index marker model.highlightables
                , hasChanged = Changed
                , hintingIndices = Nothing
              }
            , cmds
            )

        RemoveHint ->
            case model.hintingIndices of
                Just hinting ->
                    ( { model
                        | highlightables = removeHinted hinting model.highlightables
                        , hasChanged = Changed
                        , hintingIndices = Nothing
                      }
                    , cmds
                    )

                Nothing ->
                    ( model, cmds )

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


between : Int -> Int -> Highlightable marker -> Bool
between from to { index } =
    if from < to then
        from <= index && index <= to

    else
        to <= index && index <= from


saveHinted : Tool.MarkerModel marker -> ( Int, Int ) -> List (Highlightable marker) -> List (Highlightable marker)
saveHinted marker ( hintBeginning, hintEnd ) =
    List.map
        (\highlightable ->
            if between hintBeginning hintEnd highlightable then
                Highlightable.set (Just marker) highlightable

            else
                highlightable
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
    List.map toggle highlightables


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


removeHinted : ( Int, Int ) -> List (Highlightable marker) -> List (Highlightable marker)
removeHinted ( hintBeginning, hintEnd ) =
    List.map
        (\highlightable ->
            if between hintBeginning hintEnd highlightable then
                Highlightable.set Nothing highlightable

            else
                highlightable
        )


{-| -}
removeHighlights : Model marker -> Model marker
removeHighlights model =
    { model | highlightables = List.map (Highlightable.set Nothing) model.highlightables }


{-| You are not likely to need this helper unless you're working with inline commenting.
-}
clickedHighlightable : Model marker -> Maybe (Highlightable.Highlightable marker)
clickedHighlightable model =
    Maybe.andThen (\i -> Highlightable.byId i model.highlightables) model.mouseDownIndex


{-| You are not likely to need this helper unless you're working with inline commenting.
-}
hoveredHighlightable : Model marker -> Maybe (Highlightable.Highlightable marker)
hoveredHighlightable model =
    Maybe.andThen (\i -> Highlightable.byId i model.highlightables) model.mouseOverIndex



-- VIEWS


{-| -}
view : Model marker -> Html (Msg marker)
view model =
    view_
        { showTagsInline = False
        , maybeTool = Just model.marker
        , hoveringIndex = model.hoveringIndex
        , hintingIndices = model.hintingIndices
        , overlaps = False
        , viewSegment = viewHighlightable False model
        , id = model.id
        , highlightables = model.highlightables
        }


{-| -}
viewWithOverlappingHighlights : Model marker -> Html (Msg marker)
viewWithOverlappingHighlights model =
    view_
        { showTagsInline = False
        , maybeTool = Just model.marker
        , hoveringIndex = model.hoveringIndex
        , hintingIndices = model.hintingIndices
        , overlaps = True
        , viewSegment = viewHighlightable False model
        , id = model.id
        , highlightables = model.highlightables
        }


{-| -}
staticWithOverlappingHighlights : { config | id : String, highlightables : List (Highlightable marker) } -> Html msg
staticWithOverlappingHighlights config =
    view_
        { showTagsInline = False
        , maybeTool = Nothing
        , hoveringIndex = Nothing
        , hintingIndices = Nothing
        , overlaps = True
        , viewSegment =
            viewHighlightableSegment
                { interactiveHighlighterId = Nothing
                , focusIndex = Nothing
                , eventListeners = []
                , maybeTool = Nothing
                , hoveringIndex = Nothing
                , hintingIndices = Nothing
                , renderMarkdown = False
                }
        , id = config.id
        , highlightables = config.highlightables
        }


{-| Same as `view`, but will render strings like "_blah_" inside of emphasis tags.

WARNING: the version of markdown used here is extremely limited, as the highlighter content needs to be entirely in-line content. Lists & other block-level elements will _not_ render as they usually would!

WARNING: markdown is rendered highlightable by highlightable, so be sure to provide highlightables like ["_New York Times_"]["*New York Times*"], NOT like ["_New ", "York ", "Times_"]["*New ", "York ", "Times*"]

-}
viewMarkdown : Model marker -> Html (Msg marker)
viewMarkdown model =
    view_
        { showTagsInline = False
        , maybeTool = Just model.marker
        , hoveringIndex = model.hoveringIndex
        , hintingIndices = model.hintingIndices
        , overlaps = False
        , viewSegment = viewHighlightable True model
        , id = model.id
        , highlightables = model.highlightables
        }


{-| -}
static : { config | id : String, highlightables : List (Highlightable marker) } -> Html msg
static config =
    view_
        { showTagsInline = False
        , maybeTool = Nothing
        , hoveringIndex = Nothing
        , hintingIndices = Nothing
        , overlaps = False
        , viewSegment =
            viewHighlightableSegment
                { interactiveHighlighterId = Nothing
                , focusIndex = Nothing
                , eventListeners = []
                , maybeTool = Nothing
                , hoveringIndex = Nothing
                , hintingIndices = Nothing
                , renderMarkdown = False
                }
        , id = config.id
        , highlightables = config.highlightables
        }


{-| Same as `static`, but will render strings like "_blah_" inside of emphasis tags.

WARNING: the version of markdown used here is extremely limited, as the highlighter content needs to be entirely in-line content. Lists & other block-level elements will _not_ render as they usually would!

WARNING: markdown is rendered highlightable by highlightable, so be sure to provide highlightables like ["_New York Times_"]["*New York Times*"], NOT like ["_New ", "York ", "Times_"]["*New ", "York ", "Times*"]

-}
staticMarkdown : { config | id : String, highlightables : List (Highlightable marker) } -> Html msg
staticMarkdown config =
    view_
        { showTagsInline = False
        , maybeTool = Nothing
        , hoveringIndex = Nothing
        , hintingIndices = Nothing
        , overlaps = False
        , viewSegment =
            viewHighlightableSegment
                { interactiveHighlighterId = Nothing
                , focusIndex = Nothing
                , eventListeners = []
                , maybeTool = Nothing
                , hoveringIndex = Nothing
                , hintingIndices = Nothing
                , renderMarkdown = True
                }
        , id = config.id
        , highlightables = config.highlightables
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
                , hoveringIndex = Nothing
                , hintingIndices = Nothing
                , renderMarkdown = False
                }
    in
    view_
        { showTagsInline = True
        , maybeTool = Nothing
        , hoveringIndex = Nothing
        , hintingIndices = Nothing
        , overlaps = False
        , viewSegment = viewStaticHighlightableWithTags
        , id = config.id
        , highlightables = config.highlightables
        }


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
                , hoveringIndex = Nothing
                , hintingIndices = Nothing
                , renderMarkdown = True
                }
    in
    view_
        { showTagsInline = True
        , maybeTool = Nothing
        , hoveringIndex = Nothing
        , hintingIndices = Nothing
        , overlaps = False
        , viewSegment = viewStaticHighlightableWithTags
        , id = config.id
        , highlightables = config.highlightables
        }


{-| Groups highlightables with the same state together.
-}
buildGroups :
    { model
        | hintingIndices : Maybe ( Int, Int )
        , hoveringIndex : Maybe Int
    }
    -> List (Highlightable marker)
    -> List (List (Highlightable marker))
buildGroups model =
    List.Extra.groupWhile (groupHighlightables model)
        >> List.map (\( elem, list ) -> elem :: list)


groupHighlightables :
    { model
        | hintingIndices : Maybe ( Int, Int )
        , hoveringIndex : Maybe Int
    }
    -> Highlightable marker
    -> Highlightable marker
    -> Bool
groupHighlightables { hintingIndices, hoveringIndex } x y =
    let
        xIsHinted =
            -- x is Hinted
            Maybe.map (\( a, b ) -> between a b x) hintingIndices
                |> Maybe.withDefault False

        yIsHinted =
            -- y is Hinted
            Maybe.map (\( a, b ) -> between a b y) hintingIndices
                |> Maybe.withDefault False

        xIsHovered =
            hoveringIndex == Just x.index

        yIsHovered =
            hoveringIndex == Just y.index

        xAndYHaveTheSameState =
            -- Both are hinted
            (xIsHinted && yIsHinted)
                || -- Neither is hinted
                   (not xIsHinted && not yIsHinted)
                || -- Neither is hovered
                   (not xIsHovered && not yIsHovered)
    in
    (xAndYHaveTheSameState
        && (List.head x.marked == Nothing)
        && (List.head y.marked == Nothing)
    )
        || (List.head x.marked == List.head y.marked && List.head x.marked /= Nothing)
        || ((List.head x.marked /= Nothing) && yIsHinted)
        || ((List.head y.marked /= Nothing) && xIsHinted)


{-| When elements are marked and the view doesn't support overlaps, wrap the marked elements in a single `mark` html node.
-}
view_ :
    { showTagsInline : Bool
    , maybeTool : Maybe (Tool.Tool marker)
    , hoveringIndex : Maybe Int
    , hintingIndices : Maybe ( Int, Int )
    , overlaps : Bool
    , viewSegment : Highlightable marker -> List Css.Style -> Html msg
    , highlightables : List (Highlightable marker)
    , id : String
    }
    -> Html msg
view_ config =
    let
        toMark : Highlightable marker -> Tool.MarkerModel marker -> Mark.Mark
        toMark highlightable marker =
            { name = marker.name
            , startStyles = marker.startGroupClass
            , styles = highlightableStyle config highlightable
            , endStyles = marker.endGroupClass
            }

        withoutOverlaps : List (List ( Highlightable marker, Maybe Mark ))
        withoutOverlaps =
            config.highlightables
                |> buildGroups config
                |> List.map
                    (\group ->
                        List.map
                            (\highlightable ->
                                ( highlightable
                                , Maybe.map (toMark highlightable) (List.head highlightable.marked)
                                )
                            )
                            group
                    )

        withOverlaps : List ( Highlightable marker, List Mark )
        withOverlaps =
            List.map
                (\highlightable ->
                    ( highlightable
                    , List.map (toMark highlightable) highlightable.marked
                    )
                )
                config.highlightables
    in
    p [ Html.Styled.Attributes.id config.id, class "highlighter-container" ] <|
        if config.showTagsInline then
            List.concatMap (Mark.viewWithInlineTags config.viewSegment) withoutOverlaps

        else if config.overlaps then
            Mark.viewWithOverlaps config.viewSegment withOverlaps

        else
            List.concatMap (Mark.view config.viewSegment) withoutOverlaps


viewHighlightable :
    Bool
    ->
        { config
            | id : String
            , focusIndex : Maybe Int
            , marker : Tool.Tool marker
            , hoveringIndex : Maybe Int
            , hintingIndices : Maybe ( Int, Int )
        }
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
                , renderMarkdown = renderMarkdown
                , maybeTool = Just config.marker
                , hoveringIndex = config.hoveringIndex
                , hintingIndices = config.hintingIndices
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
                , renderMarkdown = renderMarkdown
                , maybeTool = Just config.marker
                , hoveringIndex = config.hoveringIndex
                , hintingIndices = config.hintingIndices
                }
                highlightable


viewHighlightableSegment :
    { interactiveHighlighterId : Maybe String
    , focusIndex : Maybe Int
    , eventListeners : List (Attribute msg)
    , maybeTool : Maybe (Tool.Tool marker)
    , hoveringIndex : Maybe Int
    , hintingIndices : Maybe ( Int, Int )
    , renderMarkdown : Bool
    }
    -> Highlightable marker
    -> List Css.Style
    -> Html msg
viewHighlightableSegment ({ interactiveHighlighterId, focusIndex, eventListeners, renderMarkdown } as config) highlightable markStyles =
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
            case txt of
                "\t" ->
                    [ class "highlighter-whitespace-tab" ]

                " " ->
                    [ class "highlighter-whitespace-single-space" ]

                "\n" ->
                    [ class "highlighter-whitespace-newline" ]

                _ ->
                    []

        isInteractive =
            interactiveHighlighterId /= Nothing
    in
    span
        (eventListeners
            ++ List.map (Html.Styled.Attributes.map never) highlightable.customAttributes
            ++ whitespaceClass highlightable.text
            ++ [ attribute "data-highlighter-item-index" <| String.fromInt highlightable.index
               , case interactiveHighlighterId of
                    Just highlighterId ->
                        Html.Styled.Attributes.id (highlightableId highlighterId highlightable.index)

                    Nothing ->
                        AttributesExtra.none
               , css
                    (Css.focus [ Css.zIndex (Css.int 1), Css.position Css.relative ]
                        :: highlightableStyle config highlightable
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


highlightableStyle :
    { config
        | maybeTool : Maybe (Tool.Tool marker)
        , hoveringIndex : Maybe Int
        , hintingIndices : Maybe ( Int, Int )
    }
    -> Highlightable kind
    -> List Css.Style
highlightableStyle { maybeTool, hoveringIndex, hintingIndices } ({ marked } as highlightable) =
    let
        isHinted =
            hintingIndices
                |> Maybe.map (\( a, b ) -> between a b highlightable)
                |> Maybe.withDefault False
    in
    case maybeTool of
        Nothing ->
            [ case List.head marked of
                Just markedWith ->
                    Css.batch markedWith.highlightClass

                Nothing ->
                    Css.backgroundColor Css.transparent
            ]

        Just (Tool.Marker marker) ->
            [ Css.property "user-select" "none"
            , case List.head marked of
                Just markedWith ->
                    if isHinted then
                        Css.batch marker.hintClass

                    else if hoveringIndex == Just highlightable.index then
                        -- Override marking with selected tool
                        Css.batch marker.hoverHighlightClass

                    else
                        -- otherwise, show the standard mark styles
                        Css.batch markedWith.highlightClass

                Nothing ->
                    if isHinted then
                        Css.batch marker.hintClass

                    else if hoveringIndex == Just highlightable.index then
                        -- When Hovered but not marked
                        [ marker.hoverClass
                        , marker.startGroupClass
                        , marker.endGroupClass
                        ]
                            |> List.concat
                            |> Css.batch

                    else
                        Css.backgroundColor Css.transparent
            ]

        Just (Tool.Eraser eraser_) ->
            case List.head marked of
                Just markedWith ->
                    [ Css.property "user-select" "none"
                    , Css.batch markedWith.highlightClass
                    , Css.batch
                        (if isHinted then
                            eraser_.hintClass

                         else if hoveringIndex == Just highlightable.index then
                            eraser_.hoverClass

                         else
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
