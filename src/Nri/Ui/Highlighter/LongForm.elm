module Nri.Ui.Highlighter.LongForm exposing
    ( Model, Msg(..), PointerMsg(..), TouchMsg(..), KeyboardMsg(..)
    , init, update
    , view
    , viewWithOverlappingHighlights
    , FoldState, initFoldState, viewFoldHighlighter, viewFoldStatic
    , Intent(..), hasChanged, HasChanged(..), Change(..)
    , removeHighlights
    , clickedHighlightable, hoveredHighlightable
    , selectShortestMarkerRange
    )

{-| Changes from V5:

  - Added `scrollFriendly` flag to `init`, which enables a mode where:
      - Mobile highlighting requires long-press instead of tap to highlight
      - Desktop highlighting requires double-click instead of single-click to highlight
  - Made Intent return whether a highlight was created or removed, and what are its bounds
      - Makes it easier to change highlighter behavior from the outside
  - Segregated Touch events from Pointer events, so we can give them distinct behavior


# Types

@docs Model, Msg, PointerMsg, TouchMsg, KeyboardMsg


# Init/View/Update

@docs init, update

@docs view
@docs viewWithOverlappingHighlights


# Foldable Views

If you want more control over the rendering of the highlightables, you can use these functions inside of a fold to
render the highlightables one by one. Use `initFoldState` to set up the initial state and then each call to `viewFoldHighlighter`
or `viewFoldStatic` will render a single highlightable along with an update to the state.

@docs FoldState, initFoldState, viewFoldHighlighter, viewFoldStatic


## Intents

@docs Intent, hasChanged, HasChanged, Change


## Setters

@docs removeHighlights


## Getters

@docs clickedHighlightable, hoveredHighlightable
@docs selectShortestMarkerRange

-}

import Accessibility.Styled.Key as Key exposing (Event)
import Accessibility.Styled.Style exposing (invisibleStyle)
import Browser.Dom as Dom
import Css
import Css.Global
import Html as Unstyled
import Html.Attributes as UnstyledAttrs
import Html.Events as UnstyledEvents
import Html.Lazy as UnstyledLazy
import Html.Styled as Html exposing (Html)
import Json.Decode
import List.Extra
import Markdown.Block
import Markdown.Inline
import Nri.Ui.Highlightable.LongForm as Highlightable exposing (Highlightable)
import Nri.Ui.Highlighter.Attribute exposing (Attribute(..), toHtmlAttribute)
import Nri.Ui.HighlighterTool.V1 as Tool
import Nri.Ui.Html.Attributes.V2 as AttributesExtra
import Nri.Ui.Mark.LongForm as Mark exposing (Mark)
import Regex
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
    , markerRanges : List (MarkerRange marker)
    , joinAdjacentInteractiveHighlights : Bool
    , overlapsSupport : OverlapsSupport marker

    -- Scroll-friendly mode is used for highlighters in longform text, where we
    -- want to prevent accidental highlighting when scrolling, or when just plain
    -- reading the text.
    --
    -- In scroll-friendly mode, we highlight on double-click for desktop, and
    -- on long press for mobile.
    , scrollFriendly : Bool
    , sorter : Sorter marker

    -- Internal state to track user's interactions
    , hintingIndices : Maybe ( Int, Int )
    , mouseDownIndex : Maybe Int
    , mouseOverIndex : Maybe Int
    , isInitialized : Initialized
    , hasChanged : HasChanged marker
    , selectionStartIndex : Maybe Int
    , selectionEndIndex : Maybe Int
    , focusIndex : Maybe Int
    }


{-| Communicates whether a change to a highlightable has been performed
-}
type HasChanged marker
    = Changed (Change marker)
    | NotChanged


{-| Describes the change being performed by the Highlighter component, alongside the index bounds of the change
-}
type Change marker
    = HighlightCreated ( Int, Int ) marker
    | HighlightRemoved ( Int, Int ) marker


type Initialized
    = Initialized
    | NotInitialized


{-| Setup initial model

joinAdjacentInteractiveHighlights - When true, and static highlightables are sandwiched by highlighted interactive highlightables of the same type, apply the highlight to the static highlightable as well.

scrollFriendly - When true, enables a mode where mobile highlighting requires long-press instead of tap to highlight, and desktop highlighting requires double-click instead of single-click to highlight a single word.

-}
init :
    { id : String
    , highlightables : List (Highlightable marker)
    , marker : Tool.Tool marker
    , joinAdjacentInteractiveHighlights : Bool
    , sorter : Sorter marker
    , scrollFriendly : Bool
    , overlapsSupport : Bool
    }
    -> Model marker
init config =
    let
        focusIndex =
            List.Extra.findIndex (\highlightable -> .type_ highlightable == Highlightable.Interactive) config.highlightables
    in
    { id = config.id
    , highlightables =
        (if config.joinAdjacentInteractiveHighlights then
            Highlightable.joinAdjacentInteractiveHighlights config.sorter config.highlightables

         else
            config.highlightables
        )
            -- Enforce highlightable index to match index in list
            -- so we can use List.Extra.getAt, updateAt, etc
            |> List.indexedMap (\index highlightable -> { highlightable | index = index })
            |> List.Extra.updateAt (Maybe.withDefault 0 focusIndex) (\highlightable -> { highlightable | isFocused = True })
    , marker = config.marker
    , markerRanges = []
    , joinAdjacentInteractiveHighlights = config.joinAdjacentInteractiveHighlights
    , overlapsSupport =
        if config.overlapsSupport then
            OverlapsSupported { hoveredMarkerWithShortestHighlight = Nothing }

        else
            OverlapsNotSupported
    , scrollFriendly = config.scrollFriendly
    , sorter = config.sorter

    -- Internal state to track user's interactions
    , hintingIndices = Nothing
    , mouseDownIndex = Nothing
    , mouseOverIndex = Nothing
    , isInitialized = NotInitialized
    , hasChanged = NotChanged
    , selectionStartIndex = Nothing
    , selectionEndIndex = Nothing
    , focusIndex = focusIndex
    }



-- UPDATE


{-| -}
type Msg marker
    = Pointer PointerMsg
    | Touch TouchMsg
    | Keyboard KeyboardMsg
    | Focused (Result Dom.Error ())


{-| Messages used by highlighter when interacting with a mouse or finger.
-}
type PointerMsg
    = Down Int
    | Out
    | Over Int
    | Click { index : Int, clickCount : Int }
    | Up (Maybe String)
    | Ignored


{-| Messages used by highlighter when interacting with a touch screen.
-}
type TouchMsg
    = TouchStart Int
    | LongPress (Maybe String) Int
    | TouchMove (Maybe String) Int
    | TouchEnd (Maybe String)
    | TouchIgnored


{-| Messages used by highlighter when interaction with the keyboard.
-}
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
type Intent marker
    = Intent
        { listenTo : ListenTo
        , changed : HasChanged marker
        }


type alias ListenTo =
    Maybe String


{-| Get intent based on the resulting model from `update`.

  - This ensures that we initialize the highlighter in JS exactly once.
  - Sets the `hasChanged` flag if the model has changed. This is used by the user of `Highlighter` to
    determine whether they want to execute follow up actions.

-}
withIntent : ( Model m, Cmd (Msg m) ) -> ( Model m, Cmd (Msg m), Intent m )
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
hasChanged : Intent marker -> HasChanged marker
hasChanged (Intent { changed }) =
    changed


{-| Actions are used as an intermediate algebra from pointer events to actual changes to the model.
-}
type Action marker
    = Focus Int
    | Hint Int Int
    | MouseDown Int
    | MouseUp
    | MouseOver Int
    | MouseOut
    | RemoveHint
    | Save (Tool.MarkerModel marker)
    | Toggle Int (Tool.MarkerModel marker)
    | StartSelection Int
    | ExpandSelection Int
    | ResetSelection
    | None


type HighlightableUpdate
    = UpdateFocused
    | UpdateHinted


{-| Update for highlighter returning additional info about whether there was a change
-}
update : Msg marker -> Model marker -> ( Model marker, Cmd (Msg marker), Intent marker )
update msg model =
    withIntent <|
        case msg of
            Pointer pointerMsg ->
                pointerEventToActions pointerMsg model
                    |> performActions model
                    |> (\( newModel, updates, cmds ) -> ( updateHighlightables model newModel updates, cmds ))
                    |> Tuple.mapFirst maybeJoinAdjacentInteractiveHighlights

            Touch touchMsg ->
                touchEventToActions touchMsg model
                    |> performActions model
                    |> (\( newModel, updates, cmds ) -> ( updateHighlightables model newModel updates, cmds ))
                    |> Tuple.mapFirst maybeJoinAdjacentInteractiveHighlights

            Keyboard keyboardMsg ->
                keyboardEventToActions keyboardMsg model
                    |> performActions model
                    |> (\( newModel, updates, cmds ) -> ( updateHighlightables model newModel updates, cmds ))
                    |> Tuple.mapFirst maybeJoinAdjacentInteractiveHighlights

            Focused _ ->
                ( model, Cmd.none )


updateHighlightables : Model marker -> Model marker -> List HighlightableUpdate -> Model marker
updateHighlightables oldModel newModel updates =
    let
        uniqueUpdates =
            List.Extra.unique updates
    in
    cleanupHighlightables uniqueUpdates oldModel newModel
        |> updateHighlightableFlags uniqueUpdates oldModel


updateHighlightableFlags : List HighlightableUpdate -> Model marker -> Model marker -> Model marker
updateHighlightableFlags updates oldModel newModel =
    let
        updateIf updateKind f highlightable =
            if List.member updateKind updates then
                f oldModel newModel highlightable

            else
                highlightable
    in
    if List.isEmpty updates then
        newModel

    else
        { newModel
            | highlightables =
                List.map
                    (updateIf UpdateFocused updateFocused
                        >> updateIf UpdateHinted updateHinted
                    )
                    newModel.highlightables
        }


cleanupHighlightables : List HighlightableUpdate -> Model marker -> Model marker -> Model marker
cleanupHighlightables updates oldModel newModel =
    let
        cleanIf updateKind f highlightable =
            if List.member updateKind updates then
                f oldModel newModel highlightable

            else
                highlightable
    in
    if List.isEmpty updates then
        newModel

    else
        { newModel
            | highlightables =
                List.map
                    (cleanIf UpdateFocused cleanFocused
                        >> cleanIf UpdateHinted cleanHinted
                    )
                    newModel.highlightables
        }


maybeJoinAdjacentInteractiveHighlights : Model m -> Model m
maybeJoinAdjacentInteractiveHighlights model =
    if model.joinAdjacentInteractiveHighlights then
        { model | highlightables = Highlightable.joinAdjacentInteractiveHighlights model.sorter model.highlightables }

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

        Over eventIndex ->
            case model.mouseDownIndex of
                Just downIndex ->
                    [ MouseOver eventIndex
                    , Hint downIndex eventIndex
                    ]

                Nothing ->
                    [ MouseOver eventIndex ]

        Out ->
            [ MouseOut ]

        Down eventIndex ->
            [ MouseOver eventIndex
            , MouseDown eventIndex
            , if model.scrollFriendly then
                -- scroll-friendly mode only hints on double-click or drag
                None

              else
                Hint eventIndex eventIndex
            ]

        Click { index, clickCount } ->
            let
                scrollFriendlyDoubleClick =
                    -- In scroll-friendly mode, a double-click is necessary to create a highlightable
                    model.scrollFriendly && clickCount > 1

                scrollFriendlyFocusClick =
                    -- In scroll-friendly mode, a single click on a highlightable focuses on it
                    model.scrollFriendly
                        && (clickCount == 1)
                        && (Nothing /= markerAtIndex index model.highlightables)
            in
            if scrollFriendlyDoubleClick || scrollFriendlyFocusClick then
                case model.marker of
                    Tool.Marker marker ->
                        [ MouseOver index
                        , MouseDown index
                        , Hint index index
                        , Toggle index marker
                        , MouseUp
                        ]

                    Tool.Eraser _ ->
                        [ MouseOver index
                        , MouseDown index
                        , Hint index index
                        , RemoveHint
                        , MouseUp
                        ]

            else
                []

        Up _ ->
            let
                save marker =
                    case ( model.mouseOverIndex, model.mouseDownIndex ) of
                        ( Just overIndex, Just downIndex ) ->
                            if overIndex == downIndex then
                                if model.scrollFriendly && model.hintingIndices == Nothing then
                                    -- scroll-friendly mode only hints on double-click or drag
                                    []

                                else
                                    [ Toggle downIndex marker ]

                            else
                                -- Finished sentence highlighting over a highlightable
                                [ Save marker ]

                        ( Nothing, Just _ ) ->
                            -- Finished sentence highlighting outside of a highlightable
                            [ Save marker ]

                        _ ->
                            []
            in
            case model.marker of
                Tool.Marker marker ->
                    MouseUp :: save marker

                Tool.Eraser _ ->
                    [ MouseUp
                    , if model.scrollFriendly then
                        -- scroll-friendly mode only erases on double-click or drag
                        None

                      else
                        RemoveHint
                    ]


touchEventToActions : TouchMsg -> Model marker -> List (Action marker)
touchEventToActions msg model =
    case msg of
        TouchIgnored ->
            []

        TouchStart eventIndex ->
            [ MouseOver eventIndex
            , MouseDown eventIndex
            , Hint eventIndex eventIndex
            ]

        TouchMove targetId eventIndex ->
            if Just model.id == targetId then
                case model.mouseDownIndex of
                    Just downIndex ->
                        [ MouseOver eventIndex
                        , Hint downIndex eventIndex
                        ]

                    Nothing ->
                        []

            else
                []

        TouchEnd _ ->
            let
                save marker =
                    case ( model.mouseOverIndex, model.mouseDownIndex ) of
                        ( Just overIndex, Just downIndex ) ->
                            if overIndex == downIndex then
                                if model.scrollFriendly && model.hintingIndices == Nothing then
                                    -- scroll-friendly mode only hints on long-press
                                    []

                                else
                                    [ Toggle downIndex marker ]

                            else
                                -- Finished sentence highlighting over a highlightable
                                [ Save marker ]

                        ( Nothing, Just _ ) ->
                            -- Finished sentence highlighting outside of a highlightable
                            [ Save marker ]

                        _ ->
                            []
            in
            case model.marker of
                Tool.Marker marker ->
                    MouseUp :: save marker

                Tool.Eraser _ ->
                    [ MouseUp
                    , if model.scrollFriendly then
                        -- scroll-friendly mode only erases on double-click or drag
                        None

                      else
                        RemoveHint
                    ]

        LongPress targetId eventIndex ->
            if Just model.id == targetId then
                [ MouseOver eventIndex
                , MouseDown eventIndex
                , Hint eventIndex eventIndex
                ]

            else
                []


{-| We fold over actions using (Model marker) as the accumulator.
-}
performActions : Model marker -> List (Action marker) -> ( Model marker, List HighlightableUpdate, Cmd (Msg m) )
performActions model actions =
    let
        _ =
            Debug.log "actions" actions
    in
    List.foldl performAction ( model, [], [] ) actions
        |> (\( newModel, highlightableUpdates, cmds ) -> ( newModel, highlightableUpdates, Cmd.batch cmds ))


{-| Performs actual changes to the model, or emit a command.
-}
performAction :
    Action marker
    -> ( Model marker, List HighlightableUpdate, List (Cmd (Msg m)) )
    -> ( Model marker, List HighlightableUpdate, List (Cmd (Msg m)) )
performAction action ( model, highlightableUpdates, cmds ) =
    case action of
        None ->
            ( model, highlightableUpdates, cmds )

        Focus index ->
            ( { model | focusIndex = Just index }
            , UpdateFocused :: highlightableUpdates
            , Task.attempt Focused (Dom.focus (highlightableId model.id index)) :: cmds
            )

        Hint start end ->
            ( { model | hintingIndices = Just ( start, end ) }
            , UpdateHinted :: highlightableUpdates
            , cmds
            )

        Save marker ->
            case model.hintingIndices of
                Just hinting ->
                    let
                        ( indexesToSave, highlightables ) =
                            saveHinted marker hinting model.highlightables
                    in
                    ( { model
                        | highlightables = highlightables |> Debug.log "saved highlightables"
                        , markerRanges = findMarkerRanges highlightables
                        , hasChanged = Changed (HighlightCreated indexesToSave marker.kind)
                        , hintingIndices = Nothing
                      }
                    , UpdateHinted :: highlightableUpdates
                    , cmds
                    )

                Nothing ->
                    ( model, highlightableUpdates, cmds )

        Toggle index marker ->
            let
                ( highlightables, changed ) =
                    toggleHighlighted index marker model

                _ =
                    Debug.log "toggled highlightables" (List.map (\h -> ( h.index, h.marked |> List.map .kind )) highlightables)
            in
            ( { model
                | highlightables = highlightables
                , markerRanges = findMarkerRanges highlightables
                , hasChanged = changed
                , hintingIndices = Nothing
              }
            , highlightableUpdates
            , cmds
            )

        RemoveHint ->
            case model.hintingIndices of
                Just hinting ->
                    ( { model
                        | highlightables = removeHinted hinting model.highlightables
                        , hasChanged =
                            markerAtIndex (Tuple.first hinting) model.highlightables
                                |> Maybe.map (\marker -> Changed (HighlightRemoved hinting marker))
                                |> Maybe.withDefault NotChanged
                        , hintingIndices = Nothing
                      }
                    , highlightableUpdates
                    , cmds
                    )

                Nothing ->
                    ( model, highlightableUpdates, cmds )

        MouseDown index ->
            ( { model | mouseDownIndex = Just index }, highlightableUpdates, cmds )

        MouseUp ->
            ( { model | mouseDownIndex = Nothing, mouseOverIndex = Nothing }, highlightableUpdates, cmds )

        MouseOver index ->
            ( { model | mouseOverIndex = Just index }, highlightableUpdates, cmds )

        MouseOut ->
            ( { model | mouseOverIndex = Nothing }, highlightableUpdates, cmds )

        StartSelection index ->
            ( { model | selectionStartIndex = Just index }, highlightableUpdates, cmds )

        ExpandSelection index ->
            ( { model | selectionEndIndex = Just index }, highlightableUpdates, cmds )

        ResetSelection ->
            ( { model | selectionStartIndex = Nothing, selectionEndIndex = Nothing }, highlightableUpdates, cmds )


cleanHinted : Model marker -> Model marker -> Highlightable marker -> Highlightable marker
cleanHinted oldModel newModel highlightable =
    let
        maybeBetween =
            Maybe.map (\( a, b ) -> between a b highlightable)
                >> Maybe.withDefault False
    in
    if maybeBetween oldModel.hintingIndices && not (maybeBetween newModel.hintingIndices) then
        { highlightable | isHinted = False, isFirstOrLastHinted = False }

    else
        highlightable


updateHinted : Model marker -> Model marker -> Highlightable marker -> Highlightable marker
updateHinted oldModel newModel highlightable =
    case ( oldModel.hintingIndices /= newModel.hintingIndices, newModel.hintingIndices ) of
        ( True, Just ( start, end ) ) ->
            if start <= highlightable.index && highlightable.index <= end then
                { highlightable
                    | isHinted = True
                    , isFirstOrLastHinted =
                        highlightable.index == start || highlightable.index == end
                }

            else
                highlightable

        _ ->
            highlightable


cleanFocused : Model marker -> Model marker -> Highlightable marker -> Highlightable marker
cleanFocused oldModel newModel highlightable =
    case ( oldModel.focusIndex /= newModel.focusIndex, oldModel.focusIndex ) of
        ( True, Just oldFocused ) ->
            if oldFocused == highlightable.index then
                { highlightable | isFocused = False }

            else
                highlightable

        _ ->
            highlightable


updateFocused : Model marker -> Model marker -> Highlightable marker -> Highlightable marker
updateFocused oldModel newModel highlightable =
    if oldModel.focusIndex /= newModel.focusIndex && newModel.focusIndex == Just highlightable.index then
        { highlightable | isFocused = True }

    else
        highlightable


markerAtIndex : Int -> List (Highlightable marker) -> Maybe marker
markerAtIndex index highlightables =
    List.Extra.getAt index highlightables
        |> Maybe.andThen (\highligtable -> highligtable.marked |> List.head |> Maybe.map .kind)


between : Int -> Int -> Highlightable marker -> Bool
between from to { index } =
    if from < to then
        from <= index && index <= to

    else
        to <= index && index <= from


saveHinted : Tool.MarkerModel marker -> ( Int, Int ) -> List (Highlightable marker) -> ( ( Int, Int ), List (Highlightable marker) )
saveHinted marker ( hintBeginning, hintEnd ) =
    List.Extra.mapAccuml
        (\acc highlightable ->
            if between hintBeginning hintEnd highlightable then
                ( highlightable :: acc, Highlightable.set (Just marker) highlightable )

            else
                ( acc, highlightable )
        )
        []
        >> Tuple.mapBoth
            -- Report back what indexes we're actually going to save.
            -- Static highlightables at the edges are not saved
            firstLastIndexInteractive
            trimHighlightableGroups


{-| Get the first and last indexes, only counting interactive highlightables.
-}
firstLastIndexInteractive : List (Highlightable marker) -> ( Int, Int )
firstLastIndexInteractive list =
    let
        indexes =
            List.filterMap
                (\{ type_, index } ->
                    if type_ == Highlightable.Interactive then
                        Just index

                    else
                        Nothing
                )
                list
    in
    ( List.minimum indexes |> Maybe.withDefault 0
    , List.maximum indexes |> Maybe.withDefault 0
    )


{-| Toggle highlights

If we're allowing overlapping highlights:

  - on a click over an existing highlight
      - If we're using the same marker as the shortest marker in range, remove the highlight
      - If we're using a different marker, add a new overlapping highlight
  - on a click over a non-highlighted segment
      - Add a new highlight

If we're not allowing overlapping highlights:

  - on a click over an existing highlight
      - Remove the existing highlight
  - on a click over a non-highlighted segment
      - Add a new highlight

-}
toggleHighlighted : Int -> Tool.MarkerModel marker -> Model marker -> ( List (Highlightable marker), HasChanged marker )
toggleHighlighted index marker model =
    let
        maybeShortestMarker =
            selectShortestMarkerRange index model.markerRanges

        toggle acc highlightable =
            let
                maybeCreateHighlight _ =
                    if highlightable.index == index && highlightable.type_ == Highlightable.Interactive then
                        ( Changed (HighlightCreated ( index, index ) marker.kind)
                        , { highlightable | isHinted = False, isFirstOrLastHinted = False }
                            |> Highlightable.set (Just marker)
                        )

                    else
                        ( acc, highlightable )
            in
            case maybeShortestMarker of
                Just shortestMarker ->
                    if marker.kind == shortestMarker.marker && between shortestMarker.start shortestMarker.end highlightable then
                        ( Changed (HighlightRemoved ( shortestMarker.start, shortestMarker.end ) marker.kind)
                        , { highlightable | isHinted = False, isFirstOrLastHinted = False }
                            |> Highlightable.set Nothing
                        )

                    else
                        maybeCreateHighlight ()

                Nothing ->
                    maybeCreateHighlight ()

        ( changed, toggled ) =
            List.Extra.mapAccuml toggle NotChanged model.highlightables
    in
    ( trimHighlightableGroups toggled, changed )


{-| This removes all-static highlights. We need to track events on static elements,
so that we don't miss mouse events if a user starts or ends a highlight on a space, say,
but we should only persist changes to interactive segments.
It is meant to be called as a clean up after the highlightings have been changed.
-}
trimHighlightableGroups : List (Highlightable marker) -> List (Highlightable marker)
trimHighlightableGroups highlightables =
    let
        apply segment ( lastInteractiveHighlighterMarkers, staticAcc, acc ) =
            -- logic largely borrowed from joinAdjacentInteractiveHighlights.
            -- TODO in the next version: clean up the implementation!
            case segment.type_ of
                Highlightable.Interactive ->
                    let
                        bracketingHighlightTypes =
                            List.filterMap (\x -> List.Extra.find ((==) x) lastInteractiveHighlighterMarkers)
                                segment.marked

                        static_ =
                            -- for every static tag, ensure that if it's not between interactive segments
                            -- that share a mark in common, marks are removed.
                            List.map
                                (\s ->
                                    { s
                                        | marked =
                                            List.filterMap (\x -> List.Extra.find ((==) x) bracketingHighlightTypes)
                                                s.marked
                                    }
                                )
                                staticAcc
                    in
                    ( segment.marked, [], segment :: static_ ++ acc )

                Highlightable.Static ->
                    ( lastInteractiveHighlighterMarkers, segment :: staticAcc, acc )
    in
    highlightables
        |> List.foldr apply ( [], [], [] )
        |> (\( _, static_, acc ) -> removeHighlights_ static_ ++ acc)
        |> List.foldl apply ( [], [], [] )
        |> (\( _, static_, acc ) -> removeHighlights_ static_ ++ acc)
        |> List.reverse


type alias MarkerRange marker =
    { marker : marker, start : Int, end : Int, size : Int }


{-| Select the shortest possibly-overlapping marker covering the index provided, return its range.
-}
selectShortestMarkerRange : Int -> List (MarkerRange marker) -> Maybe (MarkerRange marker)
selectShortestMarkerRange index markerRanges =
    markerRanges
        |> List.filter (\{ start, end } -> start <= index && index <= end)
        |> List.Extra.minimumBy (\{ size } -> size)


{-| Find all the highlighted ranges in the format: {marker, start, end, size}
-}
findMarkerRanges : List (Highlightable marker) -> List (MarkerRange marker)
findMarkerRanges highlightables =
    List.foldl
        (\highlightable acc ->
            case List.map .kind highlightable.marked of
                [] ->
                    { open = [], closed = acc.open ++ acc.closed }

                markers ->
                    -- update all open, insert into open if new, close open if necessary
                    let
                        updateResult =
                            List.foldl
                                (updateMarkerRangeStep highlightable)
                                { open = [], toBeClosed = acc.open }
                                markers
                    in
                    { open = updateResult.open, closed = updateResult.toBeClosed ++ acc.closed }
        )
        { open = [], closed = [] }
        highlightables
        |> (\{ open, closed } -> open ++ closed)


{-| Fold step for finding marker ranges.

    At every fold step, we take all currently open markers and pre-tag them as "to be closed".

    This function then tries to find if they really should be closed or not.

    If they shouldn't, we put them back into the open list.

    If we don't find our marker in the toBeClosed list, we add a new record to the open list.

    If a "to be closed" record is not touched by this function, that means it really should have been closed.

-}
updateMarkerRangeStep :
    Highlightable marker
    -> marker
    -> { open : List (MarkerRange marker), toBeClosed : List (MarkerRange marker) }
    -> { open : List (MarkerRange marker), toBeClosed : List (MarkerRange marker) }
updateMarkerRangeStep highlightable marker acc =
    -- try to remove from toBeClosed if still open
    -- add new record to new if not in toBeClosed
    let
        ( keepOpen, toBeClosed ) =
            -- if current marker is in toBeClosed, it's not meant to be closed after all
            List.partition (\x -> x.marker == marker) acc.toBeClosed
    in
    if List.isEmpty keepOpen then
        -- if current marker was not in toBeClosed, it's a new marker
        { open =
            { marker = marker
            , start = highlightable.index
            , end = highlightable.index
            , size = String.length highlightable.text
            }
                :: acc.open
        , toBeClosed = toBeClosed
        }

    else
        { open =
            acc.open
                ++ List.map
                    (\openMarker ->
                        { marker = openMarker.marker
                        , start = openMarker.start
                        , end = highlightable.index
                        , size = openMarker.size + String.length highlightable.text
                        }
                    )
                    keepOpen
        , toBeClosed = toBeClosed
        }


removeHinted : ( Int, Int ) -> List (Highlightable marker) -> List (Highlightable marker)
removeHinted ( hintBeginning, hintEnd ) =
    List.map
        (\highlightable ->
            if between hintBeginning hintEnd highlightable then
                { highlightable | isHinted = False, isFirstOrLastHinted = False }
                    |> Highlightable.set Nothing

            else
                highlightable
        )


{-| -}
removeHighlights : Model marker -> Model marker
removeHighlights model =
    { model | highlightables = removeHighlights_ model.highlightables }


removeHighlights_ : List (Highlightable m) -> List (Highlightable m)
removeHighlights_ =
    List.map (Highlightable.set Nothing)


{-| You are not likely to need this helper unless you're working with inline commenting.
-}
clickedHighlightable : Model marker -> Maybe (Highlightable.Highlightable marker)
clickedHighlightable model =
    Maybe.andThen (\i -> List.Extra.getAt i model.highlightables) model.mouseDownIndex


{-| You are not likely to need this helper unless you're working with inline commenting.
-}
hoveredHighlightable : Model marker -> Maybe (Highlightable.Highlightable marker)
hoveredHighlightable model =
    Maybe.andThen (\i -> List.Extra.getAt i model.highlightables) model.mouseOverIndex


isHovered_ :
    { config
        | mouseOverIndex : Maybe Int
        , mouseDownIndex : Maybe Int
        , overlaps : OverlapsSupport marker
        , maybeTool : Maybe tool
    }
    -> List (List (Highlightable ma))
    -> Highlightable marker
    -> Bool
isHovered_ config groups highlightable =
    case config.maybeTool of
        Nothing ->
            False

        Just _ ->
            directlyHoveringInteractiveSegment config highlightable
                || (case config.overlaps of
                        OverlapsSupported { hoveredMarkerWithShortestHighlight } ->
                            inHoveredGroupForOverlaps config hoveredMarkerWithShortestHighlight highlightable

                        OverlapsNotSupported ->
                            inHoveredGroupWithoutOverlaps config groups highlightable
                   )


directlyHoveringInteractiveSegment : { config | mouseOverIndex : Maybe Int } -> Highlightable m -> Bool
directlyHoveringInteractiveSegment { mouseOverIndex } highlightable =
    (mouseOverIndex == Just highlightable.index)
        && (highlightable.type_ == Highlightable.Interactive)


inHoveredGroupWithoutOverlaps :
    { config
        | mouseOverIndex : Maybe Int
    }
    -> List (List (Highlightable ma))
    -> Highlightable m
    -> Bool
inHoveredGroupWithoutOverlaps config groups highlightable =
    case highlightable.marked of
        [] ->
            -- if the highlightable is not marked, then it shouldn't
            -- take on group hover styles
            -- if the mouse is over it, it's hovered.
            -- otherwise, it's not!
            Just highlightable.index == config.mouseOverIndex

        _ ->
            -- if the highlightable is in a group that's hovered,
            -- apply hovered styles
            groups
                |> List.filter (List.any (.index >> (==) highlightable.index))
                |> List.head
                |> Maybe.withDefault []
                |> List.any (.index >> Just >> (==) config.mouseOverIndex)


inHoveredGroupForOverlaps :
    { config
        | mouseDownIndex : Maybe Int
    }
    -> Maybe marker
    -> Highlightable marker
    -> Bool
inHoveredGroupForOverlaps config hoveredMarkerWithShortestHighlight highlightable =
    case config.mouseDownIndex of
        Just _ ->
            -- If the user is actively highlighting, don't show the entire highlighted region as hovered
            -- This is so that when creating an overlap, the hover styles don't imply that you've
            -- selected more than you have
            False

        Nothing ->
            case hoveredMarkerWithShortestHighlight of
                Nothing ->
                    False

                Just marker ->
                    List.member marker (List.map .kind highlightable.marked)



-- VIEWS


{-| -}
view : Model marker -> Unstyled.Html (Msg marker)
view =
    UnstyledLazy.lazy
        (\model ->
            view_
                { showTagsInline = False
                , maybeTool = Just model.marker
                , mouseOverIndex = model.mouseOverIndex
                , mouseDownIndex = model.mouseDownIndex
                , hintingIndices = model.hintingIndices
                , overlaps = OverlapsNotSupported
                , viewSegment =
                    viewHighlightable
                        False
                        model
                , id = model.id
                , highlightables = model.highlightables
                }
        )


{-| -}
viewWithOverlappingHighlights : Model marker -> Unstyled.Html (Msg marker)
viewWithOverlappingHighlights =
    UnstyledLazy.lazy
        (\model ->
            let
                overlaps =
                    findOverlapsSupport model
            in
            view_
                { showTagsInline = False
                , maybeTool = Just model.marker
                , mouseOverIndex = model.mouseOverIndex
                , mouseDownIndex = model.mouseDownIndex
                , hintingIndices = model.hintingIndices
                , overlaps = overlaps
                , viewSegment = viewHighlightable False model
                , id = model.id
                , highlightables = model.highlightables
                }
        )


findOverlapsSupport : Model marker -> OverlapsSupport marker
findOverlapsSupport model =
    case model.overlapsSupport of
        OverlapsNotSupported ->
            OverlapsNotSupported

        OverlapsSupported _ ->
            OverlapsSupported
                { hoveredMarkerWithShortestHighlight =
                    model.mouseOverIndex
                        |> Maybe.andThen
                            (\index ->
                                selectShortestMarkerRange index model.markerRanges
                                    |> Maybe.map .marker
                            )
                }


{-| A type that contains information needed to render individual `Highlightable`s one at a time
-}
type FoldState marker
    = FoldState
        { model : Model marker
        , overlapsSupport : OverlapsSupport marker
        , state : List ( Highlightable marker, Maybe (Unstyled.Html Never), List Attribute )
        , styles : Maybe (Unstyled.Html Never)
        }


{-| Computes all the mark styles necessary to perform a fold over the `Highlightable` elements
-}
initFoldState : Model marker -> FoldState marker
initFoldState model =
    let
        overlapsSupport =
            findOverlapsSupport model

        config =
            { hintingIndices = model.hintingIndices
            , mouseOverIndex = model.mouseOverIndex
            , mouseDownIndex = model.mouseDownIndex
            , maybeTool = Just model.marker
            , overlaps = overlapsSupport
            , highlightables = model.highlightables
            }

        highlightableGroups =
            buildGroups config model.highlightables

        toMark : Highlightable marker -> Tool.MarkerModel marker -> Mark.Mark
        toMark highlightable marker =
            { name = marker.name
            , startStyles = marker.startGroupClass
            , styles =
                markedHighlightableStyles
                    config.maybeTool
                    (isHovered_ config highlightableGroups)
                    highlightable
            , endStyles = marker.endGroupClass
            }

        markedSegments =
            model.highlightables
                |> List.map (\highlightable -> ( highlightable, List.map (toMark highlightable) highlightable.marked ))

        precomputedSegments =
            markedSegments
                |> Mark.overlappingStyles
                |> List.map (\( a, label, c ) -> ( a, label, c ))
    in
    FoldState
        { model = model
        , overlapsSupport = overlapsSupport
        , state = precomputedSegments
        , styles =
            Just
                (renderStyles
                    { showTagsInline = False
                    , id = model.id
                    , marks = List.concatMap Tuple.second markedSegments
                    , scrollFriendly = model.scrollFriendly
                    , maybeTool = Just model.marker
                    }
                )
        }


type StyleClass
    = Highlightable
    | HintedMark (Tool.MarkerModel ())
    | HintedMarkBoundary (Tool.MarkerModel ())
    | HintedEraser Tool.EraserModel
    | HoveredNotHinted (Tool.Tool ())


styleClassName : StyleClass -> String
styleClassName styleClass =
    sanitizeCssClassName <|
        case styleClass of
            Highlightable ->
                "highlighter-highlightable"

            HintedMark marker ->
                "highlighter_hinted_mark-" ++ Maybe.withDefault "highlight" marker.name

            HintedMarkBoundary marker ->
                "highlighter_hinted_mark_boundary-" ++ Maybe.withDefault "highlight" marker.name

            HintedEraser _ ->
                "highlighter-hinted-eraser"

            HoveredNotHinted tool ->
                case tool of
                    Tool.Marker marker ->
                        "highlighter-hovered-marker-" ++ Maybe.withDefault "highlight" marker.name

                    Tool.Eraser _ ->
                        "highlighter-hovered-eraser"


sanitizeCssClassName : String -> String
sanitizeCssClassName =
    case Regex.fromString "[ \\(\\)\\[\\]]" of
        Nothing ->
            identity

        Just regex ->
            Regex.replace regex (\_ -> "_")


{-| Function to render the <style> tag for styling a highlighter.
-}
renderStyles :
    { showTagsInline : Bool
    , id : String
    , marks : List Mark.Mark
    , scrollFriendly : Bool
    , maybeTool : Maybe (Tool.Tool marker)
    }
    -> Unstyled.Html msg
renderStyles { showTagsInline, id, marks, scrollFriendly, maybeTool } =
    Css.Global.global
        [ Css.Global.id id
            [ Css.Global.descendants
                (Mark.renderStyles
                    showTagsInline
                    (List.Extra.uniqueBy .name marks)
                )
            , Css.Global.descendants
                [ Css.Global.class (styleClassName Highlightable)
                    [ Css.focus [ Css.zIndex (Css.int 1), Css.position Css.relative ]
                    , if scrollFriendly && maybeTool /= Nothing then
                        Css.batch
                            [ -- block ios safari from selecting text
                              Css.property "-webkit-user-select" "none"
                            , Css.property "-webkit-touch-callout" "none"
                            , Css.property "user-select" "none"
                            ]

                      else if maybeTool /= Nothing then
                        Css.batch [ Css.property "user-select" "none" ]

                      else
                        Css.batch []
                    ]
                ]
            , Css.Global.descendants
                (case maybeTool of
                    Nothing ->
                        []

                    Just (Tool.Marker marker_) ->
                        let
                            marker =
                                Tool.mapMarker (\_ -> ()) marker_
                        in
                        [ Css.Global.class (styleClassName (HintedMark marker)) marker.hintClass
                        , Css.Global.class (styleClassName (HintedMarkBoundary marker)) [ hintStartEndAnnouncer marker ]
                        , Css.Global.class (styleClassName (HoveredNotHinted (Tool.Marker marker)))
                            (List.concat
                                [ marker.hoverClass
                                , marker.startGroupClass
                                , marker.endGroupClass
                                ]
                            )
                        ]

                    Just (Tool.Eraser eraser) ->
                        [ Css.Global.class (styleClassName (HintedEraser eraser)) eraser.hintClass
                        , Css.Global.class (styleClassName (HoveredNotHinted (Tool.Eraser eraser))) eraser.hoverClass
                        ]
                )
            ]
        ]
        |> Html.toUnstyled


{-| Render a single `Highlightable` while also returning an updated state.

A list of extraStyles is also accepted if, for example, you want to apply bold / italic / underline formatting to the generated span.

-}
viewFoldHighlighter : List Attribute -> FoldState marker -> ( FoldState marker, List (Unstyled.Html (Msg marker)) )
viewFoldHighlighter extraStyles (FoldState ({ model } as foldState)) =
    viewFoldHelper
        (viewHighlightable False model)
        extraStyles
        (FoldState foldState)


{-| Render a single `Highlightable` that is NOT interactive while also returning an updated state.

A list of extraStyles is also accepted if, for example, you want to apply bold / italic / underline formatting to the generated span.

-}
viewFoldStatic : List Attribute -> FoldState marker -> ( FoldState marker, List (Unstyled.Html msg) )
viewFoldStatic =
    viewFoldHelper
        (viewHighlightableSegment
            { interactiveHighlighterId = Nothing
            , eventListeners = []
            , maybeTool = Nothing
            , mouseOverIndex = Nothing
            , renderMarkdown = False
            , sorter = Nothing
            }
        )


viewFoldHelper : (Highlightable marker -> List Attribute -> Unstyled.Html msg) -> List Attribute -> FoldState marker -> ( FoldState marker, List (Unstyled.Html msg) )
viewFoldHelper viewSegment extraStyles (FoldState ({ state } as foldState)) =
    case state of
        [] ->
            -- If we are in this position then the caller has called the step function too many times.
            -- We return empty output and the same fold state.
            ( FoldState foldState, [] )

        ( highlightable, maybeLabelElement, markStyles ) :: todoState ->
            let
                segmentHtml =
                    viewSegment
                        highlightable
                        (markStyles ++ extraStyles)
            in
            ( FoldState
                { foldState
                    | state = todoState
                    , styles = Nothing
                }
            , case ( foldState.styles, maybeLabelElement ) of
                ( Nothing, Nothing ) ->
                    [ segmentHtml ]

                ( Just styles, Nothing ) ->
                    [ Unstyled.map never styles, segmentHtml ]

                ( Nothing, Just labelElement ) ->
                    [ Unstyled.map never labelElement, segmentHtml ]

                ( Just styles, Just labelElement ) ->
                    [ Unstyled.map never styles, Unstyled.map never labelElement, segmentHtml ]
            )


{-| Groups highlightables with the same state together.
-}
buildGroups :
    { model
        | mouseOverIndex : Maybe Int
    }
    -> List (Highlightable marker)
    -> List (List (Highlightable marker))
buildGroups model =
    List.Extra.groupWhile (groupHighlightables model)
        >> List.map (\( elem, list ) -> elem :: list)


groupHighlightables :
    { model
        | mouseOverIndex : Maybe Int
    }
    -> Highlightable marker
    -> Highlightable marker
    -> Bool
groupHighlightables { mouseOverIndex } x y =
    let
        xIsHovered =
            mouseOverIndex == Just x.index

        yIsHovered =
            mouseOverIndex == Just y.index

        xAndYHaveTheSameState =
            -- Both are hinted
            (x.isHinted && y.isHinted)
                || -- Neither is hinted
                   (not x.isHinted && not y.isHinted)
                || -- Neither is hovered
                   (not xIsHovered && not yIsHovered)
    in
    (xAndYHaveTheSameState
        && (List.head x.marked == Nothing)
        && (List.head y.marked == Nothing)
    )
        || (List.head x.marked == List.head y.marked && List.head x.marked /= Nothing)
        || ((List.head x.marked /= Nothing) && y.isHinted)
        || ((List.head y.marked /= Nothing) && x.isHinted)


type OverlapsSupport marker
    = OverlapsNotSupported
    | OverlapsSupported { hoveredMarkerWithShortestHighlight : Maybe marker }


{-| When elements are marked and the view doesn't support overlaps, wrap the marked elements in a single `mark` html node.
-}
view_ :
    { showTagsInline : Bool
    , maybeTool : Maybe (Tool.Tool marker)
    , mouseOverIndex : Maybe Int
    , mouseDownIndex : Maybe Int
    , hintingIndices : Maybe ( Int, Int )
    , overlaps : OverlapsSupport marker
    , viewSegment : Highlightable marker -> List Attribute -> Unstyled.Html msg
    , highlightables : List (Highlightable marker)
    , id : String
    }
    -> Unstyled.Html msg
view_ config =
    let
        toMark : Highlightable marker -> Tool.MarkerModel marker -> Mark.Mark
        toMark highlightable marker =
            { name = marker.name
            , startStyles = marker.startGroupClass
            , styles =
                markedHighlightableStyles
                    config.maybeTool
                    (isHovered_ config highlightableGroups)
                    highlightable
            , endStyles = marker.endGroupClass
            }

        highlightableGroups =
            buildGroups config config.highlightables

        withoutOverlaps : List (List ( Highlightable marker, Maybe Mark ))
        withoutOverlaps =
            List.map
                (\group ->
                    List.map
                        (\highlightable ->
                            ( highlightable
                            , Maybe.map (toMark highlightable) (List.head highlightable.marked)
                            )
                        )
                        group
                )
                highlightableGroups

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
    Unstyled.p [ UnstyledAttrs.id config.id, UnstyledAttrs.class "highlighter-container" ] <|
        renderStyles
            { id = config.id
            , showTagsInline = config.showTagsInline
            , marks =
                withOverlaps
                    |> List.map Tuple.second
                    |> List.concat

            -- TODO: possibly add a scrollFriendly flag to the view_ function
            -- make it so static is always False, and interactive takes it from Model
            , scrollFriendly = False
            , maybeTool = config.maybeTool
            }
            :: (if config.showTagsInline then
                    List.concatMap (Mark.viewWithInlineTags config.viewSegment) withoutOverlaps

                else
                    case config.overlaps of
                        OverlapsSupported _ ->
                            Mark.viewWithOverlaps config.viewSegment withOverlaps

                        OverlapsNotSupported ->
                            List.concatMap (Mark.view config.viewSegment) withoutOverlaps
               )


viewHighlightable :
    Bool
    -> Model marker
    -> Highlightable marker
    -> List Attribute
    -> Unstyled.Html (Msg marker)
viewHighlightable =
    UnstyledLazy.lazy4 <|
        \renderMarkdown model highlightable customAttributes ->
            case highlightable.type_ of
                Highlightable.Interactive ->
                    viewHighlightableSegment
                        { interactiveHighlighterId = Just model.id
                        , eventListeners = highlightableEventListeners model.id model.scrollFriendly highlightable
                        , renderMarkdown = renderMarkdown
                        , maybeTool = Just model.marker
                        , mouseOverIndex = model.mouseOverIndex
                        , sorter = Just model.sorter
                        }
                        highlightable
                        customAttributes

                Highlightable.Static ->
                    viewHighlightableSegment
                        { interactiveHighlighterId = Nothing
                        , eventListeners = highlightableEventListeners model.id model.scrollFriendly highlightable
                        , renderMarkdown = renderMarkdown
                        , maybeTool = Just model.marker
                        , mouseOverIndex = model.mouseOverIndex
                        , sorter = Just model.sorter
                        }
                        highlightable
                        customAttributes


highlightableEventListeners : String -> Bool -> Highlightable marker -> List (Unstyled.Attribute (Msg marker))
highlightableEventListeners highlighterId scrollFriendly highlightable =
    case highlightable.type_ of
        Highlightable.Interactive ->
            [ onPreventDefault "mouseover" (Pointer <| Over highlightable.index)
            , onPreventDefault "mouseleave" (Pointer <| Out)
            , onPreventDefault "mouseup" (Pointer <| Up <| Just highlighterId)
            , onPreventDefault "mousedown" (Pointer <| Down highlightable.index)
            , onPreventDefault "contextmenu" (Touch <| TouchIgnored)
            , AttributesExtra.unstyledIncludeIf (not scrollFriendly)
                (onPreventDefault "touchstart" (Touch <| TouchStart highlightable.index))
            , AttributesExtra.unstyledIncludeIf (not scrollFriendly)
                (onPreventDefault "touchend" (Touch <| TouchEnd <| Just highlighterId))
            , AttributesExtra.unstyledIncludeIf scrollFriendly
                (onClickPreventDefault
                    (\count ->
                        Pointer <|
                            Click { index = highlightable.index, clickCount = count }
                    )
                )
            , UnstyledAttrs.attribute "data-interactive" ""
            , onKeyDownPreventDefault
                [ Key.space (Keyboard <| ToggleHighlight highlightable.index)
                , Key.right (Keyboard <| MoveRight highlightable.index)
                , Key.left (Keyboard <| MoveLeft highlightable.index)
                , Key.shiftRight (Keyboard <| SelectionExpandRight highlightable.index)
                , Key.shiftLeft (Keyboard <| SelectionExpandLeft highlightable.index)
                ]
            , onKeyUpPreventDefault
                [ Key.shift (Keyboard <| SelectionApplyTool highlightable.index)
                    -- Key.shift has `shiftKey` set to True, but the keyUp event
                    -- for releasing the shift key has `shiftKey` set to False.
                    |> (\k -> { k | shiftKey = False })
                , -- Escape while shift is down cancels selection
                  Key.escape (Keyboard <| SelectionReset highlightable.index)
                    |> (\k -> { k | shiftKey = True })
                ]
            ]

        Highlightable.Static ->
            -- Static highlightables need listeners as well.
            -- because otherwise we miss mouse events.
            -- For example, a user hovering over a static space in a highlight
            -- should see the entire highlight change to hover styles.
            [ onPreventDefault "mouseover" (Pointer <| Over highlightable.index)
            , onPreventDefault "mouseleave" (Pointer <| Out)
            , onPreventDefault "contextmenu" (Touch <| TouchIgnored)
            , AttributesExtra.unstyledIncludeIf (not scrollFriendly)
                (onPreventDefault "touchstart" (Touch <| TouchStart highlightable.index))
            , AttributesExtra.unstyledIncludeIf (not scrollFriendly)
                (onPreventDefault "touchend" (Touch <| TouchEnd <| Just highlighterId))
            , AttributesExtra.unstyledIncludeIf scrollFriendly
                (onClickPreventDefault
                    (\count ->
                        Pointer <|
                            Click { index = highlightable.index, clickCount = count }
                    )
                )
            , onPreventDefault "mousedown" (Pointer <| Down highlightable.index)
            , onPreventDefault "mouseup" (Pointer <| Up <| Just highlighterId)
            , UnstyledAttrs.attribute "data-static" ""
            ]


viewHighlightableSegment :
    { interactiveHighlighterId : Maybe String
    , eventListeners : List (Unstyled.Attribute msg)
    , maybeTool : Maybe (Tool.Tool marker)
    , mouseOverIndex : Maybe Int
    , renderMarkdown : Bool
    , sorter : Maybe (Sorter marker)
    }
    -> Highlightable marker
    -> List Attribute
    -> Unstyled.Html msg
viewHighlightableSegment ({ interactiveHighlighterId, eventListeners, renderMarkdown } as config) highlightable customAttributes =
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
                    [ UnstyledAttrs.class "highlighter-whitespace-tab" ]

                " " ->
                    [ UnstyledAttrs.class "highlighter-whitespace-single-space" ]

                "\n" ->
                    [ UnstyledAttrs.class "highlighter-whitespace-newline" ]

                _ ->
                    []

        isInteractive =
            interactiveHighlighterId /= Nothing
    in
    Unstyled.span
        (eventListeners
            ++ List.map toHtmlAttribute highlightable.customAttributes
            ++ List.map toHtmlAttribute customAttributes
            ++ unmarkedHighlightableStyles config highlightable
            ++ whitespaceClass highlightable.text
            ++ [ UnstyledAttrs.attribute "data-highlighter-item-index" <| String.fromInt highlightable.index
               , case interactiveHighlighterId of
                    Just highlighterId ->
                        UnstyledAttrs.id (highlightableId highlighterId highlightable.index)

                    Nothing ->
                        AttributesExtra.unstyledNone
               , UnstyledAttrs.class (styleClassName Highlightable)
               , case List.head highlightable.marked of
                    Just _ ->
                        UnstyledAttrs.class "highlighter-highlighted"

                    _ ->
                        AttributesExtra.unstyledNone
               , if highlightable.isHinted then
                    UnstyledAttrs.class "highlighter-hinted"

                 else
                    AttributesExtra.unstyledNone
               , if isInteractive then
                    if highlightable.isFocused then
                        UnstyledAttrs.tabindex 0

                    else
                        UnstyledAttrs.tabindex -1

                 else
                    AttributesExtra.unstyledNone
               ]
        )
        (if renderMarkdown then
            renderInlineMarkdown highlightable.text
                |> List.map Html.toUnstyled

         else
            [ Unstyled.text highlightable.text ]
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

        Markdown.Block.Custom _ blocks ->
            List.concatMap inlinifyMarkdownBlock blocks


highlightableId : String -> Int -> String
highlightableId highlighterId index =
    "highlighter-" ++ highlighterId ++ "-highlightable-" ++ String.fromInt index


unmarkedHighlightableStyles :
    { config
        | maybeTool : Maybe (Tool.Tool marker)
        , mouseOverIndex : Maybe Int
    }
    -> Highlightable marker
    -> List (Unstyled.Attribute msg)
unmarkedHighlightableStyles config highlightable =
    if highlightable.marked /= [] then
        []

    else
        case config.maybeTool of
            Nothing ->
                []

            Just tool ->
                let
                    isHovered =
                        directlyHoveringInteractiveSegment config highlightable
                in
                case tool of
                    Tool.Marker marker ->
                        if highlightable.isHinted then
                            [ [ UnstyledAttrs.class (styleClassName (HintedMark (Tool.mapMarker (\_ -> ()) marker)))
                              ]
                            , if highlightable.isFirstOrLastHinted then
                                -- only announce first or last hinted bc that's where
                                -- keyboard focus will be
                                [ UnstyledAttrs.class
                                    (styleClassName (HintedMarkBoundary (Tool.mapMarker (\_ -> ()) marker)))
                                ]

                              else
                                []
                            ]
                                |> List.concat

                        else if isHovered then
                            -- When hovered, but not marked
                            [ UnstyledAttrs.class (styleClassName (HoveredNotHinted (Tool.map (\_ -> ()) tool))) ]

                        else
                            []

                    Tool.Eraser eraser_ ->
                        if highlightable.isHinted then
                            [ UnstyledAttrs.class (styleClassName (HintedEraser eraser_)) ]

                        else if isHovered then
                            [ UnstyledAttrs.class (styleClassName (HoveredNotHinted (Tool.Eraser eraser_))) ]

                        else
                            []


{-| Announce for screenreaders that we are at the last hinting index
-}
hintStartEndAnnouncer : Tool.MarkerModel marker -> Css.Style
hintStartEndAnnouncer marker =
    Css.after
        [ Css.property
            "content"
            ("\" (selecting text for "
                ++ (Maybe.map
                        (\name -> stripMarkdownSyntax name)
                        marker.name
                        |> Maybe.withDefault "highlight"
                   )
                ++ ") \""
            )
        , invisibleStyle
        ]


stripMarkdownSyntax : String -> String
stripMarkdownSyntax markdown =
    case Markdown.Block.parse Nothing markdown of
        [ Markdown.Block.Paragraph _ inlines ] ->
            Markdown.Inline.extractText inlines

        _ ->
            markdown


markedHighlightableStyles :
    Maybe (Tool.Tool marker)
    -> (Highlightable marker -> Bool)
    -> Highlightable marker
    -> List Css.Style
markedHighlightableStyles maybeTool getIsHovered ({ marked } as highlightable) =
    case maybeTool of
        Nothing ->
            [ case List.head marked of
                Just markedWith ->
                    Css.batch markedWith.highlightClass

                Nothing ->
                    Css.backgroundColor Css.transparent
            ]

        Just tool ->
            let
                isHovered =
                    getIsHovered highlightable
            in
            case tool of
                Tool.Marker marker ->
                    [ Css.property "user-select" "none"
                    , case List.head marked of
                        Just markedWith ->
                            if highlightable.isHinted then
                                Css.batch marker.hintClass

                            else if isHovered then
                                -- Override marking with selected tool
                                Css.batch marker.hoverHighlightClass

                            else
                                -- otherwise, show the standard mark styles
                                Css.batch markedWith.highlightClass

                        Nothing ->
                            if highlightable.isHinted then
                                Css.batch marker.hintClass

                            else if isHovered then
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

                Tool.Eraser eraser_ ->
                    case List.head marked of
                        Just markedWith ->
                            [ Css.property "user-select" "none"
                            , Css.batch markedWith.highlightClass
                            , Css.batch
                                (if highlightable.isHinted then
                                    eraser_.hintClass

                                 else if isHovered then
                                    eraser_.hoverClass

                                 else
                                    []
                                )
                            ]

                        Nothing ->
                            [ Css.property "user-select" "none", Css.backgroundColor Css.transparent ]


{-| Helper for `on` to preventDefault.
-}
onPreventDefault : String -> msg -> Unstyled.Attribute msg
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
    UnstyledEvents.preventDefaultOn name
        checkIfCancelable


{-| Helper for `on` to preventDefault and capture `detail` from click event
-}
onClickPreventDefault : (Int -> msg) -> Unstyled.Attribute msg
onClickPreventDefault msg =
    let
        -- If we attempt to preventDefault on an event which is not cancelable
        -- Chrome will blow up and complain that:
        --
        -- Ignored attempt to cancel a touchmove event with cancelable=false,
        -- for example because scrolling is in progress and cannot be interrupted.
        --
        -- So instead we only preventDefault when it is safe to do so.
        checkIfCancelable =
            Json.Decode.map2
                (\detail result -> ( msg detail, result ))
                (Json.Decode.field "detail" Json.Decode.int)
                (Json.Decode.field "cancelable" Json.Decode.bool)
    in
    UnstyledEvents.preventDefaultOn "click"
        checkIfCancelable


onKeyDownPreventDefault : List (Event msg) -> Unstyled.Attribute msg
onKeyDownPreventDefault decoders =
    alwaysPreventDefault "keydown" decoders


onKeyUpPreventDefault : List (Event msg) -> Unstyled.Attribute msg
onKeyUpPreventDefault decoders =
    alwaysPreventDefault "keyup" decoders


alwaysPreventDefault : String -> List (Event msg) -> Unstyled.Attribute msg
alwaysPreventDefault event decoders =
    decoders
        |> Key.customOneOf
        |> Json.Decode.map (\decoder -> ( decoder, True ))
        |> UnstyledEvents.preventDefaultOn event
