module UsageExamples.LazyFocusLoop exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Accessibility.Styled as Html exposing (Attribute, Html)
import Accessibility.Styled.Key as Key
import Browser.Dom as Dom
import Css
import Html.Styled.Attributes as Attrs
import Html.Styled.Keyed as Keyed
import Html.Styled.Lazy as Lazy
import List.Extra as List
import Maybe.Extra as Maybe
import Nri.Ui.Button.V10 as Button
import Nri.Ui.CharacterIcon.V2 as Characters
import Nri.Ui.FocusLoop.Lazy.V1 as FocusLoop
import Nri.Ui.FocusLoop.V1 as FocusLoop
import Nri.Ui.Switch.V3 as Switch
import Nri.Ui.Text.V6 as Text
import Nri.Ui.TextInput.V7 as TextInput
import Nri.Ui.Tooltip.V3 as Tooltip
import Task
import UsageExample exposing (UsageExample)


example : UsageExample State Msg
example =
    { name = "Lazy Focus Loop"
    , categories = []
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , about = []
    , view = view
    }


type alias Settings =
    { useLazy : Bool
    , simulateExpensiveComputation : Bool
    , simulateExpensiveComputationIterations : Int
    }


type alias State =
    { characters : List ( Int, Character )
    , tooltip : Maybe Tooltip
    , counter : Int
    , settings : Settings
    }


type Character
    = Lindy
    | Sal
    | Red


type Tooltip
    = Tooltip Int


characters : List Character
characters =
    [ Lindy, Sal, Red ]


init : State
init =
    { characters = []
    , tooltip = Nothing
    , counter = 0
    , settings =
        { useLazy = True
        , simulateExpensiveComputation = False
        , simulateExpensiveComputationIterations = 50
        }
    }
        |> addCharacters 50


addCharacters : Int -> State -> State
addCharacters n state =
    List.repeat n () |> List.foldl (\() -> addCharacter) state


addCharacter : State -> State
addCharacter state =
    { state
        | counter = state.counter + 1
        , characters =
            state.characters
                |> Maybe.cons
                    (characters
                        |> List.getAt (modBy (List.length characters) state.counter)
                        |> Maybe.map (Tuple.pair state.counter)
                    )
    }


type Msg
    = ToggleLazy Bool
    | ToggleSimulateExpensiveComputation Bool
    | ToggleTooltip Int Bool
    | SetExpensiveComputationIterations (Maybe Int)
    | Focus String
    | AddCharacters Int
    | RemoveCharacter Int
    | NoOp


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    let
        settings =
            state.settings
    in
    case msg of
        ToggleLazy useLazy ->
            ( { state | settings = { settings | useLazy = useLazy } }, Cmd.none )

        ToggleSimulateExpensiveComputation simulateExpensiveComputation ->
            ( { state | settings = { settings | simulateExpensiveComputation = simulateExpensiveComputation } }, Cmd.none )

        ToggleTooltip tooltipId isOpen ->
            ( { state
                | tooltip =
                    if isOpen then
                        Just (Tooltip tooltipId)

                    else
                        Nothing
              }
            , Cmd.none
            )

        SetExpensiveComputationIterations maybeIter ->
            ( { state
                | settings =
                    { settings
                        | simulateExpensiveComputationIterations =
                            maybeIter |> Maybe.withDefault settings.simulateExpensiveComputationIterations
                    }
              }
            , Cmd.none
            )

        Focus id ->
            ( state
            , Task.attempt (always NoOp) (Dom.focus id)
            )

        AddCharacters n ->
            ( state |> addCharacters n
            , Cmd.none
            )

        RemoveCharacter id ->
            ( { state | characters = List.filter (Tuple.first >> (/=) id) state.characters }
            , Cmd.none
            )

        NoOp ->
            ( state, Cmd.none )


view : State -> List (Html Msg)
view state =
    let
        addCharacterButtonKeyed =
            ( "add-character", Html.li [] [ Button.button "Add character" [ Button.onClick (AddCharacters 1) ] ] )

        add10CharactersButtonKeyed =
            ( "add-10-characters", Html.li [] [ Button.button "Add 10 characters" [ Button.onClick (AddCharacters 10) ] ] )

        removeCharacterButtonsKeyed =
            state.characters
                |> List.map
                    (\( id, character ) ->
                        FocusLoop.Args4
                            state.settings
                            id
                            character
                            (state.tooltip == Just (Tooltip id))
                    )
                |> (if state.settings.useLazy then
                        viewCharactersLazy

                    else
                        viewCharacters
                   )
    in
    [ Text.mediumBody
        [ Text.plaintext "Open the developer console to see when/where vDOM diff evaluations are occuring"
        ]
    , viewLazyToggle state.settings.useLazy
    , viewSimulateExpensiveComputationToggle state.settings
    , Keyed.ul
        [ Attrs.css
            [ Css.listStyle Css.none
            , Css.displayFlex
            , Css.flexWrap Css.wrap
            , Css.property "gap" "20px"
            ]
        ]
        (addCharacterButtonKeyed :: add10CharactersButtonKeyed :: removeCharacterButtonsKeyed)
    ]


viewLazyToggle : Bool -> Html Msg
viewLazyToggle =
    Lazy.lazy <|
        \useLazy ->
            Switch.view
                { label = "Use Lazy"
                , id = "lazy-switch"
                }
                [ Switch.selected useLazy
                , Switch.onSwitch ToggleLazy
                ]


viewSimulateExpensiveComputationToggle : Settings -> Html Msg
viewSimulateExpensiveComputationToggle =
    Lazy.lazy <|
        \settings ->
            Html.div [ Attrs.css [ Css.displayFlex, Css.property "gap" "10px" ] ]
                [ Switch.view
                    { label = "Simulate Expensive Computation"
                    , id = "simulate-expensive-computation-switch"
                    }
                    [ Switch.selected settings.simulateExpensiveComputation
                    , Switch.onSwitch ToggleSimulateExpensiveComputation
                    ]
                , if settings.simulateExpensiveComputation then
                    TextInput.view "Complexity"
                        [ TextInput.number SetExpensiveComputationIterations
                        , TextInput.value (Just settings.simulateExpensiveComputationIterations)
                        , TextInput.guidance "Higher = more expensive"
                        ]

                  else
                    Html.text ""
                ]


viewCharacters : List (FocusLoop.Args4 Settings Int Character Bool) -> List ( String, Html Msg )
viewCharacters items =
    let
        keys =
            List.map (\(FocusLoop.Args4 _ id _ _) -> String.fromInt id)

        views =
            FocusLoop.view
                { toId = \(FocusLoop.Args4 _ id _ _) -> buttonDomId id
                , focus = Focus
                , leftRight = True
                , upDown = False
                , view =
                    \(FocusLoop.Args4 settings id character tooltipOpen) ->
                        viewCharacter settings id character tooltipOpen
                }
    in
    List.zip (keys items) (views items)


viewCharactersLazy : List (FocusLoop.Args4 Settings Int Character Bool) -> List ( String, Html Msg )
viewCharactersLazy =
    FocusLoop.lazy4
        { toId = \(FocusLoop.Args4 _ id _ _) -> buttonDomId id
        , focus = Focus
        , leftRight = True
        , upDown = False
        , view =
            \(FocusLoop.Args4 settings id character tooltipOpen) ->
                viewCharacter settings id character tooltipOpen
        }


goBrrrrr : Int -> Int -> String
goBrrrrr input iterations =
    let
        computeHash n base modulus x =
            -- This needs to be here or elm will optimize it out
            if Debug.log "brrrr" n <= 0 then
                1

            else
                remainderBy (computeHash (n - 1) base modulus x * base * x) modulus
    in
    computeHash iterations 37 104729 input |> String.fromInt


viewCharacter : Settings -> Int -> Character -> Bool -> List (Key.Event Msg) -> Html Msg
viewCharacter settings id character tooltipOpen focusKeyEvents =
    let
        hash =
            if settings.simulateExpensiveComputation then
                goBrrrrr id settings.simulateExpensiveComputationIterations

            else
                ""
    in
    Html.li [ Attrs.class hash ]
        [ Tooltip.view
            { id =
                String.fromInt id
                    |> Debug.log "vDOM evaluated at index"
            , trigger = viewTrigger id character focusKeyEvents
            }
            [ Tooltip.open tooltipOpen
            , Tooltip.primaryLabel
            , Tooltip.plaintext "Remove me!"
            , Tooltip.onToggle (ToggleTooltip id)
            ]
        ]


viewTrigger : Int -> Character -> List (Key.Event Msg) -> List (Attribute Msg) -> Html Msg
viewTrigger id character focusKeyEvents attrs =
    Button.button "Remove me!"
        [ Button.id (buttonDomId id)
        , Button.custom
            (attrs
                -- @TODO get rid of the conflict with the tooltip so that we can listen to all
                -- key events (this is currently overriding the esc key handl/er from the tooltip
                -- attrs on the line above)
                ++ [ Key.onKeyDownPreventDefault focusKeyEvents
                   ]
            )
        , Button.onClick (RemoveCharacter id)
        , Button.icon
            (case character of
                Lindy ->
                    Characters.lindyHeadshot

                Sal ->
                    Characters.salHeadshot

                Red ->
                    Characters.redHeadshot
            )
        ]


buttonDomId : Int -> String
buttonDomId id =
    "button-" ++ String.fromInt id
