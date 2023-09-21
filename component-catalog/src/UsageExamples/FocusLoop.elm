module UsageExamples.FocusLoop exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Accessibility.Styled as Html exposing (Attribute, Html)
import Accessibility.Styled.Key as Key
import Browser.Dom as Dom
import Css
import Html.Styled
import Html.Styled.Attributes as Attrs
import Html.Styled.Events as Events
import Html.Styled.Keyed as Keyed
import Html.Styled.Lazy as Lazy
import List.Extra as List
import Nri.Ui.Button.V10 as Button
import Nri.Ui.FocusLoop.Lazy.V1 as FocusLoop
import Nri.Ui.FocusLoop.V1 as FocusLoop
import Nri.Ui.Html.V3 exposing (viewIf)
import Nri.Ui.Switch.V3 as Switch
import Nri.Ui.TextInput.V7 as TextInput
import Nri.Ui.Tooltip.V3 as Tooltip
import Task
import UsageExample exposing (UsageExample)


example : UsageExample State Msg
example =
    { name = "Focus Loop"
    , categories = []
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , about = []
    , view = view
    }


type alias State =
    { settings : Settings
    , counter : Int
    , items : List Int
    , tooltip : Maybe Tooltip
    }


type alias Settings =
    { useLazy : Bool
    , simulateExpensiveComputation : Bool
    , simulateExpensiveComputationIterations : Setting Int
    }


type Setting a
    = Setting a
    | UnsavedSetting ( a, a )


updateSetting : Setting a -> a -> Setting a
updateSetting setting unsavedValue =
    case setting of
        Setting savedValue ->
            UnsavedSetting ( savedValue, unsavedValue )

        UnsavedSetting ( savedValue, _ ) ->
            UnsavedSetting ( savedValue, unsavedValue )


saveSetting : Setting a -> Setting a
saveSetting setting =
    case setting of
        Setting _ ->
            setting

        UnsavedSetting ( _, unsavedValue ) ->
            Setting unsavedValue


settingValue : Setting a -> a
settingValue setting =
    case setting of
        Setting savedValue ->
            savedValue

        UnsavedSetting ( savedValue, _ ) ->
            savedValue


unsavedSettingValue : Setting a -> a
unsavedSettingValue setting =
    case setting of
        Setting savedValue ->
            savedValue

        UnsavedSetting ( _, unsavedValue ) ->
            unsavedValue


type Tooltip
    = ItemTooltip Int
    | LazyToggleHelpTooltip
    | SimulateExpensiveComputationToggleHelpTooltip


itemTooltipOpen : Maybe Tooltip -> Int -> Bool
itemTooltipOpen activeTooltip id =
    activeTooltip == Just (ItemTooltip id)


init : State
init =
    { items = []
    , tooltip = Nothing
    , counter = 0
    , settings =
        { useLazy = True
        , simulateExpensiveComputation = False
        , simulateExpensiveComputationIterations = Setting 50
        }
    }
        |> addItems 50


addItems : Int -> State -> State
addItems n state =
    { state | items = state.items ++ List.range state.counter (state.counter + n - 1), counter = state.counter + n }


type Msg
    = ToggleTooltip Tooltip Bool
    | ToggleLazy Bool
    | ToggleSimulateExpensiveComputation Bool
    | UpdateExpensiveComputationIterations (Maybe Int)
    | SaveExpensiveComputationIterations
    | AddItems Int
    | RemoveItem Int
    | Focus String
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

        ToggleTooltip tooltip isOpen ->
            ( { state
                | tooltip =
                    if isOpen then
                        Just tooltip

                    else
                        Nothing
              }
            , Cmd.none
            )

        UpdateExpensiveComputationIterations maybeIter ->
            ( { state
                | settings =
                    { settings
                        | simulateExpensiveComputationIterations =
                            maybeIter
                                |> Maybe.map (updateSetting settings.simulateExpensiveComputationIterations)
                                |> Maybe.withDefault settings.simulateExpensiveComputationIterations
                    }
              }
            , Cmd.none
            )

        SaveExpensiveComputationIterations ->
            ( { state
                | settings =
                    { settings
                        | simulateExpensiveComputationIterations = settings.simulateExpensiveComputationIterations |> saveSetting
                    }
              }
            , Cmd.none
            )

        Focus id ->
            ( state
            , Task.attempt (always NoOp) (Dom.focus id)
            )

        AddItems n ->
            ( state |> addItems n
            , Cmd.none
            )

        RemoveItem item ->
            ( { state | items = List.remove item state.items }
            , Cmd.none
            )

        NoOp ->
            ( state, Cmd.none )


buttonDomId : Int -> String
buttonDomId id =
    "button-" ++ String.fromInt id


view : State -> List (Html Msg)
view state =
    let
        addItemButtonKeyed =
            ( "add-item", Html.li [] [ Button.button "Add 1" [ Button.onClick (AddItems 1) ] ] )

        add10ItemsButtonKeyed =
            ( "add-10-items", Html.li [] [ Button.button "Add 10" [ Button.onClick (AddItems 10) ] ] )

        itemButtonsKeyed =
            if state.settings.useLazy then
                viewItemsLazy <|
                    List.map
                        (\item ->
                            { settings = state.settings
                            , tooltip = state.tooltip
                            , item = item
                            }
                        )
                        state.items

            else
                viewItems state.settings state.tooltip state.items
    in
    [ Html.div [ Attrs.css [ Css.displayFlex, Css.flexDirection Css.column, Css.property "gap" "10px" ] ]
        [ viewLazyToggle state.settings.useLazy (state.tooltip == Just LazyToggleHelpTooltip)
        , viewSimulateExpensiveComputationToggle state.settings (state.tooltip == Just SimulateExpensiveComputationToggleHelpTooltip)
        , Keyed.ul
            [ Attrs.css
                [ Css.listStyle Css.none
                , Css.displayFlex
                , Css.flexWrap Css.wrap
                , Css.property "gap" "20px"
                ]
            ]
            (addItemButtonKeyed :: add10ItemsButtonKeyed :: itemButtonsKeyed)
        ]
    ]


viewLazyToggle : Bool -> Bool -> Html Msg
viewLazyToggle =
    Lazy.lazy2 <|
        \useLazy tooltipOpen ->
            Html.div [ Attrs.css [ Css.displayFlex, Css.alignItems Css.center, Css.property "gap" "10px" ] ]
                [ Switch.view
                    { label = "Use Lazy"
                    , id = "lazy-switch"
                    }
                    [ Switch.selected useLazy
                    , Switch.onSwitch ToggleLazy
                    ]
                , Tooltip.viewToggleTip { label = "lazy-tip", lastId = Nothing }
                    [ Tooltip.plaintext "Uses FocusLoop.Lazy. Open the developer console to see when/where vDOM diff evaluations are occuring."
                    , Tooltip.open tooltipOpen
                    , Tooltip.onToggle (ToggleTooltip LazyToggleHelpTooltip)
                    ]
                ]


viewSimulateExpensiveComputationToggle : Settings -> Bool -> Html Msg
viewSimulateExpensiveComputationToggle =
    Lazy.lazy2 <|
        \settings tooltipOpen ->
            Html.div [ Attrs.css [ Css.displayFlex, Css.alignItems Css.center, Css.property "gap" "10px" ] ]
                [ Switch.view
                    { label = "Simulate Expensive Computation"
                    , id = "simulate-expensive-computation-switch"
                    }
                    [ Switch.selected settings.simulateExpensiveComputation
                    , Switch.onSwitch ToggleSimulateExpensiveComputation
                    ]
                , viewIf
                    (\_ ->
                        Html.div [ Attrs.css [ Css.displayFlex ] ]
                            [ viewComplexityInput (unsavedSettingValue settings.simulateExpensiveComputationIterations)
                            ]
                    )
                    settings.simulateExpensiveComputation
                , Tooltip.viewToggleTip { label = "simulate-expensive-computation-tip", lastId = Nothing }
                    [ Tooltip.plaintext "This will simulate an expensive computation when rendering each individual item. You must have the developer console open to see the effects of this setting."
                    , Tooltip.open tooltipOpen
                    , Tooltip.onToggle (ToggleTooltip SimulateExpensiveComputationToggleHelpTooltip)
                    ]
                ]


viewComplexityInput : Int -> Html Msg
viewComplexityInput complexity =
    Html.Styled.div [ Events.onMouseLeave SaveExpensiveComputationIterations ]
        [ TextInput.view "Complexity"
            [ TextInput.number UpdateExpensiveComputationIterations
            , TextInput.value (Just complexity)
            , TextInput.onBlur SaveExpensiveComputationIterations
            , TextInput.onEnter SaveExpensiveComputationIterations
            , TextInput.guidance "Higher = Slower"
            ]
        ]


viewItems : Settings -> Maybe Tooltip -> List Int -> List ( String, Html Msg )
viewItems settings tooltip items =
    let
        keys =
            List.map (\id -> String.fromInt id)

        views =
            FocusLoop.view
                { toId = buttonDomId
                , focus = Focus
                , leftRight = True
                , upDown = False
                , view =
                    \keyEvents id ->
                        viewItem keyEvents (howMuchBrrrrr settings) id (itemTooltipOpen tooltip id)
                }
    in
    List.zip (keys items) (views items)


viewItemsLazy :
    List
        { settings : Settings
        , tooltip : Maybe Tooltip
        , item : Int
        }
    -> List ( String, Html Msg )
viewItemsLazy =
    FocusLoop.lazy3
        { toId = .item >> buttonDomId
        , focus = Focus
        , leftRight = True
        , upDown = False
        , view = viewItem
        , apply =
            \view_ { settings, tooltip, item } ->
                view_ (howMuchBrrrrr settings) item (itemTooltipOpen tooltip item)
        }


viewItem : List (Key.Event Msg) -> Int -> Int -> Bool -> Html Msg
viewItem focusKeyEvents expensiveComputationIters id tooltipOpen =
    let
        hash =
            goBrrrrr id expensiveComputationIters
    in
    Html.li [ Attrs.class hash ]
        [ Tooltip.view
            { id = String.fromInt id |> Debug.log "vDOM evaluated for"
            , trigger = viewTrigger id focusKeyEvents
            }
            [ Tooltip.open tooltipOpen
            , Tooltip.primaryLabel
            , Tooltip.smallPadding
            , Tooltip.fitToContent
            , Tooltip.plaintext "Remove me!"
            , Tooltip.onToggle (ToggleTooltip (ItemTooltip id))
            ]
        ]


viewTrigger : Int -> List (Key.Event Msg) -> List (Attribute Msg) -> Html Msg
viewTrigger id focusKeyEvents attrs =
    Button.button (String.fromInt id)
        [ Button.id (buttonDomId id)
        , Button.custom
            (attrs
                -- @TODO get rid of the conflict with the tooltip so that we can listen to all
                -- key events (this is currently overriding the esc key handl/er from the tooltip
                -- attrs on the line above)
                ++ [ Key.onKeyDownPreventDefault focusKeyEvents ]
            )
        , Button.onClick (RemoveItem id)
        ]


howMuchBrrrrr : Settings -> Int
howMuchBrrrrr settings =
    if settings.simulateExpensiveComputation then
        settingValue settings.simulateExpensiveComputationIterations

    else
        0


goBrrrrr : Int -> Int -> String
goBrrrrr input iterations =
    let
        computeHash n base modulus x =
            if n <= 0 then
                1

            else
                -- This Debug.log is here to make sure Elm doesn't optimize away the computation
                Debug.log "brrrr" <| remainderBy (computeHash (n - 1) base modulus x * base * x) modulus
    in
    computeHash iterations 37 104729 input |> String.fromInt
