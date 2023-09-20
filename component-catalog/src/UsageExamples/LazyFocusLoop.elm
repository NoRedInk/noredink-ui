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
import List.Extra as List
import Maybe.Extra as Maybe
import Nri.Ui.Button.V10 as Button
import Nri.Ui.CharacterIcon.V2 as Characters
import Nri.Ui.FocusLoop.Lazy.V1 as FocusLoop
import Nri.Ui.FocusLoop.V1 as FocusLoop
import Nri.Ui.Switch.V3 as Switch
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


type alias State =
    { characters : List ( Int, Character )
    , tooltip : Maybe Tooltip
    , counter : Int
    , useLazy : Bool
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
    , useLazy = True
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
                        |> List.getAt (List.length state.characters |> modBy (List.length characters))
                        |> Maybe.map (\character -> ( state.counter, character ))
                    )
    }


type Msg
    = ToggleLazy Bool
    | ToggleTooltip Int Bool
    | Focus String
    | AddCharacters Int
    | RemoveCharacter Int
    | NoOp


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        ToggleLazy useLazy ->
            ( { state | useLazy = useLazy }, Cmd.none )

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

        Focus id ->
            ( state
            , Task.attempt (always NoOp) (Dom.focus id)
            )

        AddCharacters n ->
            ( state |> addCharacters n
            , Cmd.none
            )

        RemoveCharacter id ->
            ( { state
                | characters =
                    List.filter (\( id_, _ ) -> id /= id_) state.characters
              }
            , Cmd.none
            )

        NoOp ->
            ( state, Cmd.none )


view : State -> List (Html Msg)
view state =
    let
        addCharacterButtonKeyed =
            ( "add-character", Html.li [] [ Button.button "Add character" [ Button.onClick (AddCharacters 1) ] ] )

        add100CharactersButtonKeyed =
            ( "add-100-characters", Html.li [] [ Button.button "Add 100 characters" [ Button.onClick (AddCharacters 100) ] ] )

        removeCharacterButtonsKeyed =
            if state.useLazy then
                viewCharactersLazy <|
                    List.map
                        (\( id, character ) -> FocusLoop.Args3 id character (state.tooltip == Just (Tooltip id)))
                        state.characters

            else
                let
                    keys =
                        List.map (Tuple.first >> String.fromInt) state.characters
                in
                List.zip keys (viewCharacters state)
    in
    [ Switch.view
        { label = "Use Lazy"
        , id = "lazy-switch"
        }
        [ Switch.selected state.useLazy
        , Switch.onSwitch ToggleLazy
        ]
    , Keyed.ul
        [ Attrs.css
            [ Css.listStyle Css.none
            , Css.displayFlex
            , Css.flexWrap Css.wrap
            , Css.property "gap" "20px"
            ]
        ]
        (addCharacterButtonKeyed :: add100CharactersButtonKeyed :: removeCharacterButtonsKeyed)
    ]


viewCharacters : State -> List (Html Msg)
viewCharacters state =
    FocusLoop.view
        { id = \( id, _ ) -> buttonDomId id
        , focus = Focus
        , leftRight = True
        , upDown = False
        , view = \focusKeyEvents ( id, character ) -> viewCharacter focusKeyEvents id character (state.tooltip == Just (Tooltip id))
        }
        state.characters


viewCharactersLazy : List (FocusLoop.Args3 Int Character Bool) -> List ( String, Html Msg )
viewCharactersLazy =
    FocusLoop.lazy3
        { id = \(FocusLoop.Args3 id _ _) -> buttonDomId id
        , focus = Focus
        , leftRight = True
        , upDown = False
        , view = \focusKeyEvents (FocusLoop.Args3 id character tooltipOpen) -> viewCharacter focusKeyEvents id character tooltipOpen
        }


viewCharacter : List (Key.Event Msg) -> Int -> Character -> Bool -> Html Msg
viewCharacter focusKeyEvents id character tooltipOpen =
    Html.li []
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
