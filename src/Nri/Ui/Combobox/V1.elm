port module Nri.Ui.Combobox.V1 exposing (..)

import Accessibility.Styled exposing (..)
import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Key as Key
import Accessibility.Styled.Role as Role
import Browser.Dom as Dom
import Css
import Html.Styled as HtmlStyled
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events as Events
import Json.Decode as Decode
import List.Extra
import Nri.Ui.Fonts.V1
import Nri.Ui.TextInput.V7 as TextInput
import Nri.Ui.Util exposing (isSubstringWithinDistance)
import String
import Task


type alias Option =
    { value : String
    , label : String
    }


type alias Options =
    List Option


type Msg
    = UpdateInput String
    | UpdateOptions Options
    | SelectOption (Maybe Option)
    | SelectOptionAndFocus (Maybe Option)
    | Expand
    | Collapse
    | CollapseAndFocus
    | Focus (Result Dom.Error ())
    | Toggle
    | MoveUp
    | MoveDown
    | Scroll Float


port scrollToItemInList : { elementId : String, direction : String } -> Cmd msg


type alias Model =
    { id : String
    , label : String
    , zIndex : Int
    , input : String
    , options : Options
    , filteredOptions : Options
    , expanded : Bool
    , selected : Maybe Option
    , activeDescendant : Maybe Option
    , optionHeight : Float
    , containerHeight : Float
    , scrollTop : Float
    }


init : String -> String -> Int -> Options -> Model
init id label zIndex options =
    { id = id
    , label = label
    , zIndex = zIndex
    , input = ""
    , options = options
    , filteredOptions = options
    , selected = Nothing
    , expanded = False
    , activeDescendant = Nothing
    , optionHeight = 40
    , containerHeight = 202
    , scrollTop = 0
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateInput input ->
            ( { model | input = input, filteredOptions = filterOptions model.options input, expanded = True }
            , Cmd.none
            )

        UpdateOptions options ->
            ( { model | options = options, filteredOptions = filterOptions options model.input }, Cmd.none )

        SelectOption maybeOption ->
            ( { model
                | selected = maybeOption
                , activeDescendant = maybeOption
                , input =
                    case maybeOption of
                        Just option ->
                            option.label

                        Nothing ->
                            ""
                , filteredOptions = model.options
                , expanded = False
              }
            , Cmd.none
            )

        SelectOptionAndFocus maybeOption ->
            ( { model
                | selected = maybeOption
                , activeDescendant = maybeOption
                , input =
                    case maybeOption of
                        Just option ->
                            option.label

                        Nothing ->
                            ""
                , filteredOptions = model.options
                , expanded = False
              }
            , Task.attempt Focus (Dom.focus (model.id ++ "-textbox"))
            )

        Expand ->
            ( { model | expanded = True }, Cmd.none )

        Collapse ->
            ( { model | expanded = False }, Cmd.none )

        CollapseAndFocus ->
            ( { model | expanded = False }, Task.attempt Focus (Dom.focus (model.id ++ "-textbox")) )

        Focus _ ->
            ( model, Cmd.none )

        Toggle ->
            ( { model | expanded = not model.expanded }, Cmd.none )

        MoveUp ->
            ( { model
                | activeDescendant =
                    case model.activeDescendant of
                        Just activeDescendant ->
                            findPrevious model.filteredOptions activeDescendant

                        Nothing ->
                            List.head (List.reverse model.filteredOptions)
                , expanded = True
              }
            , case model.activeDescendant of
                Just activeDescendant ->
                    scrollToItemInList { elementId = model.id ++ "-option-" ++ activeDescendant.value, direction = "up" }

                Nothing ->
                    Cmd.none
            )

        MoveDown ->
            ( { model
                | activeDescendant =
                    case model.activeDescendant of
                        Just activeDescendant ->
                            findNext model.filteredOptions activeDescendant

                        Nothing ->
                            List.head model.filteredOptions
                , expanded = True
              }
            , case model.activeDescendant of
                Just activeDescendant ->
                    scrollToItemInList { elementId = model.id ++ "-option-" ++ activeDescendant.value, direction = "down" }

                Nothing ->
                    Cmd.none
            )

        Scroll scrollTop ->
            ( { model | scrollTop = scrollTop }
            , Cmd.none
            )


normalizeString : String -> String
normalizeString =
    String.toLower >> String.trim


filterOptions : Options -> String -> Options
filterOptions options input =
    List.filter (\option -> isSubstringWithinDistance (normalizeString input) (normalizeString option.label) 1) options


findNext : List a -> a -> Maybe a
findNext list current =
    let
        result =
            List.foldl
                (\x ( maybeNext, hasMatched ) ->
                    if hasMatched then
                        ( Just x, False )

                    else
                        ( maybeNext, x == current )
                )
                ( Nothing, False )
                list
                |> Tuple.first
    in
    case result of
        Just next ->
            Just next

        Nothing ->
            List.head (List.reverse list)


findPrevious : List a -> a -> Maybe a
findPrevious list current =
    let
        result =
            List.foldr
                (\x ( maybePrevious, hasMatched ) ->
                    if hasMatched then
                        ( Just x, False )

                    else
                        ( maybePrevious, x == current )
                )
                ( Nothing, False )
                list
                |> Tuple.first
    in
    case result of
        Just previous ->
            Just previous

        Nothing ->
            List.head list


overlay : Model -> Html Msg
overlay model =
    HtmlStyled.div
        [ Attributes.css
            [ Css.position Css.fixed
            , Css.property "inset" "0"
            , Css.zIndex (Css.int model.zIndex)
            , Css.backgroundColor Css.transparent
            ]
        , Events.onClick CollapseAndFocus
        ]
        []


textbox : Model -> Html Msg
textbox model =
    TextInput.view model.label
        [ TextInput.id (model.id ++ "-textbox")
        , TextInput.text UpdateInput
        , TextInput.value model.input
        , TextInput.onClick Toggle
        , TextInput.onKeyDownPreventDefault
            [ Key.down MoveDown
            , Key.up MoveUp
            , Key.enter <| SelectOption model.activeDescendant
            , Key.escape <| Collapse
            ]
        , TextInput.custom
            [ Role.comboBox
            , Aria.autoCompleteList
            , Aria.controls [ model.id ++ "-listbox" ]
            , Aria.expanded model.expanded
            , Aria.activeDescendant
                (case model.activeDescendant of
                    Just activeDescendant ->
                        model.id ++ "-option-" ++ activeDescendant.value

                    Nothing ->
                        ""
                )
            ]
        ]


toggle : Model -> Html Msg
toggle model =
    button
        [ Attributes.type_ "button"
        , Aria.label model.label
        , Aria.expanded model.expanded
        , Aria.controls [ model.id ++ "-listbox" ]
        , Key.tabbable False
        , Events.onClick Toggle
        , css
            [ Css.boxSizing Css.borderBox
            , Css.position Css.absolute
            , Css.top (Css.px 9)
            , Css.bottom Css.zero
            , Css.right Css.zero
            , Css.displayFlex
            , Css.alignItems Css.center
            , Css.cursor Css.pointer
            , Css.border Css.zero
            , Css.backgroundColor Css.transparent
            , Css.outline Css.none
            , Css.color (Css.rgb 98 110 124)
            , Css.paddingLeft (Css.px 10)
            , Css.paddingRight (Css.px 10)
            ]
        ]
        [ text
            (if model.expanded then
                "▲"

             else
                "▼"
            )
        ]


listbox : Model -> Html Msg
listbox model =
    let
        -- The bufferSize constant controls how many additional items are rendered above and below the visible items to ensure smooth scrolling
        bufferSize =
            5

        visibleItemCount =
            Debug.log "visibleItemCount" <| round (model.containerHeight / model.optionHeight)

        startIndex =
            Debug.log "startIndex" <| max 0 (round (model.scrollTop / model.optionHeight) - bufferSize)

        endIndex =
            Debug.log "endIndex" <| min (List.length model.filteredOptions - 1) (startIndex + visibleItemCount + 2 * bufferSize - 1)

        visibleOptions =
            Debug.log "visibleOptions" <| List.drop startIndex (List.take (endIndex + 1) model.filteredOptions)

        activeDescendantIndex =
            case model.activeDescendant of
                Just activeDescendant ->
                    List.Extra.findIndex (\option -> option.value == activeDescendant.value) model.filteredOptions

                Nothing ->
                    Just -1

        activeDescendantNeighborStartIndex =
            case activeDescendantIndex of
                Just index ->
                    max 0 (index - bufferSize)

                Nothing ->
                    0

        activeDescendantNeighborEndIndex =
            case activeDescendantIndex of
                Just index ->
                    min (List.length model.filteredOptions - 1) (index + bufferSize)

                Nothing ->
                    0

        activeDescendantNeighbors =
            Debug.log "activeDescendantNeighbors" <|
                List.drop activeDescendantNeighborStartIndex (List.take (activeDescendantNeighborEndIndex + 1) model.filteredOptions)

        visibleOptionsOffset =
            Css.px (Debug.log "visibleOptionsOffset" <| toFloat startIndex * model.optionHeight)

        activeDescendantNeighborsOffset =
            Css.px (Debug.log "activeDescendantNeighborsOffset" <| toFloat activeDescendantNeighborStartIndex * model.optionHeight)

        totalHeight =
            Css.px (Debug.log "totalHeight" <| toFloat (List.length model.filteredOptions) * model.optionHeight)
    in
    HtmlStyled.ul
        [ Role.listBox
        , Aria.label model.label
        , Aria.setSize (List.length model.filteredOptions)
        , Attributes.id (model.id ++ "-listbox")
        , Events.on "scroll" (Decode.map Scroll (Decode.at [ "target", "scrollTop" ] Decode.float))
        , css
            [ Css.boxSizing Css.borderBox
            , if model.expanded && List.length model.filteredOptions > 0 then
                Css.display Css.block

              else
                Css.display Css.none
            , Css.height (Css.px (min model.containerHeight (toFloat (List.length model.filteredOptions * 40) + 2)))
            , Css.listStyle Css.none
            , Css.marginLeft Css.zero
            , Css.marginTop (Css.px 5)
            , Css.padding Css.zero
            , Css.border3 (Css.px 1) Css.solid (Css.rgb 217 217 217)
            , Css.borderRadius (Css.px 8)
            , Css.overflow Css.auto
            , Css.boxShadow5 (Css.px 0) (Css.px 0) (Css.px 10) (Css.px 0) (Css.rgba 0 0 0 0.1)
            , Css.position Css.absolute
            , Css.width (Css.pct 100)
            , Css.zIndex (Css.int model.zIndex)
            ]
        ]
        [ HtmlStyled.div
            [ Attributes.css
                [ Css.position Css.relative
                , Css.height totalHeight
                ]
            ]
            [ HtmlStyled.div
                [ Attributes.css
                    [ Css.position Css.absolute
                    , Css.top visibleOptionsOffset
                    , Css.width (Css.pct 100)
                    ]
                ]
                (List.map (listboxOption model) visibleOptions)
            , HtmlStyled.div
                [ Attributes.css
                    [ Css.position Css.absolute
                    , Css.top activeDescendantNeighborsOffset
                    , Css.width (Css.pct 100)
                    ]
                ]
                (List.map (listboxOption model) activeDescendantNeighbors)
            ]
        ]


listboxOption : Model -> Option -> Html Msg
listboxOption model option =
    HtmlStyled.li
        [ Role.option
        , Aria.selected (model.activeDescendant == Just option)
        , Events.onClick (SelectOptionAndFocus (Just option))
        , Attributes.id (model.id ++ "-option-" ++ option.value)
        , css
            [ Css.boxSizing Css.borderBox
            , Css.cursor Css.pointer
            , if model.activeDescendant == Just option then
                Css.backgroundColor (Css.rgb 212 240 255)

              else
                Css.backgroundColor Css.transparent
            , Css.height (Css.px model.optionHeight)
            , Css.padding2 (Css.px 10) (Css.px 20)
            , Nri.Ui.Fonts.V1.baseFont
            ]
        ]
        [ text option.label ]


view : Model -> Html Msg
view model =
    div
        [ Attributes.css
            [ Css.position Css.relative
            ]
        ]
        [ if model.expanded then
            overlay model

          else
            text ""
        , div
            [ Attributes.css
                [ Css.position Css.relative
                , Css.zIndex (Css.int (model.zIndex + 1))
                ]
            ]
            [ textbox model
            , toggle model
            ]
        , listbox model
        ]
