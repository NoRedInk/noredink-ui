module Debug.Control exposing
    ( Control
    , value
    , bool, string
    , values, maybe, choice, list, record, field
    , map
    , view, currentValue, allValues
    )

{-| Create interactive controls for complex data structures.

@docs Control
@docs value
@docs bool, string
@docs values, maybe, choice, list, record, field
@docs map

@docs view, currentValue, allValues

-}

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import String


{-| An interactive control that produces a value `a`.
-}
type Control a
    = Control
        { currentValue : a
        , allValues : () -> List a
        , view : ControlView a
        }


type ControlView a
    = NoView
    | SingleView (() -> Html (Control a))
    | FieldViews (List ( String, () -> Html (Control a) ))


{-| A `Control` that has a static value (and no UI).
-}
value : a -> Control a
value initial =
    Control
        { currentValue = initial
        , allValues = \() -> [ initial ]
        , view = NoView
        }


{-| A `Control` that chooses between a list of values with a dropdown UI.

The first value will be the initial value.

-}
values : List a -> Control a
values choices =
    choice (List.map (\x -> ( toString x, value x )) choices)


{-| A `Control` that wraps another control in a `Maybe`, which a checkbox UI.

The `Bool` parameter is the initial value, where `False` is `Nothing`,
and `True` is `Just` with the value of the nested control.

-}
maybe : Bool -> Control a -> Control (Maybe a)
maybe isJust (Control value) =
    Control
        { currentValue =
            if isJust then
                Just value.currentValue

            else
                Nothing
        , allValues =
            \() ->
                Nothing
                    :: List.map Just (value.allValues ())
        , view =
            SingleView <|
                \() ->
                    Html.span
                        [ Html.Attributes.style [ ( "white-space", "nowrap" ) ]
                        ]
                        [ Html.input
                            [ Html.Attributes.type_ "checkbox"
                            , Html.Events.onCheck (flip maybe (Control value))
                            , Html.Attributes.checked isJust
                            ]
                            []
                        , Html.text " "
                        , if isJust then
                            view_ (maybe isJust) (Control value)

                          else
                            Html.text "Nothing"
                        ]
        }


{-| A `Control` that toggles a `Bool` with a checkbox UI.
-}
bool : Bool -> Control Bool
bool value =
    Control
        { currentValue = value
        , allValues =
            \() ->
                [ value
                , not value
                ]
        , view =
            SingleView <|
                \() ->
                    Html.span []
                        [ Html.input
                            [ Html.Attributes.type_ "checkbox"
                            , Html.Events.onCheck bool
                            , Html.Attributes.checked value
                            ]
                            []
                        , Html.text " "
                        , Html.text <| toString value
                        ]
        }


{-| A `Control` that allows text input.
-}
string : String -> Control String
string value =
    Control
        { currentValue = value
        , allValues =
            \() ->
                [ value
                , ""
                , "short"
                , "Longwordyesverylongwithnospacessupercalifragilisticexpialidocious"
                , "Long text lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
                ]
        , view =
            SingleView <|
                \() ->
                    Html.input
                        [ Html.Attributes.value value
                        , Html.Events.onInput string
                        ]
                        []
        }


{-| A `Control` that chooses between a list of nested controls.

This will crash if you provide an empty list.

The first entry will be the initial value.

-}
choice : List ( String, Control a ) -> Control a
choice choices =
    case choices of
        [] ->
            Debug.crash "No choices given"

        first :: rest ->
            choice_ [] first rest


choice_ :
    List ( String, Control a )
    -> ( String, Control a )
    -> List ( String, Control a )
    -> Control a
choice_ left current right =
    Control
        { currentValue = current |> Tuple.second |> currentValue
        , allValues =
            \() ->
                (List.reverse left ++ [ current ] ++ right)
                    |> List.map (Tuple.second >> allValues)
                    |> List.concat
        , view =
            SingleView <|
                \() ->
                    let
                        option selected ( label, value ) =
                            Html.option
                                [ Html.Attributes.selected selected ]
                                [ Html.text label ]

                        selectNew i =
                            let
                                all =
                                    List.reverse left
                                        ++ [ current ]
                                        ++ right

                                left_ =
                                    all
                                        |> List.take i
                                        |> List.reverse

                                current_ =
                                    all
                                        |> List.drop i
                                        |> List.head
                                        |> Maybe.withDefault current

                                right_ =
                                    all
                                        |> List.drop (i + 1)
                            in
                            choice_ left_ current_ right_ |> Debug.log "new"

                        updateChild new =
                            choice_ left ( Tuple.first current, new ) right
                    in
                    Html.div []
                        [ Html.map selectNew <|
                            Html.select
                                [ Html.Events.on "change" (Json.Decode.at [ "target", "selectedIndex" ] Json.Decode.int)
                                ]
                            <|
                                List.concat
                                    [ List.map (option False) <| List.reverse left
                                    , [ option True current ]
                                    , List.map (option False) right
                                    ]
                        , view_ updateChild (Tuple.second current)
                        ]
        }


{-| A `Control` that provides a list of selected length.
-}
list : Control a -> Control (List a)
list itemControl =
    list_ itemControl 1 0 10


list_ : Control a -> Int -> Int -> Int -> Control (List a)
list_ itemControl current min max =
    let
        makeList n =
            allValues itemControl
                |> List.repeat n
                |> List.concat
                |> List.take n
    in
    Control
        { currentValue = makeList current
        , allValues =
            \() ->
                [ 1, 0, 3 ]
                    |> List.filter (\x -> x > min && x < max)
                    |> flip List.append [ min, max ]
                    |> List.map makeList
        , view =
            SingleView <|
                \() ->
                    let
                        selectNew new =
                            list_ itemControl new min max
                    in
                    Html.map
                        (String.toInt
                            >> Result.toMaybe
                            >> Maybe.withDefault current
                            >> selectNew
                        )
                    <|
                        Html.label []
                            [ Html.text ""
                            , Html.input
                                [ Html.Attributes.type_ "range"
                                , Html.Attributes.min <| toString min
                                , Html.Attributes.max <| toString max
                                , Html.Attributes.step <| toString 1
                                , Html.Attributes.attribute "value" <| toString current
                                , Html.Events.on "input" Html.Events.targetValue
                                ]
                                []
                            ]
        }


{-| Create a `Control` representing a record with multiple fields.

This uses an API similar to [elm-decode-pipeline](http://package.elm-lang.org/packages/NoRedInk/elm-decode-pipeline/latest).

You will use this with `field`.

    import Debug.Control exposing (field, record, string)

    type alias Point =
        { x : String
        , y : String
        }

    pointControl : Control Point
    pointControl =
        record Point
            |> field "x" (string "initial x value")
            |> field "y" (string "initial y value")

-}
record : a -> Control a
record fn =
    Control
        { currentValue = fn
        , allValues = \() -> [ fn ]
        , view = FieldViews []
        }


{-| Used with `record` to create a `Control` representing a record.

See [`record`](#record).

-}
field : String -> Control a -> Control (a -> b) -> Control b
field name (Control value) (Control pipeline) =
    Control
        { currentValue = pipeline.currentValue value.currentValue
        , allValues =
            \() ->
                value.allValues ()
                    |> List.concatMap
                        (\v ->
                            List.map (\p -> p v)
                                (pipeline.allValues ())
                        )
        , view =
            let
                otherFields =
                    case pipeline.view of
                        FieldViews fs ->
                            List.map (Tuple.mapSecond (\x -> \() -> Html.map (field name (Control value)) (x ())))
                                fs

                        _ ->
                            []

                newView () =
                    view_ (\v -> field name v (Control pipeline)) (Control value)
            in
            FieldViews (( name, newView ) :: otherFields)
        }


{-| Transform the value produced by a `Control`.
-}
map : (a -> b) -> Control a -> Control b
map fn (Control a) =
    let
        mapTuple ( label, value ) =
            ( label, map fn value )
    in
    Control
        { currentValue = fn a.currentValue
        , allValues = \() -> List.map fn (a.allValues ())
        , view =
            case a.view of
                NoView ->
                    NoView

                SingleView v ->
                    SingleView <|
                        \() -> Html.map (map fn) (v ())

                FieldViews fs ->
                    FieldViews <|
                        List.map (Tuple.mapSecond (\v -> \() -> Html.map (map fn) (v ()))) fs
        }


{-| Gets the current value of a `Control`.
-}
currentValue : Control a -> a
currentValue (Control c) =
    c.currentValue


{-| TODO: revise API
-}
allValues : Control a -> List a
allValues (Control c) =
    c.allValues ()


{-| Renders the interactive UI for a `Control`.
-}
view : (Control a -> msg) -> Control a -> Html msg
view msg (Control c) =
    let
        fieldRow ( name, view ) =
            Html.tr []
                [ Html.td
                    [ Html.Attributes.style [ ( "text-align", "right" ) ] ]
                    [ Html.text name ]
                , Html.td [] [ Html.text " = " ]
                , Html.td [] [ view () ]
                ]
    in
    Html.div []
        [ view_ msg (Control c)
        ]


view_ : (Control a -> msg) -> Control a -> Html msg
view_ msg (Control c) =
    let
        fieldRow ( name, view ) =
            Html.tr
                [ Html.Attributes.style
                    [ ( "vertical-align", "text-top" ) ]
                ]
                [ Html.td [] [ Html.text "," ]
                , Html.td
                    [ Html.Attributes.style
                        [ ( "text-align", "right" ) ]
                    ]
                    [ Html.text name ]
                , Html.td [] [ Html.text " = " ]
                , Html.td [] [ view () ]
                ]
    in
    case c.view of
        NoView ->
            Html.text ""

        SingleView v ->
            Html.map msg <| v ()

        FieldViews fs ->
            List.concat
                [ [ Html.tr
                        [ Html.Attributes.style
                            [ ( "vertical-align", "text-top" ) ]
                        ]
                        [ Html.td [] [ Html.text "{" ] ]
                  ]
                , fs
                    |> List.reverse
                    |> List.map fieldRow
                , [ Html.tr [] [ Html.td [] [ Html.text "}" ] ] ]
                ]
                |> Html.table []
                |> Html.map msg
