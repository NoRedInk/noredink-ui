module Debug.Control exposing
    ( Control
    , value
    , bool, string, stringTextarea
    , maybe, choice, record, field
    , map
    , view, currentValue
    , lazy
    )

{-| Copied in from avh4/elm-debug-control.

Create interactive controls for complex data structures.

@docs Control
@docs value
@docs bool, string, stringTextarea
@docs maybe, choice, record, field
@docs map

@docs view, currentValue
@docs lazy

-}

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode


{-| An interactive control that produces a value `a`.
-}
type Control a
    = Control
        { currentValue : () -> a
        , view : () -> ControlView a
        }


type ControlView a
    = NoView
    | SingleView (Html (Control a))
    | FieldViews (List ( String, Html (Control a) ))


{-| A `Control` that has a static value (and no UI).
-}
value : a -> Control a
value initial =
    Control
        { currentValue = \() -> initial
        , view = \() -> NoView
        }


{-| A `Control` that wraps another control in a `Maybe`, which a checkbox UI.

The `Bool` parameter is the initial value, where `False` is `Nothing`,
and `True` is `Just` with the value of the nested control.

-}
maybe : Bool -> Control a -> Control (Maybe a)
maybe isJust (Control control) =
    Control
        { currentValue =
            \() ->
                if isJust then
                    Just (control.currentValue ())

                else
                    Nothing
        , view =
            \() ->
                SingleView <|
                    Html.span
                        [ Html.Attributes.style "white-space" "nowrap"
                        ]
                        [ Html.input
                            [ Html.Attributes.type_ "checkbox"
                            , Html.Events.onCheck (\a -> maybe a (Control control))
                            , Html.Attributes.checked isJust
                            ]
                            []
                        , Html.text " "
                        , if isJust then
                            view_ (maybe isJust) (Control control)

                          else
                            Html.text "Nothing"
                        ]
        }


{-| A `Control` that toggles a `Bool` with a checkbox UI.
-}
bool : Bool -> Control Bool
bool initialValue =
    Control
        { currentValue = \() -> initialValue
        , view =
            \() ->
                SingleView <|
                    Html.span []
                        [ Html.input
                            [ Html.Attributes.type_ "checkbox"
                            , Html.Events.onCheck bool
                            , Html.Attributes.checked initialValue
                            ]
                            []
                        , Html.text " "
                        , case initialValue of
                            True ->
                                Html.text "True"

                            False ->
                                Html.text "False"
                        ]
        }


{-| A `Control` that allows text input.
-}
string : String -> Control String
string initialValue =
    Control
        { currentValue = \() -> initialValue
        , view =
            \() ->
                SingleView <|
                    Html.input
                        [ Html.Attributes.value initialValue
                        , Html.Events.onInput string
                        ]
                        []
        }


{-| A `Control` that allows multiline text input.
-}
stringTextarea : String -> Control String
stringTextarea initialValue =
    Control
        { currentValue = \() -> initialValue
        , view =
            \() ->
                SingleView <|
                    Html.textarea
                        [ Html.Attributes.value initialValue
                        , Html.Events.onInput stringTextarea
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
            -- Debug.crash "No choices given"
            choice choices

        first :: rest ->
            choice_ [] first rest


choice_ :
    List ( String, Control a )
    -> ( String, Control a )
    -> List ( String, Control a )
    -> Control a
choice_ left current right =
    Control
        { currentValue = \() -> current |> Tuple.second |> currentValue
        , view =
            \() ->
                SingleView <|
                    let
                        option selected ( label, _ ) =
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
                            choice_ left_ current_ right_

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
        { currentValue = \() -> fn
        , view = \() -> FieldViews []
        }


{-| Used with `record` to create a `Control` representing a record.

See [`record`](#record).

-}
field : String -> Control a -> Control (a -> b) -> Control b
field name (Control control) (Control pipeline) =
    Control
        { currentValue = \() -> pipeline.currentValue () (control.currentValue ())
        , view =
            \() ->
                let
                    otherFields =
                        case pipeline.view () of
                            FieldViews fs ->
                                List.map (Tuple.mapSecond (\x -> Html.map (field name (Control control)) x))
                                    fs

                            _ ->
                                []

                    newView =
                        view_ (\v -> field name v (Control pipeline)) (Control control)
                in
                FieldViews (( name, newView ) :: otherFields)
        }


{-| Transform the value produced by a `Control`.
-}
map : (a -> b) -> Control a -> Control b
map fn (Control a) =
    Control
        { currentValue = \() -> fn (a.currentValue ())
        , view = \() -> mapView fn (a.view ())
        }


{-| Use lazy when working with recursive types:

    import Debug.Control as Control exposing (Control)

    type RecursiveType
        = RecursiveType (Maybe RecursiveType)

    recursiveTypeControl : Control RecursiveType
    recursiveTypeControl =
        Control.choice
            [ ( "No child", Control.value Nothing )
            , ( "child", Control.lazy (\() -> recursiveTypeControl) |> Control.map Just )
            ]
            |> Control.map RecursiveType

-}
lazy : (() -> Control a) -> Control a
lazy fn =
    let
        unwrap (Control v) =
            v
    in
    Control
        { currentValue = \() -> (unwrap (fn ())).currentValue ()
        , view = \() -> (unwrap (fn ())).view ()
        }


mapView : (a -> b) -> ControlView a -> ControlView b
mapView fn controlView =
    case controlView of
        NoView ->
            NoView

        SingleView v ->
            SingleView (Html.map (map fn) v)

        FieldViews fs ->
            FieldViews
                (List.map (Tuple.mapSecond (Html.map (map fn))) fs)


{-| Gets the current value of a `Control`.
-}
currentValue : Control a -> a
currentValue (Control c) =
    c.currentValue ()


{-| Renders the interactive UI for a `Control`.
-}
view : (Control a -> msg) -> Control a -> Html msg
view msg (Control c) =
    Html.div []
        [ view_ msg (Control c)
        ]


view_ : (Control a -> msg) -> Control a -> Html msg
view_ msg (Control c) =
    case c.view () of
        NoView ->
            Html.text ""

        SingleView v ->
            Html.map msg v

        FieldViews fs ->
            let
                fieldRow index ( name, fieldView ) =
                    Html.label
                        [ Html.Attributes.style "display" "table-row"
                        , Html.Attributes.style "vertical-align" "text-top"
                        ]
                        [ Html.span
                            [ Html.Attributes.style "display" "table-cell"
                            ]
                            [ Html.text
                                (if index == 0 then
                                    "{"

                                 else
                                    ","
                                )
                            ]
                        , Html.span
                            [ Html.Attributes.style "display" "table-cell"
                            , Html.Attributes.style "text-align" "right"
                            ]
                            [ Html.text name ]
                        , Html.span
                            [ Html.Attributes.style "display" "table-cell"
                            ]
                            [ Html.text " = " ]
                        , Html.div
                            [ Html.Attributes.style "display" "table-cell"
                            ]
                            [ fieldView ]
                        ]
            in
            List.concat
                [ fs
                    |> List.reverse
                    |> List.indexedMap fieldRow
                , [ Html.div
                        [ Html.Attributes.style "display" "table-row"
                        ]
                        [ Html.div
                            [ Html.Attributes.style "display" "table-cell"
                            ]
                            [ Html.text "}" ]
                        ]
                  ]
                ]
                |> Html.div
                    [ Html.Attributes.style "display" "table"
                    , Html.Attributes.style "border-spacing" "2px"
                    ]
                |> Html.map msg
