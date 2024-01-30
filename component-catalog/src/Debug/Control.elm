module Debug.Control exposing
    ( Control
    , value
    , bool, string, stringTextarea
    , float, int
    , maybe, choice, revealed
    , map
    , view, currentValue
    , lazy
    , record, list, field
    )

{-| This module is heavily based on `avh4/elm-debug-control`. It was copied in directly from that package and then modified.

Create interactive controls for arbitrary data structures.

@docs Control
@docs value
@docs bool, string, stringTextarea
@docs float, int
@docs maybe, choice, revealed
@docs map

@docs view, currentValue
@docs lazy
@docs record, list, field

-}

import Css exposing (..)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Checkbox.V7 as Checkbox
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Html.Attributes.V2 exposing (safeIdWithPrefix)
import Nri.Ui.Select.V9 as Select
import Nri.Ui.TextArea.V5 as TextArea
import Nri.Ui.TextInput.V7 as TextInput


{-| An interactive control that produces a value `a`.
-}
type Control a
    = Control
        { currentValue : () -> a
        , view : () -> ControlView a
        }


type ControlView a
    = NoView
    | SingleView (String -> List (Html (Control a)))
    | FieldViews (List ( String, String -> List (Html (Control a)) ))


{-| A `Control` that has a static value (and no UI).
-}
value : a -> Control a
value initial =
    Control
        { currentValue = \() -> initial
        , view = \() -> NoView
        }


labelId : String -> String
labelId label =
    safeIdWithPrefix "debug-control-" label


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
                    \labelText ->
                        Checkbox.view
                            { label = labelText
                            , selected = Checkbox.selectedFromBool isJust
                            }
                            [ Checkbox.id (labelId ("Maybe " ++ labelText))
                            , Checkbox.onCheck (\a -> maybe a (Control control))
                            ]
                            :: (if isJust then
                                    view_ (maybe isJust) (Control control) labelText

                                else
                                    []
                               )
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
                    \labelText ->
                        [ Checkbox.view
                            { label = labelText
                            , selected = Checkbox.selectedFromBool initialValue
                            }
                            [ Checkbox.id (labelId labelText)
                            , Checkbox.onCheck bool
                            ]
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
                    \labelText ->
                        [ TextInput.view labelText
                            [ TextInput.id (labelId labelText)
                            , TextInput.text string
                            , TextInput.value initialValue
                            ]
                        ]
        }


{-| A `Control` that allows float input.
-}
float : Float -> Control Float
float initialValue =
    Control
        { currentValue = \() -> initialValue
        , view =
            \() ->
                SingleView <|
                    \labelText ->
                        [ TextInput.view labelText
                            [ TextInput.id (labelId labelText)
                            , TextInput.float (Maybe.withDefault initialValue >> float)
                            , TextInput.value (Just initialValue)
                            ]
                        ]
        }


{-| A `Control` that allows int input.
-}
int : Int -> Control Int
int initialValue =
    Control
        { currentValue = \() -> initialValue
        , view =
            \() ->
                SingleView <|
                    \labelText ->
                        [ TextInput.view labelText
                            [ TextInput.id (labelId labelText)
                            , TextInput.number (Maybe.withDefault initialValue >> int)
                            , TextInput.value (Just initialValue)
                            ]
                        ]
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
                    \labelText ->
                        [ TextArea.view labelText
                            [ TextArea.value initialValue
                            , TextArea.onInput stringTextarea
                            ]
                        ]
        }


{-| A `Control` that chooses between a list of nested controls.

This will crash if you provide an empty list.

The first entry will be the initial value.

Often used along with `revealed` to ensure that every control has a label.

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
                    \labelText ->
                        let
                            option index ( label, _ ) =
                                { label = label, value = index }

                            choices : List (Select.Choice Int)
                            choices =
                                (List.reverse left ++ current :: right)
                                    |> List.indexedMap option

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
                        (Select.view labelText
                            [ Select.choices String.fromInt choices
                            ]
                            |> Html.map selectNew
                        )
                            :: view_ updateChild (Tuple.second current) ""
        }


{-| Attach a name to options that are revealed based on a `choice`.
-}
revealed : String -> Control a -> Control a
revealed name control =
    record identity
        |> field name control


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


{-| Use with `listItem` and `optionalListItem`

    list
        |> listItem "first name" string
        |> listItem "last name" string

-}
list : Control (List a)
list =
    record []


{-| Used with [`record`](#record) or [`list`](#list) to create a `Control`.
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
                                List.map
                                    (Tuple.mapSecond
                                        (\x label -> List.map (Html.map (field name (Control control))) (x label))
                                    )
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
            SingleView (\label -> List.map (Html.map (map fn)) (v label))

        FieldViews fs ->
            FieldViews
                (List.map (Tuple.mapSecond (\x label -> List.map (Html.map (map fn)) (x label))) fs)


{-| Gets the current value of a `Control`.
-}
currentValue : Control a -> a
currentValue (Control c) =
    c.currentValue ()


{-| Renders the interactive UI for a `Control`.
-}
view : (Control a -> msg) -> Control a -> Html msg
view msg (Control c) =
    Html.div
        [ css
            [ displayFlex
            , flexWrap wrap
            , Css.property "gap" "10px"
            ]
        ]
        (view_ msg (Control c) "")


view_ : (Control a -> msg) -> Control a -> String -> List (Html msg)
view_ msg (Control c) currentLabel =
    case c.view () of
        NoView ->
            []

        SingleView v ->
            List.map (Html.map msg) (v currentLabel)

        FieldViews fs ->
            let
                contents =
                    fs
                        |> List.reverse
                        |> List.concatMap (\( label, viewInput ) -> viewInput label)
                        |> List.map (Html.map msg)
            in
            if currentLabel /= "" then
                [ Html.fieldset
                    [ css
                        [ border3 (px 1) solid Colors.gray75
                        , borderRadius (px 3)
                        , minWidth (pct 45)
                        ]
                    ]
                    (Html.legend
                        [ css
                            [ fontSize (rem 1)
                            , color Colors.navy
                            ]
                        ]
                        [ Html.text currentLabel ]
                        :: contents
                    )
                ]

            else
                contents
