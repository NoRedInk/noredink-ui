module Gen.Accessibility.Styled.Aria exposing (activeDescendant, autoCompleteBoth, autoCompleteInline, autoCompleteList, call_, checked, colCount, colIndex, colSpan, controls, currentDate, currentItem, currentLocation, currentPage, currentStep, currentTime, describedBy, details, disabled, errorMessage, expanded, flowTo, hasDialogPopUp, hasGridPopUp, hasListBoxPopUp, hasMenuPopUp, hasTreePopUp, hidden, indeterminate, invalid, invalidGrammar, invalidSpelling, keyShortcuts, label, labeledBy, labelledBy, level, longDescription, modal, moduleName_, multiLine, multiSelectable, orientationHorizontal, orientationVertical, placeholder, posInSet, pressed, readOnly, required, roleDescription, rowCount, rowIndex, rowSpan, selected, setSize, sortAscending, sortCustom, sortDescending, sortNone, valueMax, valueMin, valueNow, valueText, values_)

{-| 
@docs moduleName_, activeDescendant, controls, label, labelledBy, labeledBy, details, describedBy, longDescription, keyShortcuts, roleDescription, flowTo, placeholder, colCount, colIndex, colSpan, rowCount, rowIndex, rowSpan, setSize, posInSet, currentItem, currentPage, currentStep, currentLocation, currentDate, currentTime, required, invalid, invalidGrammar, invalidSpelling, errorMessage, pressed, multiLine, checked, selected, indeterminate, autoCompleteInline, autoCompleteList, autoCompleteBoth, expanded, hidden, readOnly, disabled, modal, hasMenuPopUp, hasListBoxPopUp, hasTreePopUp, hasGridPopUp, hasDialogPopUp, orientationHorizontal, orientationVertical, valueMin, valueMax, valueNow, valueText, sortAscending, sortDescending, sortCustom, sortNone, multiSelectable, level, call_, values_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Accessibility", "Styled", "Aria" ]


{-| Creates an [`aria-activedescendant`](https://www.w3.org/TR/wai-aria-1.1/#aria-activedescendant) attribute.

Identifies the currently-active element in order to provide an alternative way of managing focus.

Supported in container-y roles: `application`, `composite`, `group`, `textbox`, `comboBox`, `grid`, `listBox`, `menu`, `menuBar`, `radioGroup`, `row`, `searchBox`, `select`, `spinButton`, `tabList`, `toolBar`, `tree`, and `treeGrid`.

activeDescendant: String -> Html.Styled.Attribute msg
-}
activeDescendant : String -> Elm.Expression
activeDescendant activeDescendantArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "activeDescendant"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string activeDescendantArg ]


{-| Creates [`aria-controls`](https://www.w3.org/TR/wai-aria-1.1/#aria-controls) attribute.

Pass a list of ids for the elements that are being controlled by the current element.

Supported by all elements.

controls: List String -> Html.Styled.Attribute msg
-}
controls : List String -> Elm.Expression
controls controlsArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "controls"
            , annotation =
                Just
                    (Type.function
                        [ Type.list Type.string ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list (List.map Elm.string controlsArg) ]


{-| Creates an [`aria-label`](https://www.w3.org/TR/wai-aria-1.1/#aria-label) attribute.

Supported for all elements.

label: String -> Html.Styled.Attribute msg
-}
label : String -> Elm.Expression
label labelArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "label"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string labelArg ]


{-| Creates an [`aria-labelledby`](https://www.w3.org/TR/wai-aria-1.1/#aria-labelledby) attribute.

Pass the unique string id of the labelling element. Only use this property if the label is _visible_ (if the label is _not_ visible, prefer `Accessibility.Styled.Aria.label`).

labelledBy: String -> Html.Styled.Attribute msg
-}
labelledBy : String -> Elm.Expression
labelledBy labelledByArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "labelledBy"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string labelledByArg ]


{-| Convenience alias for `labelledBy`.

labeledBy: String -> Html.Styled.Attribute msg
-}
labeledBy : String -> Elm.Expression
labeledBy labeledByArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "labeledBy"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string labeledByArg ]


{-| Supported by all elements.

Refer to a single extended description section--maybe a couple of paragraphs
and a chart. Pass in the HTML id of an element with details about the current element to create an [aria-details association](https://www.w3.org/TR/wai-aria-1.1/#aria-details).

details: String -> Html.Styled.Attribute msg
-}
details : String -> Elm.Expression
details detailsArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "details"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string detailsArg ]


{-| Supported by all elements. Pass it a list of ids of elements that describe the given element.

Pass in ids of elements that describe the current element to create an [aria-describedby association](https://www.w3.org/TR/wai-aria-1.1/#aria-describedby).

You may wish to review the documentation for `labelledBy` and `details` as well -- which property you should use will depend on your specific UX requirements.

describedBy: List String -> Html.Styled.Attribute msg
-}
describedBy : List String -> Elm.Expression
describedBy describedByArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "describedBy"
            , annotation =
                Just
                    (Type.function
                        [ Type.list Type.string ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list (List.map Elm.string describedByArg) ]


{-| Creates the longDesc attribute with the given url, which should point to a text description of the content. This attribute is only supported on img tags.

    import Accessibility exposing (Html, img)
    import Accessibility.Aria exposing (longDescription)

    view : Html msg
    view =
        img
            "Growth Chart in Some Sweet Unit (Quarter 4)"
            [ longDescription "/quarter_4_summary#Growth" ]

Note that this is a deprecated HTML property, not an ARIA property. See [MDN documentation on HTMLImageElement.longDesc
](https://developer.mozilla.org/en-US/docs/Web/API/HTMLImageElement/longDesc) for general documentation and [Using longdesc (Technique H45)](https://www.w3.org/WAI/WCAG21/Techniques/html/H45) for accessibility documentation.

longDescription: String -> Html.Styled.Attribute msg
-}
longDescription : String -> Elm.Expression
longDescription longDescriptionArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "longDescription"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string longDescriptionArg ]


{-| Supported by all elements. Pass in a list of keyboard shortcuts to use the [ aria-keyshortcuts property](https://www.w3.org/TR/wai-aria-1.1/#aria-keyshortcuts).

    keyShortcuts [ "Alt+Shift+P", "Control+F" ]

Note that this property only indicates to users that certain keyboard shortcuts _exist_ -- this property does not change the behavior of the element to which it is attached. Please also note that it's nice to make the existence of keyboard shortcuts known to all users, not only to screenreader users!

Learn more about the purpose of from The WAI-ARIA Authoring Practices guide's [Keyboard Shortcuts](https://www.w3.org/TR/wai-aria-practices-1.1/#kbd_shortcuts) section.

Please be aware that providing single-character keyboard shortcuts may make using your site _less_ accessible for some users. Read [Understanding Success Criterion 2.1.4: Character Key Shortcuts](https://www.w3.org/WAI/WCAG21/Understanding/character-key-shortcuts) to learn more.

keyShortcuts: List String -> Html.Styled.Attribute msg
-}
keyShortcuts : List String -> Elm.Expression
keyShortcuts keyShortcutsArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "keyShortcuts"
            , annotation =
                Just
                    (Type.function
                        [ Type.list Type.string ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list (List.map Elm.string keyShortcutsArg) ]


{-| Supported by all elements.

Provide human-readable description of the role of an element. Should be used
alongside an actual role--this is supplementary information.

Creates an [aria-roledescription property](https://www.w3.org/TR/wai-aria-1.1/#aria-roledescription).

roleDescription: String -> Html.Styled.Attribute msg
-}
roleDescription : String -> Elm.Expression
roleDescription roleDescriptionArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "roleDescription"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string roleDescriptionArg ]


{-| Creates an [`aria-flowto`](https://www.w3.org/TR/wai-aria-1.1/#aria-flowto) attribute.

Provide an alternative document reading order and offer navigation to the
elements referenced in the passed-in list of ids.

Supported by all elements.

flowTo: List String -> Html.Styled.Attribute msg
-}
flowTo : List String -> Elm.Expression
flowTo flowToArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "flowTo"
            , annotation =
                Just
                    (Type.function
                        [ Type.list Type.string ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list (List.map Elm.string flowToArg) ]


{-| Supported by `textbox` and `searchbox`.

Provide a hint about an expected value.

Creates an [aria-placeholder](https://www.w3.org/TR/wai-aria-1.1/#aria-placeholder) property.

Generally, you should use `Html.Styled.Attributes.placeholder` instead of using `aria-placeholder`. The only time that you should add an `aria-placeholder` attribute is if you're rolling your own widget that's required you to explicitly set a wai-aria role. See some guidance around "implicit" aria semantics [here](https://www.w3.org/TR/html-aria/).

placeholder: String -> Html.Styled.Attribute msg
-}
placeholder : String -> Elm.Expression
placeholder placeholderArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "placeholder"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string placeholderArg ]


{-| Creates an [`aria-colcount`](https://www.w3.org/TR/wai-aria-1.1/#aria-colcount) attribute.

Describe the number of columns in a grid in which not all of the columns are currently in the DOM. (If all columns are already rendering, don't use this attribute.)

`-1` indicates total column number is unknown.

Supported by elements with roles `table`, `grid`, and `treegrid`.

colCount: Int -> Html.Styled.Attribute msg
-}
colCount : Int -> Elm.Expression
colCount colCountArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "colCount"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.int colCountArg ]


{-| Creates an [`aria-colindex`](https://www.w3.org/TR/wai-aria-1.1/#aria-colindex) attribute.

Indexing begins from 1, NOT 0. Plus, the colIndex should not be greater than the `colCount`!
If a cell stretches across multiple columns, use the starting column index and `colSpan`.

The simplest rule is to put the `colIndex` on every child of a `row`.

Please note that if all of the columns of the table/grid are already present in the DOM, you do not need to use this property.

Supported by elements with roles `cell`, `row`, `columnHeader`, `gridCell`, and `rowHeader`.

colIndex: Int -> Html.Styled.Attribute msg
-}
colIndex : Int -> Elm.Expression
colIndex colIndexArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "colIndex"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.int colIndexArg ]


{-| Creates an [`aria-colspan`](https://www.w3.org/TR/wai-aria-1.1/#aria-colspan) attribute.

Indicate how many columns-wide a cell is.

Please use the [HTML attribute colspan](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/td#attr-colspan) instead of `aria-colspan` for native HTML tables.

Supported by elements with roles `cell`, `columnHeader`, `gridCell`, and `rowHeader`.

colSpan: Int -> Html.Styled.Attribute msg
-}
colSpan : Int -> Elm.Expression
colSpan colSpanArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "colSpan"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.int colSpanArg ]


{-| Creates an [`aria-rowcount`](https://www.w3.org/TR/wai-aria-1.1/#aria-rowcount) attribute.

Analagous to `colcount`.

rowCount: Int -> Html.Styled.Attribute msg
-}
rowCount : Int -> Elm.Expression
rowCount rowCountArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "rowCount"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.int rowCountArg ]


{-| Creates an [`aria-rowindex`](https://www.w3.org/TR/wai-aria-1.1/#aria-rowindex) attribute.

Analagous to `colIndex`.

rowIndex: Int -> Html.Styled.Attribute msg
-}
rowIndex : Int -> Elm.Expression
rowIndex rowIndexArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "rowIndex"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.int rowIndexArg ]


{-| Creates an [`aria-rowspan`](https://www.w3.org/TR/wai-aria-1.1/#aria-rowspan) attribute.

Analagous to `colSpan`.

rowSpan: Int -> Html.Styled.Attribute msg
-}
rowSpan : Int -> Elm.Expression
rowSpan rowSpanArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "rowSpan"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.int rowSpanArg ]


{-| Creates an [`aria-setsize`](https://www.w3.org/TR/wai-aria-1.1/#aria-setsize) attribute.

`setSize` indicates the total number of items in a set where not all the items are currently present in the DOM.

Warning! The `setSize` is added to every set _item_, not to the element containing the set.

The ARIA docs include this example, which I've converted to Elm and shorted a bit:

    import Accessibility.Styled exposing (..)
    import Accessibility.Styled.Aria as Aria
    import Accessibility.Styled.Role as Role
    import Html.Styled.Attributes exposing (id)

    view : List (Html msg)
    view =
        [ h2 [ id "label_fruit" ] [ text "Available Fruit" ]
        , ul [ Role.listBox, Aria.labelledBy "label_fruit" ]
            [ li
                [ Role.option
                , Aria.setSize 16
                , Aria.posInSet 5
                ]
                [ text "apples" ]
            , li
                [ Role.option
                , Aria.setSize 16 -- <- Note the set size appears on the element, not on the container
                , Aria.posInSet 6
                ]
                [ text "bananas" ]
            ]
        ]

Supported by elements that might appear in a list: `article`, `listItem`, `menuItem`, `option`, `radio`, `tab`, `menuitemcheckbox`, `menuitemradio`, and `treeItem`.

setSize: Int -> Html.Styled.Attribute msg
-}
setSize : Int -> Elm.Expression
setSize setSizeArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "setSize"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.int setSizeArg ]


{-| Creates an [`aria-posinset`](https://www.w3.org/TR/wai-aria-1.1/#aria-posinset) attribute.

Only necessary when not all of the items in the set are in the DOM. Use with `setSize`.

Supported by elements that might appear in a list: `article`, `listItem`, `menuItem`, `option`, `radio`, `tab`, `menuitemcheckbox`, `menuitemradio`, and `treeItem`.

posInSet: Int -> Html.Styled.Attribute msg
-}
posInSet : Int -> Elm.Expression
posInSet posInSetArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "posInSet"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.int posInSetArg ]


{-| Creates an [`aria-current`](https://www.w3.org/TR/wai-aria-1.1/#aria-current) attribute, either `aria-current=true` or `aria-current=false`.

Only 1 element in a set should be marked as the current item.

Please double-check that you don't want `Accessibility.Styled.Aria.selected` instead.

Supported by all elements.

currentItem: Bool -> Html.Styled.Attribute msg
-}
currentItem : Bool -> Elm.Expression
currentItem currentItemArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "currentItem"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.bool currentItemArg ]


{-| Creates an [`aria-current`](https://www.w3.org/TR/wai-aria-1.1/#aria-current) attribute, as `aria-current=page`.

Indicate that a link in a nav or list is the current location.

Supported by all elements.

currentPage: Html.Styled.Attribute msg
-}
currentPage : Elm.Expression
currentPage =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Aria" ]
        , name = "currentPage"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Creates an [`aria-current`](https://www.w3.org/TR/wai-aria-1.1/#aria-current) attribute, as `aria-current=step`.

Indicate which step in a step-based flow is the current one.

Supported by all elements.

currentStep: Html.Styled.Attribute msg
-}
currentStep : Elm.Expression
currentStep =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Aria" ]
        , name = "currentStep"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Supported by all elements.

currentLocation: Html.Styled.Attribute msg
-}
currentLocation : Elm.Expression
currentLocation =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Aria" ]
        , name = "currentLocation"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Creates an [`aria-current`](https://www.w3.org/TR/wai-aria-1.1/#aria-current) attribute, as `aria-current=date`.

As in a calendar.

Supported by all elements.

currentDate: Html.Styled.Attribute msg
-}
currentDate : Elm.Expression
currentDate =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Aria" ]
        , name = "currentDate"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Creates an [`aria-current`](https://www.w3.org/TR/wai-aria-1.1/#aria-current) attribute, as `aria-current=time`.

As in a timepicker.

Supported by all elements.

currentTime: Html.Styled.Attribute msg
-}
currentTime : Elm.Expression
currentTime =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Aria" ]
        , name = "currentTime"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Creates an [`aria-required`](https://www.w3.org/TR/wai-aria-1.1/#aria-required) attribute.

Indicate whether user input is or is not required on a field for valid form submission.

Supported by `comboBox`, `gridCell`, `listBox`, `radioGroup`, `spinButton`, `textBox`, `tree`

required: Bool -> Html.Styled.Attribute msg
-}
required : Bool -> Elm.Expression
required requiredArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "required"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.bool requiredArg ]


{-| Creates an [`aria-invalid`](https://www.w3.org/TR/wai-aria-1.1/#aria-invalid) attribute.

Learn more about [Using Aria-Invalid to Indicate an Error Field](https://www.w3.org/WAI/WCAG21/Techniques/aria/ARIA21).

You may want to use `Accessibility.Styled.Aria.errorMessage` or `Accessibility.Styled.Aria.describedBy` to indicate what's invalid about the user's submission.

For invalid grammar or spelling specifically, use `invalidGrammar` and `invalidSpelling` respectively.

Supported for all elements.

invalid: Bool -> Html.Styled.Attribute msg
-}
invalid : Bool -> Elm.Expression
invalid invalidArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "invalid"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.bool invalidArg ]


{-| Creates an [`aria-invalid="grammar"`](https://www.w3.org/TR/wai-aria-1.1/#aria-invalid) attribute.

Supported for all elements.

invalidGrammar: Html.Styled.Attribute msg
-}
invalidGrammar : Elm.Expression
invalidGrammar =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Aria" ]
        , name = "invalidGrammar"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Creates an [`aria-invalid="spelling"`](https://www.w3.org/TR/wai-aria-1.1/#aria-invalid) attribute.

Supported for all elements.

invalidSpelling: Html.Styled.Attribute msg
-}
invalidSpelling : Elm.Expression
invalidSpelling =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Aria" ]
        , name = "invalidSpelling"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Create an [`aria-errormessage`](https://www.w3.org/TR/wai-aria-1.1/#aria-errormessage) element.

Reference the element that has error details. e.g., if you've got an input field that's invalid, add `errorMessage` to the input with the id of whatever element is telling the user in what way their submission is wrong.

    import Accessibility.Styled
    import Accessibility.Styled.Aria as Aria
    import Html.Styled.Attributes exposing (id)

    view : List (Html msg)
    view =
        [ labelBefore [] (text "Input content:") <|
            inputText "some-content"
                [ Aria.invalid True
                , Aria.errorMessage "error-message-id"
                ]
        , span [ id "error-message-id" ] [ text "Better content required" ]
        ]

You must also use `Accessibility.Styled.Aria.invalid` when using `errorMessage`.

Supported by all elements.

errorMessage: String -> Html.Styled.Attribute msg
-}
errorMessage : String -> Elm.Expression
errorMessage errorMessageArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "errorMessage"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string errorMessageArg ]


{-| Creates an [`aria-pressed`](https://www.w3.org/TR/wai-aria-1.1/#aria-pressed) attribute.

Use `pressed` when describing a toggle button--a button that can be "toggled" between an on state and an off state (or an on state, an indeterminate state, and an off state).

Please check out these [examples](https://www.w3.org/TR/2016/WD-wai-aria-practices-1.1-20160317/examples/button/button.html) as well.

    button
        [ pressed <| Just True ]
        [ text "This button should be styled for site viewers such that it's clear it's pressed!" ]

Supported on `button`.

pressed: Maybe Bool -> Html.Styled.Attribute msg
-}
pressed : Elm.Expression -> Elm.Expression
pressed pressedArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "pressed"
            , annotation =
                Just
                    (Type.function
                        [ Type.maybe Type.bool ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ pressedArg ]


{-| Creates an [`aria-multiline`](https://www.w3.org/TR/wai-aria-1.1/#aria-multiline) attribute.

Indicate whether the `textbox` is for multi-line inputs or single-line inputs.

Be careful of default keyboard behavior when coupling this property with text inputs, which by default submit their form group on enter.

Supported for `textbox` only.

multiLine: Bool -> Html.Styled.Attribute msg
-}
multiLine : Bool -> Elm.Expression
multiLine multiLineArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "multiLine"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.bool multiLineArg ]


{-| Creates an [`aria-checked`](https://www.w3.org/TR/wai-aria-1.1/#aria-checked) attribute.

Check boxes can be checked (`Just True`), unchecked (`Just False`), or indeterminate (`Nothing`).

Available on `checkbox`, `option`, `radio`, `switch`

checked: Maybe Bool -> Html.Styled.Attribute msg
-}
checked : Elm.Expression -> Elm.Expression
checked checkedArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "checked"
            , annotation =
                Just
                    (Type.function
                        [ Type.maybe Type.bool ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ checkedArg ]


{-| Creates an [`aria-selected`](https://www.w3.org/TR/wai-aria-1.1/#aria-selected) attribute.

Indicate whether an element (in a single- or multi-selectable widget) is or is not selected.

Supported by `gridCell`, `option`, `row`, `tab`.

selected: Bool -> Html.Styled.Attribute msg
-}
selected : Bool -> Elm.Expression
selected selectedArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "selected"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.bool selectedArg ]


{-| Creates an [`aria-indeterminate`](https://www.w3.org/TR/wai-aria-1.1/#aria-indeterminate) attribute.

Sets the indeterminate value to be true.

indeterminate: Html.Styled.Attribute msg
-}
indeterminate : Elm.Expression
indeterminate =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Aria" ]
        , name = "indeterminate"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Creates an [`aria-autocomplete="inline"`](https://www.w3.org/TR/wai-aria-1.1/#aria-autocomplete) attribute.

Use when there's a suggestion for completing the field that shows up in the line that the user is completing. Be sure to indicate that the auto-completed text is selected.

Available on `comboBox` or `textbox`.

autoCompleteInline: Html.Styled.Attribute msg
-}
autoCompleteInline : Elm.Expression
autoCompleteInline =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Aria" ]
        , name = "autoCompleteInline"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Creates an [`aria-autocomplete="list"`](https://www.w3.org/TR/wai-aria-1.1/#aria-autocomplete) attribute.

Use when there's a suggestion for completing the field that shows up as a list of options from which the user can pick.

Be sure to indicate that the auto-completed text is selected.

Available on `comboBox` or `textbox`.

autoCompleteList: Html.Styled.Attribute msg
-}
autoCompleteList : Elm.Expression
autoCompleteList =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Aria" ]
        , name = "autoCompleteList"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Creates an [`aria-autocomplete="both"`](https://www.w3.org/TR/wai-aria-1.1/#aria-autocomplete) attribute.

Use when there's a suggestion for completing the field when there's both inline autocomplete and list autocomplete occurring at once.

Be sure to indicate that the auto-completed text is selected.

Available on `comboBox` or `textbox`.

autoCompleteBoth: Html.Styled.Attribute msg
-}
autoCompleteBoth : Elm.Expression
autoCompleteBoth =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Aria" ]
        , name = "autoCompleteBoth"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Creates an [`aria-expanded`](https://www.w3.org/TR/wai-aria-1.1/#aria-expanded) attribute.

This attribute can be applied to either an element that is itself expanded/collapsed, OR to an elment it controls that is either expanded/collapsed. In the latter case, use a `controls` attribute as well to clarify the relationship.

Available on `button`, `comboBox`, `document`, `link`, `section`, `sectionHead`, and `window`.

expanded: Bool -> Html.Styled.Attribute msg
-}
expanded : Bool -> Elm.Expression
expanded expandedArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "expanded"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.bool expandedArg ]


{-| Creates an [`aria-hidden`](https://www.w3.org/TR/wai-aria-1.1/#aria-hidden) attribute.

hidden: Bool -> Html.Styled.Attribute msg
-}
hidden : Bool -> Elm.Expression
hidden hiddenArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "hidden"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.bool hiddenArg ]


{-| Creates an [`aria-readonly`](https://www.w3.org/TR/wai-aria-1.1/#aria-readonly) attribute.

Indicates read-only status of a widget, although normal navigation rules and copying behavior should apply. (Read: `readOnly` elements are navigable but unchangeable, and `disabled` elements are neither navigable nor unchangebale).

Supported on `checkBox`, `comboBox`, `grid`, `gridCell`, `listBox`, `radioGroup`, `slider`, `spinButton`, and `textBox`.

readOnly: Bool -> Html.Styled.Attribute msg
-}
readOnly : Bool -> Elm.Expression
readOnly readOnlyArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "readOnly"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.bool readOnlyArg ]


{-| Creates an [`aria-disabled`](https://www.w3.org/TR/wai-aria-1.1/#aria-disabled) attribute.

In deciding whether to use `Accessibility.Styled.Aria.disabled` or `Html.Styled.Attributes.disabled`, it may be helpful to read through the [Focusablity of disabled controls](https://www.w3.org/TR/wai-aria-practices-1.1/#kbd_disabled_controls) section of the WAI-ARIA Practices recommendations.

In essence, you may want to use `Accessibility.Styled.Aria.disabled` instead of `Html.Styled.Attributes.disabled` if you want users to be aware of disabled elements, and you don't mind that users will need to navigate through these disabled elements.

Supported for all elements. Elements are not disabled (they are enabled) by default.

disabled: Bool -> Html.Styled.Attribute msg
-}
disabled : Bool -> Elm.Expression
disabled disabledArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "disabled"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.bool disabledArg ]


{-| Creates an [`aria-modal`](https://www.w3.org/TR/wai-aria-1.1/#aria-modal) attribute.

Indicate that a modal is showing and the rest of the page contents are not interactable.

    import Accessibility exposing (div, h2, p, text)
    import Accessibility.Role exposing (dialog)
    import Accessibility.Styled.Aria exposing (labelledBy, modal)
    import Html.Attributes exposing (id)

    modal : Html msg
    modal =
        div [ dialog, modal, labelledBy "header-id" ]
            [ h2 [ id "header-id" ] [ text "Modal Header" ]
            , p [] [ text "Wow, such modal contents!" ]
            ]

modal: Bool -> Html.Styled.Attribute msg
-}
modal : Bool -> Elm.Expression
modal modalArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "modal"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.bool modalArg ]


{-| Creates an [`aria-haspopup="menu"`](https://www.w3.org/TR/wai-aria-1.1/#aria-haspopup) attribute.

Indicate that interaction with this element can trigger a `menu` pop-up.

Be careful while managing focus and triggering.

hasMenuPopUp: Html.Styled.Attribute msg
-}
hasMenuPopUp : Elm.Expression
hasMenuPopUp =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Aria" ]
        , name = "hasMenuPopUp"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Creates an [`aria-haspopup="listbox"`](https://www.w3.org/TR/wai-aria-1.1/#aria-haspopup) attribute.

Indicate that interaction with this element can trigger a `listBox` pop-up.

Be careful while managing focus and triggering.

hasListBoxPopUp: Html.Styled.Attribute msg
-}
hasListBoxPopUp : Elm.Expression
hasListBoxPopUp =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Aria" ]
        , name = "hasListBoxPopUp"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Creates an [`aria-haspopup="tree"`](https://www.w3.org/TR/wai-aria-1.1/#aria-haspopup) attribute.

Indicate that interaction with this element can trigger a `tree` pop-up.

Be careful while managing focus and triggering.

hasTreePopUp: Html.Styled.Attribute msg
-}
hasTreePopUp : Elm.Expression
hasTreePopUp =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Aria" ]
        , name = "hasTreePopUp"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Creates an [`aria-haspopup="grid"`](https://www.w3.org/TR/wai-aria-1.1/#aria-haspopup) attribute.

Indicate that interaction with this element can trigger a `grid` pop-up.

Be careful while managing focus and triggering.

hasGridPopUp: Html.Styled.Attribute msg
-}
hasGridPopUp : Elm.Expression
hasGridPopUp =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Aria" ]
        , name = "hasGridPopUp"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Creates an [`aria-haspopup="dialog"`](https://www.w3.org/TR/wai-aria-1.1/#aria-haspopup) attribute.

Indicate that interaction with this element can trigger a `dialog` pop-up.

Be careful while managing focus and triggering.

hasDialogPopUp: Html.Styled.Attribute msg
-}
hasDialogPopUp : Elm.Expression
hasDialogPopUp =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Aria" ]
        , name = "hasDialogPopUp"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Creates an [`aria-orientation="horizontal`](https://www.w3.org/TR/wai-aria-1.1/#aria-orientation) attribute.

Supported on roles with some sense of inherent orientation: `progressBar`, `scrollbar`, `select`, `separator`, `slider`, `tabList`, `toolBar`

Careful: default behavior is inconsistent across those roles.

orientationHorizontal: Html.Styled.Attribute msg
-}
orientationHorizontal : Elm.Expression
orientationHorizontal =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Aria" ]
        , name = "orientationHorizontal"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Creates an [`aria-orientation="vertical"`](https://www.w3.org/TR/wai-aria-1.1/#aria-orientation="vertical") attribute.

Supported on roles with some sense of inherent orientation: `progressBar`, `scrollbar`, `select`, `separator`, `slider`, `tabList`, `toolBar`

Careful: default behavior is inconsistent across those roles.

orientationVertical: Html.Styled.Attribute msg
-}
orientationVertical : Elm.Expression
orientationVertical =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Aria" ]
        , name = "orientationVertical"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Creates an [`aria-valuemin`](https://www.w3.org/TR/wai-aria-1.1/#aria-valuemin) attribute.

Set the min allowed value for a range widget.

Supported by `progressBar`, `scrollbar`, `separator`, `slider`, and `spinButton`.

valueMin: Float -> Html.Styled.Attribute msg
-}
valueMin : Float -> Elm.Expression
valueMin valueMinArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "valueMin"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.float valueMinArg ]


{-| Creates an [`aria-valuemax`](https://www.w3.org/TR/wai-aria-1.1/#aria-valuemax) attribute.

Set the max allowed value for a range widget.

Supported by `progressBar`, `scrollbar`, `separator`, `slider`, and `spinButton`.

valueMax: Float -> Html.Styled.Attribute msg
-}
valueMax : Float -> Elm.Expression
valueMax valueMaxArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "valueMax"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.float valueMaxArg ]


{-| Creates an [`aria-valuenow`](https://www.w3.org/TR/wai-aria-1.1/#aria-valuenow) attribute.

Set the current value for a range widget. Don't use this property for indeterminate states.

Supported by `progressBar`, `scrollbar`, `separator`, `slider`, and `spinButton`.

valueNow: Float -> Html.Styled.Attribute msg
-}
valueNow : Float -> Elm.Expression
valueNow valueNowArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "valueNow"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.float valueNowArg ]


{-| Creates an [`aria-valuetext`](https://www.w3.org/TR/wai-aria-1.1/#aria-valuetext) attribute.

This property takes precedence over `valueNow`, and should show a human-readable version of the current value. However, `valueNow` should always be used.

Supported by `progressBar`, `scrollbar`, `separator`, `slider`, and `spinButton`.

valueText: String -> Html.Styled.Attribute msg
-}
valueText : String -> Elm.Expression
valueText valueTextArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "valueText"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string valueTextArg ]


{-| Creates an [`aria-sort="ascending"`](https://www.w3.org/TR/wai-aria-1.1/#aria-sort) attribute.

Table is sorted by this column's values in ascending order. Only one column in a table should be sorting the values in table.

Supported by `columnHeader` and `rowHeader`, but only where those roles are used on table or grid headers.

sortAscending: Html.Styled.Attribute msg
-}
sortAscending : Elm.Expression
sortAscending =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Aria" ]
        , name = "sortAscending"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Creates an [`aria-sort="descending"`](https://www.w3.org/TR/wai-aria-1.1/#aria-sort) attribute.

Table is sorted by this column's values in descending order. Only one column in a table should be sorting the values in table.

Supported by `columnHeader` and `rowHeader`, but only where those roles are used on table or grid headers.

sortDescending: Html.Styled.Attribute msg
-}
sortDescending : Elm.Expression
sortDescending =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Aria" ]
        , name = "sortDescending"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Creates an [`aria-sort="other"`](https://www.w3.org/TR/wai-aria-1.1/#aria-sort) attribute.

Table is sorted by this column's values, but the algorithm for that sorting is custom (not ascending or descending). Only one column in a table should be sorting the values in table.

Supported by `columnHeader` and `rowHeader`, but only where those roles are used on table or grid headers.

sortCustom: Html.Styled.Attribute msg
-}
sortCustom : Elm.Expression
sortCustom =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Aria" ]
        , name = "sortCustom"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Creates an [`aria-sort="none"`](https://www.w3.org/TR/wai-aria-1.1/#aria-sort) attribute.

Table is not sorted by this column's values.

Supported by `columnHeader` and `rowHeader`, but only where those roles are used on table or grid headers.

sortNone: Html.Styled.Attribute msg
-}
sortNone : Elm.Expression
sortNone =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Aria" ]
        , name = "sortNone"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Creates an [`aria-multiselectable`](https://www.w3.org/TR/wai-aria-1.1/#aria-multiselectable) attribute.

When true, users are not restricted to selecting only one selectable descendant at a time.

Supported on `grid`, `listBox`, `tabList`, `tree`.

multiSelectable: Bool -> Html.Styled.Attribute msg
-}
multiSelectable : Bool -> Elm.Expression
multiSelectable multiSelectableArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "multiSelectable"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.bool multiSelectableArg ]


{-| Creates an [`aria-level`](https://www.w3.org/TR/wai-aria-1.1/#aria-level) attribute.

This attribute is about hierarchy--how many "levels" deep is an element?

    h7 attributes =
        div (heading :: level 7 :: attributes)

Supported for `grid`, `heading`, `listItem`, `row`, and `tabList`.

level: Int -> Html.Styled.Attribute msg
-}
level : Int -> Elm.Expression
level levelArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "level"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.int levelArg ]


call_ :
    { activeDescendant : Elm.Expression -> Elm.Expression
    , controls : Elm.Expression -> Elm.Expression
    , label : Elm.Expression -> Elm.Expression
    , labelledBy : Elm.Expression -> Elm.Expression
    , labeledBy : Elm.Expression -> Elm.Expression
    , details : Elm.Expression -> Elm.Expression
    , describedBy : Elm.Expression -> Elm.Expression
    , longDescription : Elm.Expression -> Elm.Expression
    , keyShortcuts : Elm.Expression -> Elm.Expression
    , roleDescription : Elm.Expression -> Elm.Expression
    , flowTo : Elm.Expression -> Elm.Expression
    , placeholder : Elm.Expression -> Elm.Expression
    , colCount : Elm.Expression -> Elm.Expression
    , colIndex : Elm.Expression -> Elm.Expression
    , colSpan : Elm.Expression -> Elm.Expression
    , rowCount : Elm.Expression -> Elm.Expression
    , rowIndex : Elm.Expression -> Elm.Expression
    , rowSpan : Elm.Expression -> Elm.Expression
    , setSize : Elm.Expression -> Elm.Expression
    , posInSet : Elm.Expression -> Elm.Expression
    , currentItem : Elm.Expression -> Elm.Expression
    , required : Elm.Expression -> Elm.Expression
    , invalid : Elm.Expression -> Elm.Expression
    , errorMessage : Elm.Expression -> Elm.Expression
    , pressed : Elm.Expression -> Elm.Expression
    , multiLine : Elm.Expression -> Elm.Expression
    , checked : Elm.Expression -> Elm.Expression
    , selected : Elm.Expression -> Elm.Expression
    , expanded : Elm.Expression -> Elm.Expression
    , hidden : Elm.Expression -> Elm.Expression
    , readOnly : Elm.Expression -> Elm.Expression
    , disabled : Elm.Expression -> Elm.Expression
    , modal : Elm.Expression -> Elm.Expression
    , valueMin : Elm.Expression -> Elm.Expression
    , valueMax : Elm.Expression -> Elm.Expression
    , valueNow : Elm.Expression -> Elm.Expression
    , valueText : Elm.Expression -> Elm.Expression
    , multiSelectable : Elm.Expression -> Elm.Expression
    , level : Elm.Expression -> Elm.Expression
    }
call_ =
    { activeDescendant =
        \activeDescendantArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled", "Aria" ]
                    , name = "activeDescendant"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ activeDescendantArg ]
    , controls =
        \controlsArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled", "Aria" ]
                    , name = "controls"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list Type.string ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ controlsArg ]
    , label =
        \labelArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled", "Aria" ]
                    , name = "label"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ labelArg ]
    , labelledBy =
        \labelledByArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled", "Aria" ]
                    , name = "labelledBy"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ labelledByArg ]
    , labeledBy =
        \labeledByArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled", "Aria" ]
                    , name = "labeledBy"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ labeledByArg ]
    , details =
        \detailsArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled", "Aria" ]
                    , name = "details"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ detailsArg ]
    , describedBy =
        \describedByArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled", "Aria" ]
                    , name = "describedBy"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list Type.string ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ describedByArg ]
    , longDescription =
        \longDescriptionArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled", "Aria" ]
                    , name = "longDescription"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ longDescriptionArg ]
    , keyShortcuts =
        \keyShortcutsArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled", "Aria" ]
                    , name = "keyShortcuts"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list Type.string ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ keyShortcutsArg ]
    , roleDescription =
        \roleDescriptionArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled", "Aria" ]
                    , name = "roleDescription"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ roleDescriptionArg ]
    , flowTo =
        \flowToArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled", "Aria" ]
                    , name = "flowTo"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list Type.string ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ flowToArg ]
    , placeholder =
        \placeholderArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled", "Aria" ]
                    , name = "placeholder"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ placeholderArg ]
    , colCount =
        \colCountArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled", "Aria" ]
                    , name = "colCount"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ colCountArg ]
    , colIndex =
        \colIndexArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled", "Aria" ]
                    , name = "colIndex"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ colIndexArg ]
    , colSpan =
        \colSpanArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled", "Aria" ]
                    , name = "colSpan"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ colSpanArg ]
    , rowCount =
        \rowCountArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled", "Aria" ]
                    , name = "rowCount"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ rowCountArg ]
    , rowIndex =
        \rowIndexArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled", "Aria" ]
                    , name = "rowIndex"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ rowIndexArg ]
    , rowSpan =
        \rowSpanArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled", "Aria" ]
                    , name = "rowSpan"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ rowSpanArg ]
    , setSize =
        \setSizeArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled", "Aria" ]
                    , name = "setSize"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ setSizeArg ]
    , posInSet =
        \posInSetArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled", "Aria" ]
                    , name = "posInSet"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ posInSetArg ]
    , currentItem =
        \currentItemArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled", "Aria" ]
                    , name = "currentItem"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.bool ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ currentItemArg ]
    , required =
        \requiredArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled", "Aria" ]
                    , name = "required"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.bool ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ requiredArg ]
    , invalid =
        \invalidArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled", "Aria" ]
                    , name = "invalid"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.bool ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ invalidArg ]
    , errorMessage =
        \errorMessageArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled", "Aria" ]
                    , name = "errorMessage"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ errorMessageArg ]
    , pressed =
        \pressedArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled", "Aria" ]
                    , name = "pressed"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.maybe Type.bool ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ pressedArg ]
    , multiLine =
        \multiLineArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled", "Aria" ]
                    , name = "multiLine"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.bool ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ multiLineArg ]
    , checked =
        \checkedArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled", "Aria" ]
                    , name = "checked"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.maybe Type.bool ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ checkedArg ]
    , selected =
        \selectedArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled", "Aria" ]
                    , name = "selected"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.bool ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ selectedArg ]
    , expanded =
        \expandedArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled", "Aria" ]
                    , name = "expanded"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.bool ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ expandedArg ]
    , hidden =
        \hiddenArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled", "Aria" ]
                    , name = "hidden"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.bool ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ hiddenArg ]
    , readOnly =
        \readOnlyArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled", "Aria" ]
                    , name = "readOnly"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.bool ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ readOnlyArg ]
    , disabled =
        \disabledArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled", "Aria" ]
                    , name = "disabled"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.bool ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ disabledArg ]
    , modal =
        \modalArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled", "Aria" ]
                    , name = "modal"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.bool ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ modalArg ]
    , valueMin =
        \valueMinArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled", "Aria" ]
                    , name = "valueMin"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ valueMinArg ]
    , valueMax =
        \valueMaxArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled", "Aria" ]
                    , name = "valueMax"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ valueMaxArg ]
    , valueNow =
        \valueNowArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled", "Aria" ]
                    , name = "valueNow"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ valueNowArg ]
    , valueText =
        \valueTextArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled", "Aria" ]
                    , name = "valueText"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ valueTextArg ]
    , multiSelectable =
        \multiSelectableArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled", "Aria" ]
                    , name = "multiSelectable"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.bool ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ multiSelectableArg ]
    , level =
        \levelArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled", "Aria" ]
                    , name = "level"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ levelArg ]
    }


values_ :
    { activeDescendant : Elm.Expression
    , controls : Elm.Expression
    , label : Elm.Expression
    , labelledBy : Elm.Expression
    , labeledBy : Elm.Expression
    , details : Elm.Expression
    , describedBy : Elm.Expression
    , longDescription : Elm.Expression
    , keyShortcuts : Elm.Expression
    , roleDescription : Elm.Expression
    , flowTo : Elm.Expression
    , placeholder : Elm.Expression
    , colCount : Elm.Expression
    , colIndex : Elm.Expression
    , colSpan : Elm.Expression
    , rowCount : Elm.Expression
    , rowIndex : Elm.Expression
    , rowSpan : Elm.Expression
    , setSize : Elm.Expression
    , posInSet : Elm.Expression
    , currentItem : Elm.Expression
    , currentPage : Elm.Expression
    , currentStep : Elm.Expression
    , currentLocation : Elm.Expression
    , currentDate : Elm.Expression
    , currentTime : Elm.Expression
    , required : Elm.Expression
    , invalid : Elm.Expression
    , invalidGrammar : Elm.Expression
    , invalidSpelling : Elm.Expression
    , errorMessage : Elm.Expression
    , pressed : Elm.Expression
    , multiLine : Elm.Expression
    , checked : Elm.Expression
    , selected : Elm.Expression
    , indeterminate : Elm.Expression
    , autoCompleteInline : Elm.Expression
    , autoCompleteList : Elm.Expression
    , autoCompleteBoth : Elm.Expression
    , expanded : Elm.Expression
    , hidden : Elm.Expression
    , readOnly : Elm.Expression
    , disabled : Elm.Expression
    , modal : Elm.Expression
    , hasMenuPopUp : Elm.Expression
    , hasListBoxPopUp : Elm.Expression
    , hasTreePopUp : Elm.Expression
    , hasGridPopUp : Elm.Expression
    , hasDialogPopUp : Elm.Expression
    , orientationHorizontal : Elm.Expression
    , orientationVertical : Elm.Expression
    , valueMin : Elm.Expression
    , valueMax : Elm.Expression
    , valueNow : Elm.Expression
    , valueText : Elm.Expression
    , sortAscending : Elm.Expression
    , sortDescending : Elm.Expression
    , sortCustom : Elm.Expression
    , sortNone : Elm.Expression
    , multiSelectable : Elm.Expression
    , level : Elm.Expression
    }
values_ =
    { activeDescendant =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "activeDescendant"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , controls =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "controls"
            , annotation =
                Just
                    (Type.function
                        [ Type.list Type.string ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , label =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "label"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , labelledBy =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "labelledBy"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , labeledBy =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "labeledBy"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , details =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "details"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , describedBy =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "describedBy"
            , annotation =
                Just
                    (Type.function
                        [ Type.list Type.string ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , longDescription =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "longDescription"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , keyShortcuts =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "keyShortcuts"
            , annotation =
                Just
                    (Type.function
                        [ Type.list Type.string ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , roleDescription =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "roleDescription"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , flowTo =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "flowTo"
            , annotation =
                Just
                    (Type.function
                        [ Type.list Type.string ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , placeholder =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "placeholder"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , colCount =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "colCount"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , colIndex =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "colIndex"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , colSpan =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "colSpan"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , rowCount =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "rowCount"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , rowIndex =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "rowIndex"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , rowSpan =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "rowSpan"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , setSize =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "setSize"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , posInSet =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "posInSet"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , currentItem =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "currentItem"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , currentPage =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "currentPage"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , currentStep =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "currentStep"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , currentLocation =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "currentLocation"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , currentDate =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "currentDate"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , currentTime =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "currentTime"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , required =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "required"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , invalid =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "invalid"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , invalidGrammar =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "invalidGrammar"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , invalidSpelling =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "invalidSpelling"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , errorMessage =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "errorMessage"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , pressed =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "pressed"
            , annotation =
                Just
                    (Type.function
                        [ Type.maybe Type.bool ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , multiLine =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "multiLine"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , checked =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "checked"
            , annotation =
                Just
                    (Type.function
                        [ Type.maybe Type.bool ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , selected =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "selected"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , indeterminate =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "indeterminate"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , autoCompleteInline =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "autoCompleteInline"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , autoCompleteList =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "autoCompleteList"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , autoCompleteBoth =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "autoCompleteBoth"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , expanded =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "expanded"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , hidden =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "hidden"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , readOnly =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "readOnly"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , disabled =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "disabled"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , modal =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "modal"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , hasMenuPopUp =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "hasMenuPopUp"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , hasListBoxPopUp =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "hasListBoxPopUp"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , hasTreePopUp =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "hasTreePopUp"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , hasGridPopUp =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "hasGridPopUp"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , hasDialogPopUp =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "hasDialogPopUp"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , orientationHorizontal =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "orientationHorizontal"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , orientationVertical =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "orientationVertical"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , valueMin =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "valueMin"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , valueMax =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "valueMax"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , valueNow =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "valueNow"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , valueText =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "valueText"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , sortAscending =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "sortAscending"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , sortDescending =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "sortDescending"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , sortCustom =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "sortCustom"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , sortNone =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "sortNone"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , multiSelectable =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "multiSelectable"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , level =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Aria" ]
            , name = "level"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    }


