module Gen.Accessibility.Styled exposing (a, abbr, address, annotation_, article, aside, audio, b, bdi, bdo, blockquote, br, button, call_, canvas, caption, checkbox, cite, code, col, colgroup, datalist, dd, decorativeImg, del, details, dfn, div, dl, dt, em, embed, fieldset, figcaption, figure, footer, form, formWithListeners, fromUnstyled, h1, h2, h3, h4, h5, h6, header, hr, i, iframe, img, inputNumber, inputText, ins, kbd, label, labelAfter, labelBefore, labelHidden, legend, li, main_, map, mark, math, menu, menuitem, meter, moduleName_, nav, object, ol, optgroup, option, output, p, param, pre, progress, q, radio, rp, rt, ruby, s, samp, section, select, small, source, span, strong, styled, sub, summary, sup, tab, tabList, tabPanel, table, tbody, td, text, textarea, tfoot, th, thead, time, toUnstyled, tr, track, u, ul, values_, var, video, wbr)

{-| 
@docs moduleName_, labelBefore, labelAfter, labelHidden, inputText, inputNumber, radio, checkbox, tabList, tab, tabPanel, img, decorativeImg, button, textarea, select, text, h1, h2, h3, h4, h5, h6, div, p, hr, pre, blockquote, span, a, code, em, strong, i, b, u, sub, sup, br, ol, ul, li, dl, dt, dd, iframe, canvas, math, form, formWithListeners, option, section, nav, article, aside, header, footer, address, main_, figure, figcaption, table, caption, colgroup, col, tbody, thead, tfoot, tr, td, th, fieldset, legend, label, datalist, optgroup, output, progress, meter, audio, video, source, track, embed, object, param, ins, del, small, cite, dfn, abbr, time, var, samp, kbd, s, q, mark, ruby, rt, rp, bdi, bdo, wbr, details, summary, menuitem, menu, map, fromUnstyled, toUnstyled, styled, annotation_, call_, values_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Accessibility", "Styled" ]


{-| All inputs must be associated with a `label`.

    labelBefore [] viewLabel viewInput

labelBefore: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> Accessibility.Styled.Html Basics.Never
    -> Accessibility.Styled.Html msg
    -> Accessibility.Styled.Html msg
-}
labelBefore :
    List Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
labelBefore labelBeforeArg labelBeforeArg0 labelBeforeArg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "labelBefore"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.namedWith [ "Basics" ] "Never" [] ]
                        , Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list labelBeforeArg, labelBeforeArg0, labelBeforeArg1 ]


{-| All inputs must be associated with a `label`.

    labelAfter [] viewLabel viewInput

labelAfter: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> Accessibility.Styled.Html Basics.Never
    -> Accessibility.Styled.Html msg
    -> Accessibility.Styled.Html msg
-}
labelAfter :
    List Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
labelAfter labelAfterArg labelAfterArg0 labelAfterArg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "labelAfter"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.namedWith [ "Basics" ] "Never" [] ]
                        , Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list labelAfterArg, labelAfterArg0, labelAfterArg1 ]


{-| All inputs must be associated with a `label`.

    labelHidden "id-of-input" [] viewLabel viewInput

The id that's passed in must be added to the input!

labelHidden: 
    String
    -> List (Accessibility.Styled.Attribute Basics.Never)
    -> Accessibility.Styled.Html Basics.Never
    -> Accessibility.Styled.Html msg
    -> Accessibility.Styled.Html msg
-}
labelHidden :
    String
    -> List Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
labelHidden labelHiddenArg labelHiddenArg0 labelHiddenArg1 labelHiddenArg2 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "labelHidden"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.namedWith [ "Basics" ] "Never" [] ]
                        , Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string labelHiddenArg
        , Elm.list labelHiddenArg0
        , labelHiddenArg1
        , labelHiddenArg2
        ]


{-| Constructs an input of type `text`. Use in conjunction with one of the label helpers (`labelBefore`, `labelAfter`, `labelHidden`).

    inputText "the value of the input" [ property "autocomplete" "given-name" ]

Use the HTML autocomplete attribute whenever possible. Read [Understanding Success Criterion 1.3.5: Identify Input Purpose](https://www.w3.org/WAI/WCAG21/Understanding/identify-input-purpose) and [Using HTML 5.2 autocomplete attributes (Technique H98)](https://www.w3.org/WAI/WCAG21/Techniques/html/H98) for more information.

You might notice that `Html.Styled.Attributes` and `Html.Attributes` don't provide full autocomplete support. This is tracked in [elm/html issue 189](https://github.com/elm/html/issues/189).

inputText: 
    String
    -> List (Accessibility.Styled.Attribute msg)
    -> Accessibility.Styled.Html msg
-}
inputText : String -> List Elm.Expression -> Elm.Expression
inputText inputTextArg inputTextArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "inputText"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string inputTextArg, Elm.list inputTextArg0 ]


{-| Constructs an input of type "text" but constricting the input to allow only numbers as recommended by [gov.uk](https://technology.blog.gov.uk/2020/02/24/why-the-gov-uk-design-system-team-changed-the-input-type-for-numbers/). Use in conjunction with one of the label helpers (`labelBefore`, `labelAfter`, `labelHidden`).

    inputNumber 1950 [ property "autocomplete" "bday-year" ]

Use the HTML autocomplete attribute whenever possible. Read [Understanding Success Criterion 1.3.5: Identify Input Purpose](https://www.w3.org/WAI/WCAG21/Understanding/identify-input-purpose) and [Using HTML 5.2 autocomplete attributes (Technique H98)](https://www.w3.org/WAI/WCAG21/Techniques/html/H98) for more information.

You might notice that `Html.Styled.Attributes` and `Html.Attributes` don't provide full autocomplete support. This is tracked in [elm/html issue 189](https://github.com/elm/html/issues/189).

inputNumber: 
    String
    -> List (Accessibility.Styled.Attribute msg)
    -> Accessibility.Styled.Html msg
-}
inputNumber : String -> List Elm.Expression -> Elm.Expression
inputNumber inputNumberArg inputNumberArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "inputNumber"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string inputNumberArg, Elm.list inputNumberArg0 ]


{-| Constructs an input of type "radio". Use in conjunction with one of the label helpers (`labelBefore`, `labelAfter`, `labelHidden`).

    radio "radio-group-name" "Radio input value" True []

radio: 
    String
    -> String
    -> Bool
    -> List (Accessibility.Styled.Attribute msg)
    -> Accessibility.Styled.Html msg
-}
radio : String -> String -> Bool -> List Elm.Expression -> Elm.Expression
radio radioArg radioArg0 radioArg1 radioArg2 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "radio"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.string
                        , Type.bool
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string radioArg
        , Elm.string radioArg0
        , Elm.bool radioArg1
        , Elm.list radioArg2
        ]


{-| Constructs an input of type "checkbox". Use in conjunction with one of the label helpers (`labelBefore`, `labelAfter`, `labelHidden`).

Checkboxes may be checked (`Just True`), unchecked (`Just False`), or indeterminate (`Nothing`).

    checkbox "Checkbox value" Nothing []

checkbox: 
    String
    -> Maybe Bool
    -> List (Accessibility.Styled.Attribute msg)
    -> Accessibility.Styled.Html msg
-}
checkbox : String -> Elm.Expression -> List Elm.Expression -> Elm.Expression
checkbox checkboxArg checkboxArg0 checkboxArg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "checkbox"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.maybe Type.bool
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string checkboxArg, checkboxArg0, Elm.list checkboxArg1 ]


{-| Create a tablist. This is the outer container for a list of tabs.

tabList: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
tabList : List Elm.Expression -> List Elm.Expression -> Elm.Expression
tabList tabListArg tabListArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "tabList"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list tabListArg, Elm.list tabListArg0 ]


{-| Create a tab. This is the part that you select in order to change panel views.

You'll want to listen for click events **and** for keyboard events: when users hit
the right and left keys on their keyboards, they expect for the selected tab to change.

tab: 
    List (Accessibility.Styled.Attribute msg)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
tab : List Elm.Expression -> List Elm.Expression -> Elm.Expression
tab tabArg tabArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "tab"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list tabArg, Elm.list tabArg0 ]


{-| Create a tab panel.

tabPanel: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
tabPanel : List Elm.Expression -> List Elm.Expression -> Elm.Expression
tabPanel tabPanelArg tabPanelArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "tabPanel"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list tabPanelArg, Elm.list tabPanelArg0 ]


{-| Use this tag when the image provides information not expressed in the text of the page. When images are used to express the purpose of a button or link, aim for alternative text that encapsulates the function of the image.

Read through [the w3 informative image tutorial](https://www.w3.org/WAI/tutorials/images/informative/) and the [the w3 functional image tutorial](https://www.w3.org/WAI/tutorials/images/functional/) to learn more.

For graphs and diagrams, see `figure` and `longDesc`.

    img "Bear rubbing back on tree" [ src "bear.png" ]

img: 
    String
    -> List (Accessibility.Styled.Attribute Basics.Never)
    -> Accessibility.Styled.Html msg
-}
img : String -> List Elm.Expression -> Elm.Expression
img imgArg imgArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "img"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string imgArg, Elm.list imgArg0 ]


{-| Use this tag when the image is decorative or provides redundant information. Read through [the w3 decorative image tutorial](https://www.w3.org/WAI/tutorials/images/decorative/) to learn more.

    decorativeImg [ src "smiling_family.jpg" ]

decorativeImg: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> Accessibility.Styled.Html msg
-}
decorativeImg : List Elm.Expression -> Elm.Expression
decorativeImg decorativeImgArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "decorativeImg"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list decorativeImgArg ]


{-| button: 
    List (Accessibility.Styled.Attribute msg)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
button : List Elm.Expression -> List Elm.Expression -> Elm.Expression
button buttonArg buttonArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "button"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list buttonArg, Elm.list buttonArg0 ]


{-| textarea: 
    List (Accessibility.Styled.Attribute msg)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
textarea : List Elm.Expression -> List Elm.Expression -> Elm.Expression
textarea textareaArg textareaArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "textarea"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list textareaArg, Elm.list textareaArg0 ]


{-| select: 
    List (Accessibility.Styled.Attribute msg)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
select : List Elm.Expression -> List Elm.Expression -> Elm.Expression
select selectArg selectArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "select"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list selectArg, Elm.list selectArg0 ]


{-| text: String -> Html.Styled.Html msg -}
text : String -> Elm.Expression
text textArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "text"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string textArg ]


{-| `h1` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

h1: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
h1 : List Elm.Expression -> List Elm.Expression -> Elm.Expression
h1 h1Arg h1Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "h1"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list h1Arg, Elm.list h1Arg0 ]


{-| `h2` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

h2: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
h2 : List Elm.Expression -> List Elm.Expression -> Elm.Expression
h2 h2Arg h2Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "h2"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list h2Arg, Elm.list h2Arg0 ]


{-| `h3` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

h3: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
h3 : List Elm.Expression -> List Elm.Expression -> Elm.Expression
h3 h3Arg h3Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "h3"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list h3Arg, Elm.list h3Arg0 ]


{-| `h4` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

h4: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
h4 : List Elm.Expression -> List Elm.Expression -> Elm.Expression
h4 h4Arg h4Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "h4"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list h4Arg, Elm.list h4Arg0 ]


{-| `h5` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

h5: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
h5 : List Elm.Expression -> List Elm.Expression -> Elm.Expression
h5 h5Arg h5Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "h5"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list h5Arg, Elm.list h5Arg0 ]


{-| `h6` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

h6: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
h6 : List Elm.Expression -> List Elm.Expression -> Elm.Expression
h6 h6Arg h6Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "h6"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list h6Arg, Elm.list h6Arg0 ]


{-| `div` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

div: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
div : List Elm.Expression -> List Elm.Expression -> Elm.Expression
div divArg divArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "div"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list divArg, Elm.list divArg0 ]


{-| `p` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

p: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
p : List Elm.Expression -> List Elm.Expression -> Elm.Expression
p pArg pArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "p"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list pArg, Elm.list pArg0 ]


{-| `hr` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

hr: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
hr : List Elm.Expression -> List Elm.Expression -> Elm.Expression
hr hrArg hrArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "hr"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list hrArg, Elm.list hrArg0 ]


{-| `pre` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

pre: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
pre : List Elm.Expression -> List Elm.Expression -> Elm.Expression
pre preArg preArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "pre"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list preArg, Elm.list preArg0 ]


{-| `blockquote` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

blockquote: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
blockquote : List Elm.Expression -> List Elm.Expression -> Elm.Expression
blockquote blockquoteArg blockquoteArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "blockquote"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list blockquoteArg, Elm.list blockquoteArg0 ]


{-| `span` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

span: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
span : List Elm.Expression -> List Elm.Expression -> Elm.Expression
span spanArg spanArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "span"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list spanArg, Elm.list spanArg0 ]


{-| Read [Understanding Success Criterion 2.4.9: Link Purpose (Link Only)](https://www.w3.org/WAI/WCAG21/Understanding/link-purpose-link-only) to improve the usability of your links.

As you add links in your web application, please also consider reading through [Understanding Success Criterion 2.4.8: Location](https://www.w3.org/WAI/WCAG21/Understanding/location), which will help you learn how you can orient users to where they are in relation to the rest of the website content.

`a` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

a: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
a : List Elm.Expression -> List Elm.Expression -> Elm.Expression
a aArg aArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "a"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list aArg, Elm.list aArg0 ]


{-| `code` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

code: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
code : List Elm.Expression -> List Elm.Expression -> Elm.Expression
code codeArg codeArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "code"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list codeArg, Elm.list codeArg0 ]


{-| `em` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

em: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
em : List Elm.Expression -> List Elm.Expression -> Elm.Expression
em emArg emArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "em"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list emArg, Elm.list emArg0 ]


{-| `strong` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

strong: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
strong : List Elm.Expression -> List Elm.Expression -> Elm.Expression
strong strongArg strongArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "strong"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list strongArg, Elm.list strongArg0 ]


{-| `i` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

i: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
i : List Elm.Expression -> List Elm.Expression -> Elm.Expression
i iArg iArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "i"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list iArg, Elm.list iArg0 ]


{-| `b` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

b: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
b : List Elm.Expression -> List Elm.Expression -> Elm.Expression
b bArg bArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "b"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list bArg, Elm.list bArg0 ]


{-| `u` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

u: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
u : List Elm.Expression -> List Elm.Expression -> Elm.Expression
u uArg uArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "u"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list uArg, Elm.list uArg0 ]


{-| `sub` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

sub: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
sub : List Elm.Expression -> List Elm.Expression -> Elm.Expression
sub subArg subArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "sub"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list subArg, Elm.list subArg0 ]


{-| `sup` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

sup: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
sup : List Elm.Expression -> List Elm.Expression -> Elm.Expression
sup supArg supArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "sup"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list supArg, Elm.list supArg0 ]


{-| `br` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

br: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> Accessibility.Styled.Html msg
-}
br : List Elm.Expression -> Elm.Expression
br brArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "br"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list brArg ]


{-| `ol` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

ol: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
ol : List Elm.Expression -> List Elm.Expression -> Elm.Expression
ol olArg olArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "ol"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list olArg, Elm.list olArg0 ]


{-| `ul` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

ul: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
ul : List Elm.Expression -> List Elm.Expression -> Elm.Expression
ul ulArg ulArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "ul"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list ulArg, Elm.list ulArg0 ]


{-| `li` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

li: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
li : List Elm.Expression -> List Elm.Expression -> Elm.Expression
li liArg liArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "li"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list liArg, Elm.list liArg0 ]


{-| `dl` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

dl: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
dl : List Elm.Expression -> List Elm.Expression -> Elm.Expression
dl dlArg dlArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "dl"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list dlArg, Elm.list dlArg0 ]


{-| `dt` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

dt: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
dt : List Elm.Expression -> List Elm.Expression -> Elm.Expression
dt dtArg dtArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "dt"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list dtArg, Elm.list dtArg0 ]


{-| `dd` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

dd: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
dd : List Elm.Expression -> List Elm.Expression -> Elm.Expression
dd ddArg ddArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "dd"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list ddArg, Elm.list ddArg0 ]


{-| `iframe` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

iframe: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
iframe : List Elm.Expression -> List Elm.Expression -> Elm.Expression
iframe iframeArg iframeArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "iframe"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list iframeArg, Elm.list iframeArg0 ]


{-| `canvas` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

canvas: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
canvas : List Elm.Expression -> List Elm.Expression -> Elm.Expression
canvas canvasArg canvasArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "canvas"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list canvasArg, Elm.list canvasArg0 ]


{-| `math` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

math: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
math : List Elm.Expression -> List Elm.Expression -> Elm.Expression
math mathArg mathArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "math"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list mathArg, Elm.list mathArg0 ]


{-| `form` should generally not have event listeners. If you _really_ need to add
an event listener, use the formWithListeners method or directly use the elm/html library instead.

form: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
form : List Elm.Expression -> List Elm.Expression -> Elm.Expression
form formArg formArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "form"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list formArg, Elm.list formArg0 ]


{-| `form` should generally not have event listeners and `Accessibility.form` method
should be your go to implementation. If you _really_ need to add an event listener,
you can use this method instead but use caution when doing so!

formWithListeners: 
    List (Accessibility.Styled.Attribute msg)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
formWithListeners : List Elm.Expression -> List Elm.Expression -> Elm.Expression
formWithListeners formWithListenersArg formWithListenersArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "formWithListeners"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list formWithListenersArg, Elm.list formWithListenersArg0 ]


{-| `option` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

option: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
option : List Elm.Expression -> List Elm.Expression -> Elm.Expression
option optionArg optionArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "option"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list optionArg, Elm.list optionArg0 ]


{-| `section` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

section: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
section : List Elm.Expression -> List Elm.Expression -> Elm.Expression
section sectionArg sectionArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "section"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list sectionArg, Elm.list sectionArg0 ]


{-| `nav` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

nav: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
nav : List Elm.Expression -> List Elm.Expression -> Elm.Expression
nav navArg navArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "nav"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list navArg, Elm.list navArg0 ]


{-| `article` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

article: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
article : List Elm.Expression -> List Elm.Expression -> Elm.Expression
article articleArg articleArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "article"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list articleArg, Elm.list articleArg0 ]


{-| `aside` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

aside: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
aside : List Elm.Expression -> List Elm.Expression -> Elm.Expression
aside asideArg asideArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "aside"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list asideArg, Elm.list asideArg0 ]


{-| `header` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

header: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
header : List Elm.Expression -> List Elm.Expression -> Elm.Expression
header headerArg headerArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "header"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list headerArg, Elm.list headerArg0 ]


{-| `footer` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

footer: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
footer : List Elm.Expression -> List Elm.Expression -> Elm.Expression
footer footerArg footerArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "footer"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list footerArg, Elm.list footerArg0 ]


{-| `address` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

address: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
address : List Elm.Expression -> List Elm.Expression -> Elm.Expression
address addressArg addressArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "address"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list addressArg, Elm.list addressArg0 ]


{-| `main_` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

main_: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
main_ : List Elm.Expression -> List Elm.Expression -> Elm.Expression
main_ main_Arg main_Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "main_"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list main_Arg, Elm.list main_Arg0 ]


{-| Adds the group role to a figure.

figure: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
figure : List Elm.Expression -> List Elm.Expression -> Elm.Expression
figure figureArg figureArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "figure"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list figureArg, Elm.list figureArg0 ]


{-| `figcaption` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

figcaption: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
figcaption : List Elm.Expression -> List Elm.Expression -> Elm.Expression
figcaption figcaptionArg figcaptionArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "figcaption"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list figcaptionArg, Elm.list figcaptionArg0 ]


{-| `table` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

table: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
table : List Elm.Expression -> List Elm.Expression -> Elm.Expression
table tableArg tableArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "table"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list tableArg, Elm.list tableArg0 ]


{-| `caption` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

caption: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
caption : List Elm.Expression -> List Elm.Expression -> Elm.Expression
caption captionArg captionArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "caption"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list captionArg, Elm.list captionArg0 ]


{-| `colgroup` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

colgroup: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
colgroup : List Elm.Expression -> List Elm.Expression -> Elm.Expression
colgroup colgroupArg colgroupArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "colgroup"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list colgroupArg, Elm.list colgroupArg0 ]


{-| `col` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

col: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
col : List Elm.Expression -> List Elm.Expression -> Elm.Expression
col colArg colArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "col"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list colArg, Elm.list colArg0 ]


{-| `tbody` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

tbody: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
tbody : List Elm.Expression -> List Elm.Expression -> Elm.Expression
tbody tbodyArg tbodyArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "tbody"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list tbodyArg, Elm.list tbodyArg0 ]


{-| `thead` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

thead: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
thead : List Elm.Expression -> List Elm.Expression -> Elm.Expression
thead theadArg theadArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "thead"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list theadArg, Elm.list theadArg0 ]


{-| `tfoot` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

tfoot: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
tfoot : List Elm.Expression -> List Elm.Expression -> Elm.Expression
tfoot tfootArg tfootArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "tfoot"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list tfootArg, Elm.list tfootArg0 ]


{-| `tr` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

tr: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
tr : List Elm.Expression -> List Elm.Expression -> Elm.Expression
tr trArg trArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "tr"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list trArg, Elm.list trArg0 ]


{-| `td` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

td: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
td : List Elm.Expression -> List Elm.Expression -> Elm.Expression
td tdArg tdArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "td"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list tdArg, Elm.list tdArg0 ]


{-| `th` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

th: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
th : List Elm.Expression -> List Elm.Expression -> Elm.Expression
th thArg thArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "th"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list thArg, Elm.list thArg0 ]


{-| `fieldset` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

fieldset: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
fieldset : List Elm.Expression -> List Elm.Expression -> Elm.Expression
fieldset fieldsetArg fieldsetArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "fieldset"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list fieldsetArg, Elm.list fieldsetArg0 ]


{-| `legend` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

legend: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
legend : List Elm.Expression -> List Elm.Expression -> Elm.Expression
legend legendArg legendArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "legend"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list legendArg, Elm.list legendArg0 ]


{-| `label` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

label: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
label : List Elm.Expression -> List Elm.Expression -> Elm.Expression
label labelArg labelArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "label"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list labelArg, Elm.list labelArg0 ]


{-| `datalist` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

datalist: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
datalist : List Elm.Expression -> List Elm.Expression -> Elm.Expression
datalist datalistArg datalistArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "datalist"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list datalistArg, Elm.list datalistArg0 ]


{-| `optgroup` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

optgroup: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
optgroup : List Elm.Expression -> List Elm.Expression -> Elm.Expression
optgroup optgroupArg optgroupArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "optgroup"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list optgroupArg, Elm.list optgroupArg0 ]


{-| `output` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

output: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
output : List Elm.Expression -> List Elm.Expression -> Elm.Expression
output outputArg outputArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "output"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list outputArg, Elm.list outputArg0 ]


{-| `progress` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

progress: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
progress : List Elm.Expression -> List Elm.Expression -> Elm.Expression
progress progressArg progressArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "progress"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list progressArg, Elm.list progressArg0 ]


{-| `meter` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

meter: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
meter : List Elm.Expression -> List Elm.Expression -> Elm.Expression
meter meterArg meterArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "meter"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list meterArg, Elm.list meterArg0 ]


{-| `audio` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

audio: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
audio : List Elm.Expression -> List Elm.Expression -> Elm.Expression
audio audioArg audioArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "audio"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list audioArg, Elm.list audioArg0 ]


{-| `video` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

video: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
video : List Elm.Expression -> List Elm.Expression -> Elm.Expression
video videoArg videoArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "video"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list videoArg, Elm.list videoArg0 ]


{-| `source` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

source: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
source : List Elm.Expression -> List Elm.Expression -> Elm.Expression
source sourceArg sourceArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "source"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list sourceArg, Elm.list sourceArg0 ]


{-| `track` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

track: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
track : List Elm.Expression -> List Elm.Expression -> Elm.Expression
track trackArg trackArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "track"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list trackArg, Elm.list trackArg0 ]


{-| `embed` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

embed: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
embed : List Elm.Expression -> List Elm.Expression -> Elm.Expression
embed embedArg embedArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "embed"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list embedArg, Elm.list embedArg0 ]


{-| `object` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

object: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
object : List Elm.Expression -> List Elm.Expression -> Elm.Expression
object objectArg objectArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "object"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list objectArg, Elm.list objectArg0 ]


{-| `param` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

param: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
param : List Elm.Expression -> List Elm.Expression -> Elm.Expression
param paramArg paramArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "param"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list paramArg, Elm.list paramArg0 ]


{-| `ins` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

ins: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
ins : List Elm.Expression -> List Elm.Expression -> Elm.Expression
ins insArg insArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "ins"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list insArg, Elm.list insArg0 ]


{-| `del` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

del: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
del : List Elm.Expression -> List Elm.Expression -> Elm.Expression
del delArg delArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "del"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list delArg, Elm.list delArg0 ]


{-| `small` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

small: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
small : List Elm.Expression -> List Elm.Expression -> Elm.Expression
small smallArg smallArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "small"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list smallArg, Elm.list smallArg0 ]


{-| `cite` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

cite: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
cite : List Elm.Expression -> List Elm.Expression -> Elm.Expression
cite citeArg citeArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "cite"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list citeArg, Elm.list citeArg0 ]


{-| `dfn` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

dfn: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
dfn : List Elm.Expression -> List Elm.Expression -> Elm.Expression
dfn dfnArg dfnArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "dfn"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list dfnArg, Elm.list dfnArg0 ]


{-| `abbr` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

abbr: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
abbr : List Elm.Expression -> List Elm.Expression -> Elm.Expression
abbr abbrArg abbrArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "abbr"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list abbrArg, Elm.list abbrArg0 ]


{-| `time` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

time: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
time : List Elm.Expression -> List Elm.Expression -> Elm.Expression
time timeArg timeArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "time"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list timeArg, Elm.list timeArg0 ]


{-| `var` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

var: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
var : List Elm.Expression -> List Elm.Expression -> Elm.Expression
var varArg varArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "var"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list varArg, Elm.list varArg0 ]


{-| `samp` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

samp: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
samp : List Elm.Expression -> List Elm.Expression -> Elm.Expression
samp sampArg sampArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "samp"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list sampArg, Elm.list sampArg0 ]


{-| `kbd` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

kbd: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
kbd : List Elm.Expression -> List Elm.Expression -> Elm.Expression
kbd kbdArg kbdArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "kbd"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list kbdArg, Elm.list kbdArg0 ]


{-| `s` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

s: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
s : List Elm.Expression -> List Elm.Expression -> Elm.Expression
s sArg sArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "s"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list sArg, Elm.list sArg0 ]


{-| `q` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

q: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
q : List Elm.Expression -> List Elm.Expression -> Elm.Expression
q qArg qArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "q"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list qArg, Elm.list qArg0 ]


{-| `mark` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

mark: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
mark : List Elm.Expression -> List Elm.Expression -> Elm.Expression
mark markArg markArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "mark"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list markArg, Elm.list markArg0 ]


{-| `ruby` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

ruby: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
ruby : List Elm.Expression -> List Elm.Expression -> Elm.Expression
ruby rubyArg rubyArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "ruby"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list rubyArg, Elm.list rubyArg0 ]


{-| `rt` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

rt: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
rt : List Elm.Expression -> List Elm.Expression -> Elm.Expression
rt rtArg rtArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "rt"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list rtArg, Elm.list rtArg0 ]


{-| `rp` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

rp: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
rp : List Elm.Expression -> List Elm.Expression -> Elm.Expression
rp rpArg rpArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "rp"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list rpArg, Elm.list rpArg0 ]


{-| `bdi` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

bdi: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
bdi : List Elm.Expression -> List Elm.Expression -> Elm.Expression
bdi bdiArg bdiArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "bdi"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list bdiArg, Elm.list bdiArg0 ]


{-| `bdo` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

bdo: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
bdo : List Elm.Expression -> List Elm.Expression -> Elm.Expression
bdo bdoArg bdoArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "bdo"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list bdoArg, Elm.list bdoArg0 ]


{-| `wbr` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

wbr: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
wbr : List Elm.Expression -> List Elm.Expression -> Elm.Expression
wbr wbrArg wbrArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "wbr"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list wbrArg, Elm.list wbrArg0 ]


{-| `details` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

details: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
details : List Elm.Expression -> List Elm.Expression -> Elm.Expression
details detailsArg detailsArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "details"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list detailsArg, Elm.list detailsArg0 ]


{-| `summary` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

summary: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
summary : List Elm.Expression -> List Elm.Expression -> Elm.Expression
summary summaryArg summaryArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "summary"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list summaryArg, Elm.list summaryArg0 ]


{-| `menuitem` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

menuitem: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
menuitem : List Elm.Expression -> List Elm.Expression -> Elm.Expression
menuitem menuitemArg menuitemArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "menuitem"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list menuitemArg, Elm.list menuitemArg0 ]


{-| `menu` should generally not have event listeners. If you _really_ need to add
an event listener, use the elm/html library instead.

menu: 
    List (Accessibility.Styled.Attribute Basics.Never)
    -> List (Accessibility.Styled.Html msg)
    -> Accessibility.Styled.Html msg
-}
menu : List Elm.Expression -> List Elm.Expression -> Elm.Expression
menu menuArg menuArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "menu"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list menuArg, Elm.list menuArg0 ]


{-| `map` directly aliases the function of the same name from rtfeldman/elm-css.

Please see [the docs for the Html.Styled.map](https://package.elm-lang.org/packages/rtfeldman/elm-css/17.0.1/Html-Styled#map).

map: (a -> msg) -> Accessibility.Styled.Html a -> Accessibility.Styled.Html msg
-}
map : (Elm.Expression -> Elm.Expression) -> Elm.Expression -> Elm.Expression
map mapArg mapArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "map"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.var "a" ] (Type.var "msg")
                        , Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "a" ]
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.functionReduced "mapUnpack" mapArg, mapArg0 ]


{-| fromUnstyled: Html.Html msg -> Accessibility.Styled.Html msg -}
fromUnstyled : Elm.Expression -> Elm.Expression
fromUnstyled fromUnstyledArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "fromUnstyled"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ] ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ fromUnstyledArg ]


{-| toUnstyled: Accessibility.Styled.Html msg -> Html.Html msg -}
toUnstyled : Elm.Expression -> Elm.Expression
toUnstyled toUnstyledArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "toUnstyled"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
        )
        [ toUnstyledArg ]


{-| styled: 
    (List (Accessibility.Styled.Attribute a)
    -> List (Accessibility.Styled.Html b)
    -> Accessibility.Styled.Html msg)
    -> List Css.Style
    -> List (Accessibility.Styled.Attribute a)
    -> List (Accessibility.Styled.Html b)
    -> Accessibility.Styled.Html msg
-}
styled :
    (Elm.Expression -> Elm.Expression -> Elm.Expression)
    -> List Elm.Expression
    -> List Elm.Expression
    -> List Elm.Expression
    -> Elm.Expression
styled styledArg styledArg0 styledArg1 styledArg2 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "styled"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.list
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Attribute"
                                    [ Type.var "a" ]
                                )
                            , Type.list
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "b" ]
                                )
                            ]
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        , Type.list (Type.namedWith [ "Css" ] "Style" [])
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.var "a" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "b" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.functionReduced
            "styledUnpack"
            (\functionReducedUnpack ->
                Elm.functionReduced "unpack" (styledArg functionReducedUnpack)
            )
        , Elm.list styledArg0
        , Elm.list styledArg1
        , Elm.list styledArg2
        ]


annotation_ :
    { html : Type.Annotation -> Type.Annotation
    , attribute : Type.Annotation -> Type.Annotation
    }
annotation_ =
    { html =
        \htmlArg0 ->
            Type.alias
                moduleName_
                "Html"
                [ htmlArg0 ]
                (Type.namedWith [ "Html", "Styled" ] "Html" [ Type.var "msg" ])
    , attribute =
        \attributeArg0 ->
            Type.alias
                moduleName_
                "Attribute"
                [ attributeArg0 ]
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
    }


call_ :
    { labelBefore :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , labelAfter :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , labelHidden :
        Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
    , inputText : Elm.Expression -> Elm.Expression -> Elm.Expression
    , inputNumber : Elm.Expression -> Elm.Expression -> Elm.Expression
    , radio :
        Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
    , checkbox :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , tabList : Elm.Expression -> Elm.Expression -> Elm.Expression
    , tab : Elm.Expression -> Elm.Expression -> Elm.Expression
    , tabPanel : Elm.Expression -> Elm.Expression -> Elm.Expression
    , img : Elm.Expression -> Elm.Expression -> Elm.Expression
    , decorativeImg : Elm.Expression -> Elm.Expression
    , button : Elm.Expression -> Elm.Expression -> Elm.Expression
    , textarea : Elm.Expression -> Elm.Expression -> Elm.Expression
    , select : Elm.Expression -> Elm.Expression -> Elm.Expression
    , text : Elm.Expression -> Elm.Expression
    , h1 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , h2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , h3 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , h4 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , h5 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , h6 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , div : Elm.Expression -> Elm.Expression -> Elm.Expression
    , p : Elm.Expression -> Elm.Expression -> Elm.Expression
    , hr : Elm.Expression -> Elm.Expression -> Elm.Expression
    , pre : Elm.Expression -> Elm.Expression -> Elm.Expression
    , blockquote : Elm.Expression -> Elm.Expression -> Elm.Expression
    , span : Elm.Expression -> Elm.Expression -> Elm.Expression
    , a : Elm.Expression -> Elm.Expression -> Elm.Expression
    , code : Elm.Expression -> Elm.Expression -> Elm.Expression
    , em : Elm.Expression -> Elm.Expression -> Elm.Expression
    , strong : Elm.Expression -> Elm.Expression -> Elm.Expression
    , i : Elm.Expression -> Elm.Expression -> Elm.Expression
    , b : Elm.Expression -> Elm.Expression -> Elm.Expression
    , u : Elm.Expression -> Elm.Expression -> Elm.Expression
    , sub : Elm.Expression -> Elm.Expression -> Elm.Expression
    , sup : Elm.Expression -> Elm.Expression -> Elm.Expression
    , br : Elm.Expression -> Elm.Expression
    , ol : Elm.Expression -> Elm.Expression -> Elm.Expression
    , ul : Elm.Expression -> Elm.Expression -> Elm.Expression
    , li : Elm.Expression -> Elm.Expression -> Elm.Expression
    , dl : Elm.Expression -> Elm.Expression -> Elm.Expression
    , dt : Elm.Expression -> Elm.Expression -> Elm.Expression
    , dd : Elm.Expression -> Elm.Expression -> Elm.Expression
    , iframe : Elm.Expression -> Elm.Expression -> Elm.Expression
    , canvas : Elm.Expression -> Elm.Expression -> Elm.Expression
    , math : Elm.Expression -> Elm.Expression -> Elm.Expression
    , form : Elm.Expression -> Elm.Expression -> Elm.Expression
    , formWithListeners : Elm.Expression -> Elm.Expression -> Elm.Expression
    , option : Elm.Expression -> Elm.Expression -> Elm.Expression
    , section : Elm.Expression -> Elm.Expression -> Elm.Expression
    , nav : Elm.Expression -> Elm.Expression -> Elm.Expression
    , article : Elm.Expression -> Elm.Expression -> Elm.Expression
    , aside : Elm.Expression -> Elm.Expression -> Elm.Expression
    , header : Elm.Expression -> Elm.Expression -> Elm.Expression
    , footer : Elm.Expression -> Elm.Expression -> Elm.Expression
    , address : Elm.Expression -> Elm.Expression -> Elm.Expression
    , main_ : Elm.Expression -> Elm.Expression -> Elm.Expression
    , figure : Elm.Expression -> Elm.Expression -> Elm.Expression
    , figcaption : Elm.Expression -> Elm.Expression -> Elm.Expression
    , table : Elm.Expression -> Elm.Expression -> Elm.Expression
    , caption : Elm.Expression -> Elm.Expression -> Elm.Expression
    , colgroup : Elm.Expression -> Elm.Expression -> Elm.Expression
    , col : Elm.Expression -> Elm.Expression -> Elm.Expression
    , tbody : Elm.Expression -> Elm.Expression -> Elm.Expression
    , thead : Elm.Expression -> Elm.Expression -> Elm.Expression
    , tfoot : Elm.Expression -> Elm.Expression -> Elm.Expression
    , tr : Elm.Expression -> Elm.Expression -> Elm.Expression
    , td : Elm.Expression -> Elm.Expression -> Elm.Expression
    , th : Elm.Expression -> Elm.Expression -> Elm.Expression
    , fieldset : Elm.Expression -> Elm.Expression -> Elm.Expression
    , legend : Elm.Expression -> Elm.Expression -> Elm.Expression
    , label : Elm.Expression -> Elm.Expression -> Elm.Expression
    , datalist : Elm.Expression -> Elm.Expression -> Elm.Expression
    , optgroup : Elm.Expression -> Elm.Expression -> Elm.Expression
    , output : Elm.Expression -> Elm.Expression -> Elm.Expression
    , progress : Elm.Expression -> Elm.Expression -> Elm.Expression
    , meter : Elm.Expression -> Elm.Expression -> Elm.Expression
    , audio : Elm.Expression -> Elm.Expression -> Elm.Expression
    , video : Elm.Expression -> Elm.Expression -> Elm.Expression
    , source : Elm.Expression -> Elm.Expression -> Elm.Expression
    , track : Elm.Expression -> Elm.Expression -> Elm.Expression
    , embed : Elm.Expression -> Elm.Expression -> Elm.Expression
    , object : Elm.Expression -> Elm.Expression -> Elm.Expression
    , param : Elm.Expression -> Elm.Expression -> Elm.Expression
    , ins : Elm.Expression -> Elm.Expression -> Elm.Expression
    , del : Elm.Expression -> Elm.Expression -> Elm.Expression
    , small : Elm.Expression -> Elm.Expression -> Elm.Expression
    , cite : Elm.Expression -> Elm.Expression -> Elm.Expression
    , dfn : Elm.Expression -> Elm.Expression -> Elm.Expression
    , abbr : Elm.Expression -> Elm.Expression -> Elm.Expression
    , time : Elm.Expression -> Elm.Expression -> Elm.Expression
    , var : Elm.Expression -> Elm.Expression -> Elm.Expression
    , samp : Elm.Expression -> Elm.Expression -> Elm.Expression
    , kbd : Elm.Expression -> Elm.Expression -> Elm.Expression
    , s : Elm.Expression -> Elm.Expression -> Elm.Expression
    , q : Elm.Expression -> Elm.Expression -> Elm.Expression
    , mark : Elm.Expression -> Elm.Expression -> Elm.Expression
    , ruby : Elm.Expression -> Elm.Expression -> Elm.Expression
    , rt : Elm.Expression -> Elm.Expression -> Elm.Expression
    , rp : Elm.Expression -> Elm.Expression -> Elm.Expression
    , bdi : Elm.Expression -> Elm.Expression -> Elm.Expression
    , bdo : Elm.Expression -> Elm.Expression -> Elm.Expression
    , wbr : Elm.Expression -> Elm.Expression -> Elm.Expression
    , details : Elm.Expression -> Elm.Expression -> Elm.Expression
    , summary : Elm.Expression -> Elm.Expression -> Elm.Expression
    , menuitem : Elm.Expression -> Elm.Expression -> Elm.Expression
    , menu : Elm.Expression -> Elm.Expression -> Elm.Expression
    , map : Elm.Expression -> Elm.Expression -> Elm.Expression
    , fromUnstyled : Elm.Expression -> Elm.Expression
    , toUnstyled : Elm.Expression -> Elm.Expression
    , styled :
        Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
    }
call_ =
    { labelBefore =
        \labelBeforeArg labelBeforeArg0 labelBeforeArg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "labelBefore"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.namedWith [ "Basics" ] "Never" [] ]
                                , Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ labelBeforeArg, labelBeforeArg0, labelBeforeArg1 ]
    , labelAfter =
        \labelAfterArg labelAfterArg0 labelAfterArg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "labelAfter"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.namedWith [ "Basics" ] "Never" [] ]
                                , Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ labelAfterArg, labelAfterArg0, labelAfterArg1 ]
    , labelHidden =
        \labelHiddenArg labelHiddenArg0 labelHiddenArg1 labelHiddenArg2 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "labelHidden"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.namedWith [ "Basics" ] "Never" [] ]
                                , Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ labelHiddenArg
                , labelHiddenArg0
                , labelHiddenArg1
                , labelHiddenArg2
                ]
    , inputText =
        \inputTextArg inputTextArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "inputText"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ inputTextArg, inputTextArg0 ]
    , inputNumber =
        \inputNumberArg inputNumberArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "inputNumber"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ inputNumberArg, inputNumberArg0 ]
    , radio =
        \radioArg radioArg0 radioArg1 radioArg2 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "radio"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string
                                , Type.string
                                , Type.bool
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ radioArg, radioArg0, radioArg1, radioArg2 ]
    , checkbox =
        \checkboxArg checkboxArg0 checkboxArg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "checkbox"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string
                                , Type.maybe Type.bool
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ checkboxArg, checkboxArg0, checkboxArg1 ]
    , tabList =
        \tabListArg tabListArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "tabList"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ tabListArg, tabListArg0 ]
    , tab =
        \tabArg tabArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "tab"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ tabArg, tabArg0 ]
    , tabPanel =
        \tabPanelArg tabPanelArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "tabPanel"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ tabPanelArg, tabPanelArg0 ]
    , img =
        \imgArg imgArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "img"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ imgArg, imgArg0 ]
    , decorativeImg =
        \decorativeImgArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "decorativeImg"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ decorativeImgArg ]
    , button =
        \buttonArg buttonArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "button"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ buttonArg, buttonArg0 ]
    , textarea =
        \textareaArg textareaArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "textarea"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ textareaArg, textareaArg0 ]
    , select =
        \selectArg selectArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "select"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ selectArg, selectArg0 ]
    , text =
        \textArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "text"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ textArg ]
    , h1 =
        \h1Arg h1Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "h1"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ h1Arg, h1Arg0 ]
    , h2 =
        \h2Arg h2Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "h2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ h2Arg, h2Arg0 ]
    , h3 =
        \h3Arg h3Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "h3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ h3Arg, h3Arg0 ]
    , h4 =
        \h4Arg h4Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "h4"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ h4Arg, h4Arg0 ]
    , h5 =
        \h5Arg h5Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "h5"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ h5Arg, h5Arg0 ]
    , h6 =
        \h6Arg h6Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "h6"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ h6Arg, h6Arg0 ]
    , div =
        \divArg divArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "div"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ divArg, divArg0 ]
    , p =
        \pArg pArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "p"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ pArg, pArg0 ]
    , hr =
        \hrArg hrArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "hr"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ hrArg, hrArg0 ]
    , pre =
        \preArg preArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "pre"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ preArg, preArg0 ]
    , blockquote =
        \blockquoteArg blockquoteArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "blockquote"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ blockquoteArg, blockquoteArg0 ]
    , span =
        \spanArg spanArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "span"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ spanArg, spanArg0 ]
    , a =
        \aArg aArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "a"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ aArg, aArg0 ]
    , code =
        \codeArg codeArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "code"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ codeArg, codeArg0 ]
    , em =
        \emArg emArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "em"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ emArg, emArg0 ]
    , strong =
        \strongArg strongArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "strong"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ strongArg, strongArg0 ]
    , i =
        \iArg iArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "i"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ iArg, iArg0 ]
    , b =
        \bArg bArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "b"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ bArg, bArg0 ]
    , u =
        \uArg uArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "u"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ uArg, uArg0 ]
    , sub =
        \subArg subArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "sub"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ subArg, subArg0 ]
    , sup =
        \supArg supArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "sup"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ supArg, supArg0 ]
    , br =
        \brArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "br"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ brArg ]
    , ol =
        \olArg olArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "ol"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ olArg, olArg0 ]
    , ul =
        \ulArg ulArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "ul"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ ulArg, ulArg0 ]
    , li =
        \liArg liArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "li"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ liArg, liArg0 ]
    , dl =
        \dlArg dlArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "dl"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ dlArg, dlArg0 ]
    , dt =
        \dtArg dtArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "dt"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ dtArg, dtArg0 ]
    , dd =
        \ddArg ddArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "dd"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ ddArg, ddArg0 ]
    , iframe =
        \iframeArg iframeArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "iframe"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ iframeArg, iframeArg0 ]
    , canvas =
        \canvasArg canvasArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "canvas"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ canvasArg, canvasArg0 ]
    , math =
        \mathArg mathArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "math"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ mathArg, mathArg0 ]
    , form =
        \formArg formArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "form"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ formArg, formArg0 ]
    , formWithListeners =
        \formWithListenersArg formWithListenersArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "formWithListeners"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ formWithListenersArg, formWithListenersArg0 ]
    , option =
        \optionArg optionArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "option"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ optionArg, optionArg0 ]
    , section =
        \sectionArg sectionArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "section"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ sectionArg, sectionArg0 ]
    , nav =
        \navArg navArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "nav"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ navArg, navArg0 ]
    , article =
        \articleArg articleArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "article"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ articleArg, articleArg0 ]
    , aside =
        \asideArg asideArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "aside"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ asideArg, asideArg0 ]
    , header =
        \headerArg headerArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "header"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ headerArg, headerArg0 ]
    , footer =
        \footerArg footerArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "footer"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ footerArg, footerArg0 ]
    , address =
        \addressArg addressArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "address"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ addressArg, addressArg0 ]
    , main_ =
        \main_Arg main_Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "main_"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ main_Arg, main_Arg0 ]
    , figure =
        \figureArg figureArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "figure"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ figureArg, figureArg0 ]
    , figcaption =
        \figcaptionArg figcaptionArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "figcaption"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ figcaptionArg, figcaptionArg0 ]
    , table =
        \tableArg tableArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "table"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ tableArg, tableArg0 ]
    , caption =
        \captionArg captionArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "caption"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ captionArg, captionArg0 ]
    , colgroup =
        \colgroupArg colgroupArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "colgroup"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ colgroupArg, colgroupArg0 ]
    , col =
        \colArg colArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "col"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ colArg, colArg0 ]
    , tbody =
        \tbodyArg tbodyArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "tbody"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ tbodyArg, tbodyArg0 ]
    , thead =
        \theadArg theadArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "thead"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ theadArg, theadArg0 ]
    , tfoot =
        \tfootArg tfootArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "tfoot"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ tfootArg, tfootArg0 ]
    , tr =
        \trArg trArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "tr"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ trArg, trArg0 ]
    , td =
        \tdArg tdArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "td"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ tdArg, tdArg0 ]
    , th =
        \thArg thArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "th"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ thArg, thArg0 ]
    , fieldset =
        \fieldsetArg fieldsetArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "fieldset"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ fieldsetArg, fieldsetArg0 ]
    , legend =
        \legendArg legendArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "legend"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ legendArg, legendArg0 ]
    , label =
        \labelArg labelArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "label"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ labelArg, labelArg0 ]
    , datalist =
        \datalistArg datalistArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "datalist"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ datalistArg, datalistArg0 ]
    , optgroup =
        \optgroupArg optgroupArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "optgroup"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ optgroupArg, optgroupArg0 ]
    , output =
        \outputArg outputArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "output"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ outputArg, outputArg0 ]
    , progress =
        \progressArg progressArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "progress"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ progressArg, progressArg0 ]
    , meter =
        \meterArg meterArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "meter"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ meterArg, meterArg0 ]
    , audio =
        \audioArg audioArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "audio"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ audioArg, audioArg0 ]
    , video =
        \videoArg videoArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "video"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ videoArg, videoArg0 ]
    , source =
        \sourceArg sourceArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "source"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ sourceArg, sourceArg0 ]
    , track =
        \trackArg trackArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "track"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ trackArg, trackArg0 ]
    , embed =
        \embedArg embedArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "embed"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ embedArg, embedArg0 ]
    , object =
        \objectArg objectArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "object"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ objectArg, objectArg0 ]
    , param =
        \paramArg paramArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "param"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ paramArg, paramArg0 ]
    , ins =
        \insArg insArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "ins"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ insArg, insArg0 ]
    , del =
        \delArg delArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "del"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ delArg, delArg0 ]
    , small =
        \smallArg smallArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "small"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ smallArg, smallArg0 ]
    , cite =
        \citeArg citeArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "cite"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ citeArg, citeArg0 ]
    , dfn =
        \dfnArg dfnArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "dfn"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ dfnArg, dfnArg0 ]
    , abbr =
        \abbrArg abbrArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "abbr"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ abbrArg, abbrArg0 ]
    , time =
        \timeArg timeArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "time"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ timeArg, timeArg0 ]
    , var =
        \varArg varArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "var"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ varArg, varArg0 ]
    , samp =
        \sampArg sampArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "samp"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ sampArg, sampArg0 ]
    , kbd =
        \kbdArg kbdArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "kbd"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ kbdArg, kbdArg0 ]
    , s =
        \sArg sArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "s"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ sArg, sArg0 ]
    , q =
        \qArg qArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "q"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ qArg, qArg0 ]
    , mark =
        \markArg markArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "mark"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ markArg, markArg0 ]
    , ruby =
        \rubyArg rubyArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "ruby"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ rubyArg, rubyArg0 ]
    , rt =
        \rtArg rtArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "rt"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ rtArg, rtArg0 ]
    , rp =
        \rpArg rpArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "rp"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ rpArg, rpArg0 ]
    , bdi =
        \bdiArg bdiArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "bdi"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ bdiArg, bdiArg0 ]
    , bdo =
        \bdoArg bdoArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "bdo"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ bdoArg, bdoArg0 ]
    , wbr =
        \wbrArg wbrArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "wbr"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ wbrArg, wbrArg0 ]
    , details =
        \detailsArg detailsArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "details"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ detailsArg, detailsArg0 ]
    , summary =
        \summaryArg summaryArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "summary"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ summaryArg, summaryArg0 ]
    , menuitem =
        \menuitemArg menuitemArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "menuitem"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ menuitemArg, menuitemArg0 ]
    , menu =
        \menuArg menuArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "menu"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ menuArg, menuArg0 ]
    , map =
        \mapArg mapArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "map"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.var "a" ]
                                    (Type.var "msg")
                                , Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "a" ]
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ mapArg, mapArg0 ]
    , fromUnstyled =
        \fromUnstyledArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "fromUnstyled"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Html" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ fromUnstyledArg ]
    , toUnstyled =
        \toUnstyledArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "toUnstyled"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith
                                    [ "Html" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ toUnstyledArg ]
    , styled =
        \styledArg styledArg0 styledArg1 styledArg2 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Accessibility", "Styled" ]
                    , name = "styled"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.list
                                        (Type.namedWith
                                            [ "Accessibility", "Styled" ]
                                            "Attribute"
                                            [ Type.var "a" ]
                                        )
                                    , Type.list
                                        (Type.namedWith
                                            [ "Accessibility", "Styled" ]
                                            "Html"
                                            [ Type.var "b" ]
                                        )
                                    ]
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.namedWith [ "Css" ] "Style" [])
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Attribute"
                                        [ Type.var "a" ]
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Accessibility", "Styled" ]
                                        "Html"
                                        [ Type.var "b" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ styledArg, styledArg0, styledArg1, styledArg2 ]
    }


values_ :
    { labelBefore : Elm.Expression
    , labelAfter : Elm.Expression
    , labelHidden : Elm.Expression
    , inputText : Elm.Expression
    , inputNumber : Elm.Expression
    , radio : Elm.Expression
    , checkbox : Elm.Expression
    , tabList : Elm.Expression
    , tab : Elm.Expression
    , tabPanel : Elm.Expression
    , img : Elm.Expression
    , decorativeImg : Elm.Expression
    , button : Elm.Expression
    , textarea : Elm.Expression
    , select : Elm.Expression
    , text : Elm.Expression
    , h1 : Elm.Expression
    , h2 : Elm.Expression
    , h3 : Elm.Expression
    , h4 : Elm.Expression
    , h5 : Elm.Expression
    , h6 : Elm.Expression
    , div : Elm.Expression
    , p : Elm.Expression
    , hr : Elm.Expression
    , pre : Elm.Expression
    , blockquote : Elm.Expression
    , span : Elm.Expression
    , a : Elm.Expression
    , code : Elm.Expression
    , em : Elm.Expression
    , strong : Elm.Expression
    , i : Elm.Expression
    , b : Elm.Expression
    , u : Elm.Expression
    , sub : Elm.Expression
    , sup : Elm.Expression
    , br : Elm.Expression
    , ol : Elm.Expression
    , ul : Elm.Expression
    , li : Elm.Expression
    , dl : Elm.Expression
    , dt : Elm.Expression
    , dd : Elm.Expression
    , iframe : Elm.Expression
    , canvas : Elm.Expression
    , math : Elm.Expression
    , form : Elm.Expression
    , formWithListeners : Elm.Expression
    , option : Elm.Expression
    , section : Elm.Expression
    , nav : Elm.Expression
    , article : Elm.Expression
    , aside : Elm.Expression
    , header : Elm.Expression
    , footer : Elm.Expression
    , address : Elm.Expression
    , main_ : Elm.Expression
    , figure : Elm.Expression
    , figcaption : Elm.Expression
    , table : Elm.Expression
    , caption : Elm.Expression
    , colgroup : Elm.Expression
    , col : Elm.Expression
    , tbody : Elm.Expression
    , thead : Elm.Expression
    , tfoot : Elm.Expression
    , tr : Elm.Expression
    , td : Elm.Expression
    , th : Elm.Expression
    , fieldset : Elm.Expression
    , legend : Elm.Expression
    , label : Elm.Expression
    , datalist : Elm.Expression
    , optgroup : Elm.Expression
    , output : Elm.Expression
    , progress : Elm.Expression
    , meter : Elm.Expression
    , audio : Elm.Expression
    , video : Elm.Expression
    , source : Elm.Expression
    , track : Elm.Expression
    , embed : Elm.Expression
    , object : Elm.Expression
    , param : Elm.Expression
    , ins : Elm.Expression
    , del : Elm.Expression
    , small : Elm.Expression
    , cite : Elm.Expression
    , dfn : Elm.Expression
    , abbr : Elm.Expression
    , time : Elm.Expression
    , var : Elm.Expression
    , samp : Elm.Expression
    , kbd : Elm.Expression
    , s : Elm.Expression
    , q : Elm.Expression
    , mark : Elm.Expression
    , ruby : Elm.Expression
    , rt : Elm.Expression
    , rp : Elm.Expression
    , bdi : Elm.Expression
    , bdo : Elm.Expression
    , wbr : Elm.Expression
    , details : Elm.Expression
    , summary : Elm.Expression
    , menuitem : Elm.Expression
    , menu : Elm.Expression
    , map : Elm.Expression
    , fromUnstyled : Elm.Expression
    , toUnstyled : Elm.Expression
    , styled : Elm.Expression
    }
values_ =
    { labelBefore =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "labelBefore"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.namedWith [ "Basics" ] "Never" [] ]
                        , Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , labelAfter =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "labelAfter"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.namedWith [ "Basics" ] "Never" [] ]
                        , Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , labelHidden =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "labelHidden"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.namedWith [ "Basics" ] "Never" [] ]
                        , Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , inputText =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "inputText"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , inputNumber =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "inputNumber"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , radio =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "radio"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.string
                        , Type.bool
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , checkbox =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "checkbox"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.maybe Type.bool
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , tabList =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "tabList"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , tab =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "tab"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , tabPanel =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "tabPanel"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , img =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "img"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , decorativeImg =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "decorativeImg"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , button =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "button"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , textarea =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "textarea"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , select =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "select"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , text =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "text"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , h1 =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "h1"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , h2 =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "h2"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , h3 =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "h3"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , h4 =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "h4"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , h5 =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "h5"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , h6 =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "h6"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , div =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "div"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , p =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "p"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , hr =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "hr"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , pre =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "pre"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , blockquote =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "blockquote"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , span =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "span"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , a =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "a"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , code =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "code"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , em =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "em"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , strong =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "strong"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , i =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "i"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , b =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "b"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , u =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "u"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , sub =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "sub"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , sup =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "sup"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , br =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "br"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , ol =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "ol"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , ul =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "ul"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , li =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "li"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , dl =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "dl"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , dt =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "dt"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , dd =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "dd"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , iframe =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "iframe"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , canvas =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "canvas"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , math =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "math"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , form =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "form"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , formWithListeners =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "formWithListeners"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , option =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "option"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , section =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "section"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , nav =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "nav"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , article =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "article"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , aside =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "aside"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , header =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "header"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , footer =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "footer"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , address =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "address"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , main_ =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "main_"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , figure =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "figure"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , figcaption =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "figcaption"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , table =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "table"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , caption =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "caption"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , colgroup =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "colgroup"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , col =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "col"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , tbody =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "tbody"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , thead =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "thead"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , tfoot =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "tfoot"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , tr =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "tr"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , td =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "td"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , th =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "th"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , fieldset =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "fieldset"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , legend =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "legend"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , label =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "label"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , datalist =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "datalist"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , optgroup =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "optgroup"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , output =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "output"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , progress =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "progress"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , meter =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "meter"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , audio =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "audio"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , video =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "video"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , source =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "source"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , track =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "track"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , embed =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "embed"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , object =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "object"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , param =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "param"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , ins =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "ins"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , del =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "del"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , small =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "small"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , cite =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "cite"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , dfn =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "dfn"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , abbr =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "abbr"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , time =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "time"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , var =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "var"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , samp =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "samp"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , kbd =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "kbd"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , s =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "s"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , q =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "q"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , mark =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "mark"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , ruby =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "ruby"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , rt =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "rt"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , rp =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "rp"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , bdi =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "bdi"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , bdo =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "bdo"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , wbr =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "wbr"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , details =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "details"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , summary =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "summary"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , menuitem =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "menuitem"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , menu =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "menu"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , map =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "map"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.var "a" ] (Type.var "msg")
                        , Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "a" ]
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , fromUnstyled =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "fromUnstyled"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ] ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , toUnstyled =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "toUnstyled"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , styled =
        Elm.value
            { importFrom = [ "Accessibility", "Styled" ]
            , name = "styled"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.list
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Attribute"
                                    [ Type.var "a" ]
                                )
                            , Type.list
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "b" ]
                                )
                            ]
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                            )
                        , Type.list (Type.namedWith [ "Css" ] "Style" [])
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.var "a" ]
                            )
                        , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "b" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    }


