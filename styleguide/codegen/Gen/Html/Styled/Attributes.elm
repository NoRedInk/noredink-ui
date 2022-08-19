module Gen.Html.Styled.Attributes exposing (accept, acceptCharset, accesskey, action, align, alt, attribute, autocomplete, autofocus, autoplay, call_, checked, cite, class, classList, cols, colspan, contenteditable, contextmenu, controls, coords, css, datetime, default, dir, disabled, download, draggable, dropzone, enctype, for, form, fromUnstyled, headers, height, hidden, href, hreflang, id, ismap, itemprop, kind, lang, list, loop, manifest, map, max, maxlength, media, method, min, minlength, moduleName_, multiple, name, novalidate, pattern, ping, placeholder, poster, preload, property, pubdate, readonly, rel, required, reversed, rows, rowspan, sandbox, scope, selected, shape, size, spellcheck, src, srcdoc, srclang, start, step, style, tabindex, target, title, type_, usemap, value, values_, width, wrap)

{-| 
@docs moduleName_, css, fromUnstyled, property, attribute, map, style, class, classList, id, title, hidden, type_, value, checked, placeholder, selected, accept, acceptCharset, action, autocomplete, autofocus, disabled, enctype, list, maxlength, minlength, method, multiple, name, novalidate, pattern, readonly, required, size, for, form, max, min, step, cols, rows, wrap, href, target, download, hreflang, media, ping, rel, ismap, usemap, shape, coords, src, height, width, alt, autoplay, controls, loop, preload, poster, default, kind, srclang, sandbox, srcdoc, reversed, start, align, colspan, rowspan, headers, scope, accesskey, contenteditable, contextmenu, dir, draggable, dropzone, itemprop, lang, spellcheck, tabindex, cite, datetime, pubdate, manifest, call_, values_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Html", "Styled", "Attributes" ]


{-| Apply styles to an element.
See the [`Css` module documentation](http://package.elm-lang.org/packages/rtfeldman/elm-css/latest/Css) for an overview of how to use this function.

css: List Css.Style -> Html.Styled.Attribute msg
-}
css : List Elm.Expression -> Elm.Expression
css cssArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "css"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list cssArg ]


{-| Convert from an unstyled `Attribute` to a styled one.

Note that there is no `toUnstyled` for Attributes! This is because the process
of going from styled to unstyled involves adding a `<style>` element to the DOM,
and Attributes cannot do that. So it would be impossible to implement
`toUnstyled` for Attributes without throwing the styles away - which is definitely
undesirable!

fromUnstyled: VirtualDom.Attribute msg -> Html.Styled.Attribute msg
-}
fromUnstyled : Elm.Expression -> Elm.Expression
fromUnstyled fromUnstyledArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "fromUnstyled"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "VirtualDom" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ fromUnstyledArg ]


{-| Create _properties_, like saying `domNode.className = 'greeting'` in
JavaScript.

    import Json.Encode as Encode

    class : String -> Attribute msg
    class name =
        property "className" (Encode.string name)

Read more about the difference between properties and attributes [here].

[here]: https://github.com/elm/html/blob/master/properties-vs-attributes.md

property: String -> Json.Encode.Value -> Html.Styled.Attribute msg
-}
property : String -> Elm.Expression -> Elm.Expression
property propertyArg propertyArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "property"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.namedWith [ "Json", "Encode" ] "Value" []
                        ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string propertyArg, propertyArg0 ]


{-| Create _attributes_, like saying `domNode.setAttribute('class', 'greeting')`
in JavaScript.

    class : String -> Attribute msg
    class name =
        attribute "class" name

Read more about the difference between properties and attributes [here].

[here]: https://github.com/elm/html/blob/master/properties-vs-attributes.md

attribute: String -> String -> Html.Styled.Attribute msg
-}
attribute : String -> String -> Elm.Expression
attribute attributeArg attributeArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "attribute"
            , annotation =
                Just
                    (Type.function
                        [ Type.string, Type.string ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string attributeArg, Elm.string attributeArg0 ]


{-| Transform the messages produced by an `Attribute`.

map: (a -> msg) -> Html.Styled.Attribute a -> Html.Styled.Attribute msg
-}
map : (Elm.Expression -> Elm.Expression) -> Elm.Expression -> Elm.Expression
map mapArg mapArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "map"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.var "a" ] (Type.var "msg")
                        , Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "a" ]
                        ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.functionReduced "mapUnpack" mapArg, mapArg0 ]


{-| **NOTE:** If you're using `elm-css`, you **probably do not want this!**

This is the `style` **attribute**, which has higher precedence than anything
`elm-css` does. It's probably best to use this as a workaround only.

    Specify a style.

    greeting : Node msg
    greeting =
      div
        [ style "background-color" "red"
        , style "height" "90px"
        , style "width" "100%"
        ]
        [ text "Hello!"
        ]

There is no `Html.Styles` module because best practices for working with HTML
suggest that this should primarily be specified in CSS files. So the general
recommendation is to use this function lightly.

style: String -> String -> Html.Styled.Attribute msg
-}
style : String -> String -> Elm.Expression
style styleArg styleArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "style"
            , annotation =
                Just
                    (Type.function
                        [ Type.string, Type.string ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string styleArg, Elm.string styleArg0 ]


{-| Often used with CSS to style elements with common properties.

**Note:** You can have as many `class` and `classList` attributes as you want.
They all get applied, so if you say `[ class "notice", class "notice-seen" ]`
you will get both classes!

class: String -> Html.Styled.Attribute msg
-}
class : String -> Elm.Expression
class classArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "class"
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
        [ Elm.string classArg ]


{-| This function makes it easier to build a space-separated class attribute.
Each class can easily be added and removed depending on the boolean value it
is paired with. For example, maybe we want a way to view notices:

    viewNotice : Notice -> Html msg
    viewNotice notice =
        div
            [ classList
                [ ( "notice", True )
                , ( "notice-important", notice.isImportant )
                , ( "notice-seen", notice.isSeen )
                ]
            ]
            [ text notice.content ]

**Note:** You can have as many `class` and `classList` attributes as you want.
They all get applied, so if you say `[ class "notice", class "notice-seen" ]`
you will get both classes!

classList: List ( String, Bool ) -> Html.Styled.Attribute msg
-}
classList : List Elm.Expression -> Elm.Expression
classList classListArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "classList"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.tuple Type.string Type.bool) ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list classListArg ]


{-| Often used with CSS to style a specific element. The value of this
attribute must be unique.

id: String -> Html.Styled.Attribute msg
-}
id : String -> Elm.Expression
id idArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "id"
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
        [ Elm.string idArg ]


{-| Text to be displayed in a tooltip when hovering over the element.

title: String -> Html.Styled.Attribute msg
-}
title : String -> Elm.Expression
title titleArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "title"
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
        [ Elm.string titleArg ]


{-| Indicates the relevance of an element.

hidden: Bool -> Html.Styled.Attribute msg
-}
hidden : Bool -> Elm.Expression
hidden hiddenArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
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


{-| Defines the type of a `button`, `input`, `embed`, `object`, `script`,
`source`, `style`, or `menu`.

type_: String -> Html.Styled.Attribute msg
-}
type_ : String -> Elm.Expression
type_ type_Arg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "type_"
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
        [ Elm.string type_Arg ]


{-| Defines a default value which will be displayed in a `button`, `option`,
`input`, `li`, `meter`, `progress`, or `param`.

value: String -> Html.Styled.Attribute msg
-}
value : String -> Elm.Expression
value valueArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "value"
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
        [ Elm.string valueArg ]


{-| Indicates whether an `input` of type checkbox is checked.

checked: Bool -> Html.Styled.Attribute msg
-}
checked : Bool -> Elm.Expression
checked checkedArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "checked"
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
        [ Elm.bool checkedArg ]


{-| Provides a hint to the user of what can be entered into an `input` or
`textarea`.

placeholder: String -> Html.Styled.Attribute msg
-}
placeholder : String -> Elm.Expression
placeholder placeholderArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
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


{-| Defines which `option` will be selected on page load.

selected: Bool -> Html.Styled.Attribute msg
-}
selected : Bool -> Elm.Expression
selected selectedArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
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


{-| List of types the server accepts, typically a file type.
For `form` and `input`.

accept: String -> Html.Styled.Attribute msg
-}
accept : String -> Elm.Expression
accept acceptArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "accept"
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
        [ Elm.string acceptArg ]


{-| List of supported charsets in a `form`.

acceptCharset: String -> Html.Styled.Attribute msg
-}
acceptCharset : String -> Elm.Expression
acceptCharset acceptCharsetArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "acceptCharset"
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
        [ Elm.string acceptCharsetArg ]


{-| The URI of a program that processes the information submitted via a `form`.

action: String -> Html.Styled.Attribute msg
-}
action : String -> Elm.Expression
action actionArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "action"
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
        [ Elm.string actionArg ]


{-| Indicates whether a `form` or an `input` can have their values automatically
completed by the browser.

autocomplete: Bool -> Html.Styled.Attribute msg
-}
autocomplete : Bool -> Elm.Expression
autocomplete autocompleteArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "autocomplete"
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
        [ Elm.bool autocompleteArg ]


{-| The element should be automatically focused after the page loaded.
For `button`, `input`, `select`, and `textarea`.

autofocus: Bool -> Html.Styled.Attribute msg
-}
autofocus : Bool -> Elm.Expression
autofocus autofocusArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "autofocus"
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
        [ Elm.bool autofocusArg ]


{-| Indicates whether the user can interact with a `button`, `fieldset`,
`input`, `optgroup`, `option`, `select` or `textarea`.

disabled: Bool -> Html.Styled.Attribute msg
-}
disabled : Bool -> Elm.Expression
disabled disabledArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
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


{-| How `form` data should be encoded when submitted with the POST method.
Options include: application/x-www-form-urlencoded, multipart/form-data, and
text/plain.

enctype: String -> Html.Styled.Attribute msg
-}
enctype : String -> Elm.Expression
enctype enctypeArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "enctype"
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
        [ Elm.string enctypeArg ]


{-| Associates an `input` with a `datalist` tag. The datalist gives some
pre-defined options to suggest to the user as they interact with an input.
The value of the list attribute must match the id of a `datalist` node.
For `input`.

list: String -> Html.Styled.Attribute msg
-}
list : String -> Elm.Expression
list listArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "list"
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
        [ Elm.string listArg ]


{-| Defines the maximum number of characters allowed in an `input` or
`textarea`.

maxlength: Int -> Html.Styled.Attribute msg
-}
maxlength : Int -> Elm.Expression
maxlength maxlengthArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "maxlength"
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
        [ Elm.int maxlengthArg ]


{-| Defines the minimum number of characters allowed in an `input` or
`textarea`.

minlength: Int -> Html.Styled.Attribute msg
-}
minlength : Int -> Elm.Expression
minlength minlengthArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "minlength"
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
        [ Elm.int minlengthArg ]


{-| Defines which HTTP method to use when submitting a `form`. Can be GET
(default) or POST.

method: String -> Html.Styled.Attribute msg
-}
method : String -> Elm.Expression
method methodArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "method"
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
        [ Elm.string methodArg ]


{-| Indicates whether multiple values can be entered in an `input` of type
email or file. Can also indicate that you can `select` many options.

multiple: Bool -> Html.Styled.Attribute msg
-}
multiple : Bool -> Elm.Expression
multiple multipleArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "multiple"
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
        [ Elm.bool multipleArg ]


{-| Name of the element. For example used by the server to identify the fields
in form submits. For `button`, `form`, `fieldset`, `iframe`, `input`,
`object`, `output`, `select`, `textarea`, `map`, `meta`, and `param`.

name: String -> Html.Styled.Attribute msg
-}
name : String -> Elm.Expression
name nameArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "name"
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
        [ Elm.string nameArg ]


{-| This attribute indicates that a `form` shouldn't be validated when
submitted.

novalidate: Bool -> Html.Styled.Attribute msg
-}
novalidate : Bool -> Elm.Expression
novalidate novalidateArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "novalidate"
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
        [ Elm.bool novalidateArg ]


{-| Defines a regular expression which an `input`'s value will be validated
against.

pattern: String -> Html.Styled.Attribute msg
-}
pattern : String -> Elm.Expression
pattern patternArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "pattern"
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
        [ Elm.string patternArg ]


{-| Indicates whether an `input` or `textarea` can be edited.

readonly: Bool -> Html.Styled.Attribute msg
-}
readonly : Bool -> Elm.Expression
readonly readonlyArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "readonly"
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
        [ Elm.bool readonlyArg ]


{-| Indicates whether this element is required to fill out or not.
For `input`, `select`, and `textarea`.

required: Bool -> Html.Styled.Attribute msg
-}
required : Bool -> Elm.Expression
required requiredArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
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


{-| For `input` specifies the width of an input in characters.

For `select` specifies the number of visible options in a drop-down list.

size: Int -> Html.Styled.Attribute msg
-}
size : Int -> Elm.Expression
size sizeArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "size"
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
        [ Elm.int sizeArg ]


{-| The element ID described by this `label` or the element IDs that are used
for an `output`.

for: String -> Html.Styled.Attribute msg
-}
for : String -> Elm.Expression
for forArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "for"
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
        [ Elm.string forArg ]


{-| Indicates the element ID of the `form` that owns this particular `button`,
`fieldset`, `input`, `label`, `meter`, `object`, `output`, `progress`,
`select`, or `textarea`.

form: String -> Html.Styled.Attribute msg
-}
form : String -> Elm.Expression
form formArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "form"
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
        [ Elm.string formArg ]


{-| Indicates the maximum value allowed. When using an input of type number or
date, the max value must be a number or date. For `input`, `meter`, and `progress`.

max: String -> Html.Styled.Attribute msg
-}
max : String -> Elm.Expression
max maxArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "max"
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
        [ Elm.string maxArg ]


{-| Indicates the minimum value allowed. When using an input of type number or
date, the min value must be a number or date. For `input` and `meter`.

min: String -> Html.Styled.Attribute msg
-}
min : String -> Elm.Expression
min minArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "min"
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
        [ Elm.string minArg ]


{-| Add a step size to an `input`. Use `step "any"` to allow any floating-point
number to be used in the input.

step: String -> Html.Styled.Attribute msg
-}
step : String -> Elm.Expression
step stepArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "step"
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
        [ Elm.string stepArg ]


{-| Defines the number of columns in a `textarea`.

cols: Int -> Html.Styled.Attribute msg
-}
cols : Int -> Elm.Expression
cols colsArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "cols"
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
        [ Elm.int colsArg ]


{-| Defines the number of rows in a `textarea`.

rows: Int -> Html.Styled.Attribute msg
-}
rows : Int -> Elm.Expression
rows rowsArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "rows"
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
        [ Elm.int rowsArg ]


{-| Indicates whether the text should be wrapped in a `textarea`. Possible
values are "hard" and "soft".

wrap: String -> Html.Styled.Attribute msg
-}
wrap : String -> Elm.Expression
wrap wrapArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "wrap"
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
        [ Elm.string wrapArg ]


{-| The URL of a linked resource, such as `a`, `area`, `base`, or `link`.

href: String -> Html.Styled.Attribute msg
-}
href : String -> Elm.Expression
href hrefArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "href"
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
        [ Elm.string hrefArg ]


{-| Specify where the results of clicking an `a`, `area`, `base`, or `form`
should appear. Possible special values include:

  - \_blank &mdash; a new window or tab
  - \_self &mdash; the same frame (this is default)
  - \_parent &mdash; the parent frame
  - \_top &mdash; the full body of the window

You can also give the name of any `frame` you have created.

target: String -> Html.Styled.Attribute msg
-}
target : String -> Elm.Expression
target targetArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "target"
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
        [ Elm.string targetArg ]


{-| Indicates that clicking an `a` and `area` will download the resource
directly. The `String` argument determins the name of the downloaded file.
Say the file you are serving is named `hats.json`.

    download "" -- hats.json

    download "my-hats.json" -- my-hats.json

    download "snakes.json" -- snakes.json

The empty `String` says to just name it whatever it was called on the server.

download: String -> Html.Styled.Attribute msg
-}
download : String -> Elm.Expression
download downloadArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "download"
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
        [ Elm.string downloadArg ]


{-| Two-letter language code of the linked resource of an `a`, `area`, or `link`.

hreflang: String -> Html.Styled.Attribute msg
-}
hreflang : String -> Elm.Expression
hreflang hreflangArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "hreflang"
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
        [ Elm.string hreflangArg ]


{-| Specifies a hint of the target media of a `a`, `area`, `link`, `source`,
or `style`.

media: String -> Html.Styled.Attribute msg
-}
media : String -> Elm.Expression
media mediaArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "media"
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
        [ Elm.string mediaArg ]


{-| Specify a URL to send a short POST request to when the user clicks on an
`a` or `area`. Useful for monitoring and tracking.

ping: String -> Html.Styled.Attribute msg
-}
ping : String -> Elm.Expression
ping pingArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "ping"
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
        [ Elm.string pingArg ]


{-| Specifies the relationship of the target object to the link object.
For `a`, `area`, `link`.

rel: String -> Html.Styled.Attribute msg
-}
rel : String -> Elm.Expression
rel relArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "rel"
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
        [ Elm.string relArg ]


{-| When an `img` is a descendant of an `a` tag, the `ismap` attribute
indicates that the click location should be added to the parent `a`'s href as
a query string.

ismap: Bool -> Html.Styled.Attribute msg
-}
ismap : Bool -> Elm.Expression
ismap ismapArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "ismap"
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
        [ Elm.bool ismapArg ]


{-| Specify the hash name reference of a `map` that should be used for an `img`
or `object`. A hash name reference is a hash symbol followed by the element's name or id.
E.g. `"#planet-map"`.

usemap: String -> Html.Styled.Attribute msg
-}
usemap : String -> Elm.Expression
usemap usemapArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "usemap"
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
        [ Elm.string usemapArg ]


{-| Declare the shape of the clickable area in an `a` or `area`. Valid values
include: default, rect, circle, poly. This attribute can be paired with
`coords` to create more particular shapes.

shape: String -> Html.Styled.Attribute msg
-}
shape : String -> Elm.Expression
shape shapeArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "shape"
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
        [ Elm.string shapeArg ]


{-| A set of values specifying the coordinates of the hot-spot region in an
`area`. Needs to be paired with a `shape` attribute to be meaningful.

coords: String -> Html.Styled.Attribute msg
-}
coords : String -> Elm.Expression
coords coordsArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "coords"
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
        [ Elm.string coordsArg ]


{-| The URL of the embeddable content. For `audio`, `embed`, `iframe`, `img`,
`input`, `script`, `source`, `track`, and `video`.

src: String -> Html.Styled.Attribute msg
-}
src : String -> Elm.Expression
src srcArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "src"
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
        [ Elm.string srcArg ]


{-| Declare the height of a `canvas`, `embed`, `iframe`, `img`, `input`,
`object`, or `video`.

height: Int -> Html.Styled.Attribute msg
-}
height : Int -> Elm.Expression
height heightArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "height"
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
        [ Elm.int heightArg ]


{-| Declare the width of a `canvas`, `embed`, `iframe`, `img`, `input`,
`object`, or `video`.

width: Int -> Html.Styled.Attribute msg
-}
width : Int -> Elm.Expression
width widthArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "width"
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
        [ Elm.int widthArg ]


{-| Alternative text in case an image can't be displayed. Works with `img`,
`area`, and `input`.

alt: String -> Html.Styled.Attribute msg
-}
alt : String -> Elm.Expression
alt altArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "alt"
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
        [ Elm.string altArg ]


{-| The `audio` or `video` should play as soon as possible.

autoplay: Bool -> Html.Styled.Attribute msg
-}
autoplay : Bool -> Elm.Expression
autoplay autoplayArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "autoplay"
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
        [ Elm.bool autoplayArg ]


{-| Indicates whether the browser should show playback controls for the `audio`
or `video`.

controls: Bool -> Html.Styled.Attribute msg
-}
controls : Bool -> Elm.Expression
controls controlsArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "controls"
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
        [ Elm.bool controlsArg ]


{-| Indicates whether the `audio` or `video` should start playing from the
start when it's finished.

loop: Bool -> Html.Styled.Attribute msg
-}
loop : Bool -> Elm.Expression
loop loopArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "loop"
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
        [ Elm.bool loopArg ]


{-| Control how much of an `audio` or `video` resource should be preloaded.

preload: String -> Html.Styled.Attribute msg
-}
preload : String -> Elm.Expression
preload preloadArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "preload"
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
        [ Elm.string preloadArg ]


{-| A URL indicating a poster frame to show until the user plays or seeks the
`video`.

poster: String -> Html.Styled.Attribute msg
-}
poster : String -> Elm.Expression
poster posterArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "poster"
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
        [ Elm.string posterArg ]


{-| Indicates that the `track` should be enabled unless the user's preferences
indicate something different.

default: Bool -> Html.Styled.Attribute msg
-}
default : Bool -> Elm.Expression
default defaultArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "default"
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
        [ Elm.bool defaultArg ]


{-| Specifies the kind of text `track`.

kind: String -> Html.Styled.Attribute msg
-}
kind : String -> Elm.Expression
kind kindArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "kind"
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
        [ Elm.string kindArg ]


{-| A two letter language code indicating the language of the `track` text data.

srclang: String -> Html.Styled.Attribute msg
-}
srclang : String -> Elm.Expression
srclang srclangArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "srclang"
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
        [ Elm.string srclangArg ]


{-| A space separated list of security restrictions you'd like to lift for an
`iframe`.

sandbox: String -> Html.Styled.Attribute msg
-}
sandbox : String -> Elm.Expression
sandbox sandboxArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "sandbox"
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
        [ Elm.string sandboxArg ]


{-| An HTML document that will be displayed as the body of an `iframe`. It will
override the content of the `src` attribute if it has been specified.

srcdoc: String -> Html.Styled.Attribute msg
-}
srcdoc : String -> Elm.Expression
srcdoc srcdocArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "srcdoc"
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
        [ Elm.string srcdocArg ]


{-| Indicates whether an ordered list `ol` should be displayed in a descending
order instead of a ascending.

reversed: Bool -> Html.Styled.Attribute msg
-}
reversed : Bool -> Elm.Expression
reversed reversedArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "reversed"
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
        [ Elm.bool reversedArg ]


{-| Defines the first number of an ordered list if you want it to be something
besides 1.

start: Int -> Html.Styled.Attribute msg
-}
start : Int -> Elm.Expression
start startArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "start"
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
        [ Elm.int startArg ]


{-| Specifies the horizontal alignment of a `caption`, `col`, `colgroup`,
`hr`, `iframe`, `img`, `table`, `tbody`, `td`, `tfoot`, `th`, `thead`, or
`tr`.

align: String -> Html.Styled.Attribute msg
-}
align : String -> Elm.Expression
align alignArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "align"
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
        [ Elm.string alignArg ]


{-| The colspan attribute defines the number of columns a cell should span.
For `td` and `th`.

colspan: Int -> Html.Styled.Attribute msg
-}
colspan : Int -> Elm.Expression
colspan colspanArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "colspan"
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
        [ Elm.int colspanArg ]


{-| Defines the number of rows a table cell should span over.
For `td` and `th`.

rowspan: Int -> Html.Styled.Attribute msg
-}
rowspan : Int -> Elm.Expression
rowspan rowspanArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "rowspan"
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
        [ Elm.int rowspanArg ]


{-| A space separated list of element IDs indicating which `th` elements are
headers for this cell. For `td` and `th`.

headers: String -> Html.Styled.Attribute msg
-}
headers : String -> Elm.Expression
headers headersArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "headers"
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
        [ Elm.string headersArg ]


{-| Specifies the scope of a header cell `th`. Possible values are: col, row,
colgroup, rowgroup.

scope: String -> Html.Styled.Attribute msg
-}
scope : String -> Elm.Expression
scope scopeArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "scope"
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
        [ Elm.string scopeArg ]


{-| Defines a keyboard shortcut to activate or add focus to the element.

accesskey: Char.Char -> Html.Styled.Attribute msg
-}
accesskey : Char.Char -> Elm.Expression
accesskey accesskeyArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "accesskey"
            , annotation =
                Just
                    (Type.function
                        [ Type.char ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.char accesskeyArg ]


{-| Indicates whether the element's content is editable.

contenteditable: Bool -> Html.Styled.Attribute msg
-}
contenteditable : Bool -> Elm.Expression
contenteditable contenteditableArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "contenteditable"
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
        [ Elm.bool contenteditableArg ]


{-| Defines the ID of a `menu` element which will serve as the element's
context menu.

contextmenu: String -> Html.Styled.Attribute msg
-}
contextmenu : String -> Elm.Expression
contextmenu contextmenuArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "contextmenu"
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
        [ Elm.string contextmenuArg ]


{-| Defines the text direction. Allowed values are ltr (Left-To-Right) or rtl
(Right-To-Left).

dir: String -> Html.Styled.Attribute msg
-}
dir : String -> Elm.Expression
dir dirArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "dir"
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
        [ Elm.string dirArg ]


{-| Defines whether the element can be dragged.

draggable: String -> Html.Styled.Attribute msg
-}
draggable : String -> Elm.Expression
draggable draggableArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "draggable"
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
        [ Elm.string draggableArg ]


{-| Indicates that the element accept the dropping of content on it.

dropzone: String -> Html.Styled.Attribute msg
-}
dropzone : String -> Elm.Expression
dropzone dropzoneArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "dropzone"
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
        [ Elm.string dropzoneArg ]


{-| itemprop: String -> Html.Styled.Attribute msg -}
itemprop : String -> Elm.Expression
itemprop itempropArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "itemprop"
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
        [ Elm.string itempropArg ]


{-| Defines the language used in the element.

lang: String -> Html.Styled.Attribute msg
-}
lang : String -> Elm.Expression
lang langArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "lang"
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
        [ Elm.string langArg ]


{-| Indicates whether spell checking is allowed for the element.

spellcheck: Bool -> Html.Styled.Attribute msg
-}
spellcheck : Bool -> Elm.Expression
spellcheck spellcheckArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "spellcheck"
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
        [ Elm.bool spellcheckArg ]


{-| Overrides the browser's default tab order and follows the one specified
instead.

tabindex: Int -> Html.Styled.Attribute msg
-}
tabindex : Int -> Elm.Expression
tabindex tabindexArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "tabindex"
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
        [ Elm.int tabindexArg ]


{-| Contains a URI which points to the source of the quote or change in a
`blockquote`, `del`, `ins`, or `q`.

cite: String -> Html.Styled.Attribute msg
-}
cite : String -> Elm.Expression
cite citeArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "cite"
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
        [ Elm.string citeArg ]


{-| Indicates the date and time associated with the element.
For `del`, `ins`, `time`.

datetime: String -> Html.Styled.Attribute msg
-}
datetime : String -> Elm.Expression
datetime datetimeArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "datetime"
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
        [ Elm.string datetimeArg ]


{-| Indicates whether this date and time is the date of the nearest `article`
ancestor element. For `time`.

pubdate: String -> Html.Styled.Attribute msg
-}
pubdate : String -> Elm.Expression
pubdate pubdateArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "pubdate"
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
        [ Elm.string pubdateArg ]


{-| Specifies the URL of the cache manifest for an `html` tag.

manifest: String -> Html.Styled.Attribute msg
-}
manifest : String -> Elm.Expression
manifest manifestArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "manifest"
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
        [ Elm.string manifestArg ]


call_ :
    { css : Elm.Expression -> Elm.Expression
    , fromUnstyled : Elm.Expression -> Elm.Expression
    , property : Elm.Expression -> Elm.Expression -> Elm.Expression
    , attribute : Elm.Expression -> Elm.Expression -> Elm.Expression
    , map : Elm.Expression -> Elm.Expression -> Elm.Expression
    , style : Elm.Expression -> Elm.Expression -> Elm.Expression
    , class : Elm.Expression -> Elm.Expression
    , classList : Elm.Expression -> Elm.Expression
    , id : Elm.Expression -> Elm.Expression
    , title : Elm.Expression -> Elm.Expression
    , hidden : Elm.Expression -> Elm.Expression
    , type_ : Elm.Expression -> Elm.Expression
    , value : Elm.Expression -> Elm.Expression
    , checked : Elm.Expression -> Elm.Expression
    , placeholder : Elm.Expression -> Elm.Expression
    , selected : Elm.Expression -> Elm.Expression
    , accept : Elm.Expression -> Elm.Expression
    , acceptCharset : Elm.Expression -> Elm.Expression
    , action : Elm.Expression -> Elm.Expression
    , autocomplete : Elm.Expression -> Elm.Expression
    , autofocus : Elm.Expression -> Elm.Expression
    , disabled : Elm.Expression -> Elm.Expression
    , enctype : Elm.Expression -> Elm.Expression
    , list : Elm.Expression -> Elm.Expression
    , maxlength : Elm.Expression -> Elm.Expression
    , minlength : Elm.Expression -> Elm.Expression
    , method : Elm.Expression -> Elm.Expression
    , multiple : Elm.Expression -> Elm.Expression
    , name : Elm.Expression -> Elm.Expression
    , novalidate : Elm.Expression -> Elm.Expression
    , pattern : Elm.Expression -> Elm.Expression
    , readonly : Elm.Expression -> Elm.Expression
    , required : Elm.Expression -> Elm.Expression
    , size : Elm.Expression -> Elm.Expression
    , for : Elm.Expression -> Elm.Expression
    , form : Elm.Expression -> Elm.Expression
    , max : Elm.Expression -> Elm.Expression
    , min : Elm.Expression -> Elm.Expression
    , step : Elm.Expression -> Elm.Expression
    , cols : Elm.Expression -> Elm.Expression
    , rows : Elm.Expression -> Elm.Expression
    , wrap : Elm.Expression -> Elm.Expression
    , href : Elm.Expression -> Elm.Expression
    , target : Elm.Expression -> Elm.Expression
    , download : Elm.Expression -> Elm.Expression
    , hreflang : Elm.Expression -> Elm.Expression
    , media : Elm.Expression -> Elm.Expression
    , ping : Elm.Expression -> Elm.Expression
    , rel : Elm.Expression -> Elm.Expression
    , ismap : Elm.Expression -> Elm.Expression
    , usemap : Elm.Expression -> Elm.Expression
    , shape : Elm.Expression -> Elm.Expression
    , coords : Elm.Expression -> Elm.Expression
    , src : Elm.Expression -> Elm.Expression
    , height : Elm.Expression -> Elm.Expression
    , width : Elm.Expression -> Elm.Expression
    , alt : Elm.Expression -> Elm.Expression
    , autoplay : Elm.Expression -> Elm.Expression
    , controls : Elm.Expression -> Elm.Expression
    , loop : Elm.Expression -> Elm.Expression
    , preload : Elm.Expression -> Elm.Expression
    , poster : Elm.Expression -> Elm.Expression
    , default : Elm.Expression -> Elm.Expression
    , kind : Elm.Expression -> Elm.Expression
    , srclang : Elm.Expression -> Elm.Expression
    , sandbox : Elm.Expression -> Elm.Expression
    , srcdoc : Elm.Expression -> Elm.Expression
    , reversed : Elm.Expression -> Elm.Expression
    , start : Elm.Expression -> Elm.Expression
    , align : Elm.Expression -> Elm.Expression
    , colspan : Elm.Expression -> Elm.Expression
    , rowspan : Elm.Expression -> Elm.Expression
    , headers : Elm.Expression -> Elm.Expression
    , scope : Elm.Expression -> Elm.Expression
    , accesskey : Elm.Expression -> Elm.Expression
    , contenteditable : Elm.Expression -> Elm.Expression
    , contextmenu : Elm.Expression -> Elm.Expression
    , dir : Elm.Expression -> Elm.Expression
    , draggable : Elm.Expression -> Elm.Expression
    , dropzone : Elm.Expression -> Elm.Expression
    , itemprop : Elm.Expression -> Elm.Expression
    , lang : Elm.Expression -> Elm.Expression
    , spellcheck : Elm.Expression -> Elm.Expression
    , tabindex : Elm.Expression -> Elm.Expression
    , cite : Elm.Expression -> Elm.Expression
    , datetime : Elm.Expression -> Elm.Expression
    , pubdate : Elm.Expression -> Elm.Expression
    , manifest : Elm.Expression -> Elm.Expression
    }
call_ =
    { css =
        \cssArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "css"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith [ "Css" ] "Style" [])
                                ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ cssArg ]
    , fromUnstyled =
        \fromUnstyledArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "fromUnstyled"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "VirtualDom" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ fromUnstyledArg ]
    , property =
        \propertyArg propertyArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "property"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string
                                , Type.namedWith [ "Json", "Encode" ] "Value" []
                                ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ propertyArg, propertyArg0 ]
    , attribute =
        \attributeArg attributeArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "attribute"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string, Type.string ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ attributeArg, attributeArg0 ]
    , map =
        \mapArg mapArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "map"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.var "a" ]
                                    (Type.var "msg")
                                , Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "a" ]
                                ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ mapArg, mapArg0 ]
    , style =
        \styleArg styleArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "style"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string, Type.string ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ styleArg, styleArg0 ]
    , class =
        \classArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "class"
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
                [ classArg ]
    , classList =
        \classListArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "classList"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list (Type.tuple Type.string Type.bool) ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ classListArg ]
    , id =
        \idArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "id"
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
                [ idArg ]
    , title =
        \titleArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "title"
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
                [ titleArg ]
    , hidden =
        \hiddenArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
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
    , type_ =
        \type_Arg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "type_"
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
                [ type_Arg ]
    , value =
        \valueArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "value"
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
                [ valueArg ]
    , checked =
        \checkedArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "checked"
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
                [ checkedArg ]
    , placeholder =
        \placeholderArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
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
    , selected =
        \selectedArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
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
    , accept =
        \acceptArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "accept"
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
                [ acceptArg ]
    , acceptCharset =
        \acceptCharsetArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "acceptCharset"
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
                [ acceptCharsetArg ]
    , action =
        \actionArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "action"
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
                [ actionArg ]
    , autocomplete =
        \autocompleteArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "autocomplete"
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
                [ autocompleteArg ]
    , autofocus =
        \autofocusArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "autofocus"
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
                [ autofocusArg ]
    , disabled =
        \disabledArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
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
    , enctype =
        \enctypeArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "enctype"
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
                [ enctypeArg ]
    , list =
        \listArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "list"
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
                [ listArg ]
    , maxlength =
        \maxlengthArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "maxlength"
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
                [ maxlengthArg ]
    , minlength =
        \minlengthArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "minlength"
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
                [ minlengthArg ]
    , method =
        \methodArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "method"
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
                [ methodArg ]
    , multiple =
        \multipleArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "multiple"
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
                [ multipleArg ]
    , name =
        \nameArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "name"
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
                [ nameArg ]
    , novalidate =
        \novalidateArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "novalidate"
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
                [ novalidateArg ]
    , pattern =
        \patternArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "pattern"
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
                [ patternArg ]
    , readonly =
        \readonlyArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "readonly"
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
                [ readonlyArg ]
    , required =
        \requiredArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
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
    , size =
        \sizeArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "size"
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
                [ sizeArg ]
    , for =
        \forArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "for"
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
                [ forArg ]
    , form =
        \formArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "form"
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
                [ formArg ]
    , max =
        \maxArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "max"
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
                [ maxArg ]
    , min =
        \minArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "min"
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
                [ minArg ]
    , step =
        \stepArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "step"
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
                [ stepArg ]
    , cols =
        \colsArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "cols"
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
                [ colsArg ]
    , rows =
        \rowsArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "rows"
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
                [ rowsArg ]
    , wrap =
        \wrapArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "wrap"
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
                [ wrapArg ]
    , href =
        \hrefArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "href"
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
                [ hrefArg ]
    , target =
        \targetArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "target"
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
                [ targetArg ]
    , download =
        \downloadArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "download"
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
                [ downloadArg ]
    , hreflang =
        \hreflangArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "hreflang"
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
                [ hreflangArg ]
    , media =
        \mediaArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "media"
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
                [ mediaArg ]
    , ping =
        \pingArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "ping"
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
                [ pingArg ]
    , rel =
        \relArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "rel"
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
                [ relArg ]
    , ismap =
        \ismapArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "ismap"
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
                [ ismapArg ]
    , usemap =
        \usemapArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "usemap"
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
                [ usemapArg ]
    , shape =
        \shapeArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "shape"
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
                [ shapeArg ]
    , coords =
        \coordsArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "coords"
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
                [ coordsArg ]
    , src =
        \srcArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "src"
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
                [ srcArg ]
    , height =
        \heightArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "height"
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
                [ heightArg ]
    , width =
        \widthArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "width"
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
                [ widthArg ]
    , alt =
        \altArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "alt"
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
                [ altArg ]
    , autoplay =
        \autoplayArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "autoplay"
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
                [ autoplayArg ]
    , controls =
        \controlsArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "controls"
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
                [ controlsArg ]
    , loop =
        \loopArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "loop"
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
                [ loopArg ]
    , preload =
        \preloadArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "preload"
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
                [ preloadArg ]
    , poster =
        \posterArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "poster"
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
                [ posterArg ]
    , default =
        \defaultArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "default"
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
                [ defaultArg ]
    , kind =
        \kindArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "kind"
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
                [ kindArg ]
    , srclang =
        \srclangArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "srclang"
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
                [ srclangArg ]
    , sandbox =
        \sandboxArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "sandbox"
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
                [ sandboxArg ]
    , srcdoc =
        \srcdocArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "srcdoc"
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
                [ srcdocArg ]
    , reversed =
        \reversedArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "reversed"
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
                [ reversedArg ]
    , start =
        \startArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "start"
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
                [ startArg ]
    , align =
        \alignArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "align"
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
                [ alignArg ]
    , colspan =
        \colspanArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "colspan"
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
                [ colspanArg ]
    , rowspan =
        \rowspanArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "rowspan"
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
                [ rowspanArg ]
    , headers =
        \headersArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "headers"
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
                [ headersArg ]
    , scope =
        \scopeArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "scope"
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
                [ scopeArg ]
    , accesskey =
        \accesskeyArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "accesskey"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.char ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ accesskeyArg ]
    , contenteditable =
        \contenteditableArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "contenteditable"
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
                [ contenteditableArg ]
    , contextmenu =
        \contextmenuArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "contextmenu"
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
                [ contextmenuArg ]
    , dir =
        \dirArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "dir"
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
                [ dirArg ]
    , draggable =
        \draggableArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "draggable"
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
                [ draggableArg ]
    , dropzone =
        \dropzoneArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "dropzone"
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
                [ dropzoneArg ]
    , itemprop =
        \itempropArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "itemprop"
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
                [ itempropArg ]
    , lang =
        \langArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "lang"
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
                [ langArg ]
    , spellcheck =
        \spellcheckArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "spellcheck"
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
                [ spellcheckArg ]
    , tabindex =
        \tabindexArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "tabindex"
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
                [ tabindexArg ]
    , cite =
        \citeArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "cite"
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
                [ citeArg ]
    , datetime =
        \datetimeArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "datetime"
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
                [ datetimeArg ]
    , pubdate =
        \pubdateArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "pubdate"
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
                [ pubdateArg ]
    , manifest =
        \manifestArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Html", "Styled", "Attributes" ]
                    , name = "manifest"
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
                [ manifestArg ]
    }


values_ :
    { css : Elm.Expression
    , fromUnstyled : Elm.Expression
    , property : Elm.Expression
    , attribute : Elm.Expression
    , map : Elm.Expression
    , style : Elm.Expression
    , class : Elm.Expression
    , classList : Elm.Expression
    , id : Elm.Expression
    , title : Elm.Expression
    , hidden : Elm.Expression
    , type_ : Elm.Expression
    , value : Elm.Expression
    , checked : Elm.Expression
    , placeholder : Elm.Expression
    , selected : Elm.Expression
    , accept : Elm.Expression
    , acceptCharset : Elm.Expression
    , action : Elm.Expression
    , autocomplete : Elm.Expression
    , autofocus : Elm.Expression
    , disabled : Elm.Expression
    , enctype : Elm.Expression
    , list : Elm.Expression
    , maxlength : Elm.Expression
    , minlength : Elm.Expression
    , method : Elm.Expression
    , multiple : Elm.Expression
    , name : Elm.Expression
    , novalidate : Elm.Expression
    , pattern : Elm.Expression
    , readonly : Elm.Expression
    , required : Elm.Expression
    , size : Elm.Expression
    , for : Elm.Expression
    , form : Elm.Expression
    , max : Elm.Expression
    , min : Elm.Expression
    , step : Elm.Expression
    , cols : Elm.Expression
    , rows : Elm.Expression
    , wrap : Elm.Expression
    , href : Elm.Expression
    , target : Elm.Expression
    , download : Elm.Expression
    , hreflang : Elm.Expression
    , media : Elm.Expression
    , ping : Elm.Expression
    , rel : Elm.Expression
    , ismap : Elm.Expression
    , usemap : Elm.Expression
    , shape : Elm.Expression
    , coords : Elm.Expression
    , src : Elm.Expression
    , height : Elm.Expression
    , width : Elm.Expression
    , alt : Elm.Expression
    , autoplay : Elm.Expression
    , controls : Elm.Expression
    , loop : Elm.Expression
    , preload : Elm.Expression
    , poster : Elm.Expression
    , default : Elm.Expression
    , kind : Elm.Expression
    , srclang : Elm.Expression
    , sandbox : Elm.Expression
    , srcdoc : Elm.Expression
    , reversed : Elm.Expression
    , start : Elm.Expression
    , align : Elm.Expression
    , colspan : Elm.Expression
    , rowspan : Elm.Expression
    , headers : Elm.Expression
    , scope : Elm.Expression
    , accesskey : Elm.Expression
    , contenteditable : Elm.Expression
    , contextmenu : Elm.Expression
    , dir : Elm.Expression
    , draggable : Elm.Expression
    , dropzone : Elm.Expression
    , itemprop : Elm.Expression
    , lang : Elm.Expression
    , spellcheck : Elm.Expression
    , tabindex : Elm.Expression
    , cite : Elm.Expression
    , datetime : Elm.Expression
    , pubdate : Elm.Expression
    , manifest : Elm.Expression
    }
values_ =
    { css =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "css"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , fromUnstyled =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "fromUnstyled"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "VirtualDom" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , property =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "property"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.namedWith [ "Json", "Encode" ] "Value" []
                        ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , attribute =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "attribute"
            , annotation =
                Just
                    (Type.function
                        [ Type.string, Type.string ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , map =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "map"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.var "a" ] (Type.var "msg")
                        , Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "a" ]
                        ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , style =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "style"
            , annotation =
                Just
                    (Type.function
                        [ Type.string, Type.string ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , class =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "class"
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
    , classList =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "classList"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.tuple Type.string Type.bool) ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , id =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "id"
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
    , title =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "title"
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
    , hidden =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
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
    , type_ =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "type_"
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
    , value =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "value"
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
    , checked =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "checked"
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
    , placeholder =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
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
    , selected =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
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
    , accept =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "accept"
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
    , acceptCharset =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "acceptCharset"
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
    , action =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "action"
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
    , autocomplete =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "autocomplete"
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
    , autofocus =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "autofocus"
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
            { importFrom = [ "Html", "Styled", "Attributes" ]
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
    , enctype =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "enctype"
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
    , list =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "list"
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
    , maxlength =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "maxlength"
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
    , minlength =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "minlength"
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
    , method =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "method"
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
    , multiple =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "multiple"
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
    , name =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "name"
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
    , novalidate =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "novalidate"
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
    , pattern =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "pattern"
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
    , readonly =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "readonly"
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
    , required =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
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
    , size =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "size"
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
    , for =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "for"
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
    , form =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "form"
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
    , max =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "max"
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
    , min =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "min"
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
    , step =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "step"
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
    , cols =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "cols"
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
    , rows =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "rows"
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
    , wrap =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "wrap"
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
    , href =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "href"
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
    , target =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "target"
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
    , download =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "download"
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
    , hreflang =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "hreflang"
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
    , media =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "media"
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
    , ping =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "ping"
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
    , rel =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "rel"
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
    , ismap =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "ismap"
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
    , usemap =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "usemap"
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
    , shape =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "shape"
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
    , coords =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "coords"
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
    , src =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "src"
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
    , height =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "height"
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
    , width =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "width"
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
    , alt =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "alt"
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
    , autoplay =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "autoplay"
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
    , controls =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "controls"
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
    , loop =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "loop"
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
    , preload =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "preload"
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
    , poster =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "poster"
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
    , default =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "default"
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
    , kind =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "kind"
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
    , srclang =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "srclang"
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
    , sandbox =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "sandbox"
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
    , srcdoc =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "srcdoc"
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
    , reversed =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "reversed"
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
    , start =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "start"
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
    , align =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "align"
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
    , colspan =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "colspan"
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
    , rowspan =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "rowspan"
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
    , headers =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "headers"
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
    , scope =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "scope"
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
    , accesskey =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "accesskey"
            , annotation =
                Just
                    (Type.function
                        [ Type.char ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , contenteditable =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "contenteditable"
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
    , contextmenu =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "contextmenu"
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
    , dir =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "dir"
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
    , draggable =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "draggable"
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
    , dropzone =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "dropzone"
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
    , itemprop =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "itemprop"
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
    , lang =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "lang"
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
    , spellcheck =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "spellcheck"
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
    , tabindex =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "tabindex"
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
    , cite =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "cite"
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
    , datetime =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "datetime"
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
    , pubdate =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "pubdate"
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
    , manifest =
        Elm.value
            { importFrom = [ "Html", "Styled", "Attributes" ]
            , name = "manifest"
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
    }


