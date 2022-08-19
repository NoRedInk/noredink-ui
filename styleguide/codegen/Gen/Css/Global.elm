module Gen.Css.Global exposing (a, adjacentSiblings, annotation_, article, aside, audio, blockquote, body, button, call_, canvas, caption, children, circle, class, code, col, colgroup, dd, descendants, details, div, dl, dt, each, ellipse, em, everything, fieldset, footer, form, generalSiblings, global, h1, h2, h3, h4, h5, h6, header, hr, html, i, id, img, input, label, legend, li, line, main_, media, mediaQuery, menu, moduleName_, nav, ol, optgroup, option, p, path, polygon, polyline, pre, progress, q, rect, section, select, selector, small, span, strong, summary, svg, table, tbody, td, textarea, tfoot, th, thead, time, tr, typeSelector, ul, values_, video, withAttribute, withClass)

{-| 
@docs moduleName_, global, class, id, selector, everything, media, mediaQuery, children, descendants, adjacentSiblings, generalSiblings, each, withAttribute, withClass, typeSelector, html, body, article, header, footer, h1, h2, h3, h4, h5, h6, nav, menu, section, aside, time, details, summary, div, hr, li, main_, ol, p, ul, pre, dl, dt, dd, blockquote, a, code, small, span, strong, i, em, q, img, audio, video, canvas, caption, col, colgroup, table, tbody, td, tfoot, th, thead, tr, button, fieldset, form, input, label, legend, optgroup, option, progress, select, textarea, svg, path, rect, circle, ellipse, line, polyline, polygon, annotation_, call_, values_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Css", "Global" ]


{-| Add global styles to the page. This compiles directly to a `<style>` element.

global: List Css.Global.Snippet -> Html.Styled.Html msg
-}
global : List Elm.Expression -> Elm.Expression
global globalArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "global"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                        ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list globalArg ]


{-| A [class selector](https://developer.mozilla.org/en-US/docs/Web/CSS/Class_selectors).

    global
        [ class "login-form-button"
            [ fontWeight normal
            , color (rgb 128 64 32)
            ]
        ]

class: String -> List Css.Preprocess.Style -> Css.Global.Snippet
-}
class : String -> List Elm.Expression -> Elm.Expression
class classArg classArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "class"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.string classArg, Elm.list classArg0 ]


{-| An [id selector](https://developer.mozilla.org/en-US/docs/Web/CSS/ID_selectors).

    global
        [ id "nav-bar"
            [ width 960 px
            , backgroundColor (rgb 123 42 208)
            ]
        ]

id: String -> List Css.Preprocess.Style -> Css.Global.Snippet
-}
id : String -> List Elm.Expression -> Elm.Expression
id idArg idArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "id"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.string idArg, Elm.list idArg0 ]


{-| A custom selector. Use this for things like
[attribute selectors](https://developer.mozilla.org/en-US/docs/Web/CSS/Attribute_selectors)
and [universal selectors](https://developer.mozilla.org/en-US/docs/Web/CSS/Universal_selectors).

    global
        [ selector "* [lang^=en]"
            [ textDecoration underline
            , color (rgb 7 7 7)
            ]
        ]

selector: String -> List Css.Preprocess.Style -> Css.Global.Snippet
-}
selector : String -> List Elm.Expression -> Elm.Expression
selector selectorArg selectorArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "selector"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.string selectorArg, Elm.list selectorArg0 ]


{-| A [`*` selector](https://developer.mozilla.org/en-US/docs/Web/CSS/Universal_selectors).

    class "Foo"
        [ children
            [ everything
                [ color (rgb 14 15 16)
                , borderRadius (px 5)
                ]
            ]
        ]

...compiles to:

    .Foo > * {
      color: rgb(14, 15, 16);
      border-radius: 5px;
    }

everything: List Css.Preprocess.Style -> Css.Global.Snippet
-}
everything : List Elm.Expression -> Elm.Expression
everything everythingArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "everything"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list everythingArg ]


{-| Combines media queries into a `@media` rule.

    global
        [ media [ only screen [ Media.minWidth (px 300) ] ]
            [ footer [ Css.maxWidth (px 300) ] ]
        ]

The above code translates into the following CSS.

```css
@media screen and (min-width: 300px) {
    footer {
        max-width: 300px;
    }
}
```

media: List Css.Media.MediaQuery -> List Css.Global.Snippet -> Css.Global.Snippet
-}
media : List Elm.Expression -> List Elm.Expression -> Elm.Expression
media mediaArg mediaArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "media"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Media" ] "MediaQuery" [])
                        , Type.list
                            (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list mediaArg, Elm.list mediaArg0 ]


{-| Manually specify a `@media` rule using a List of strings.

    mediaQuery [ "screen and (min-width: 320px)", "screen and (max-height: 400px)" ]
        [ body [ fontSize (px 14) ] ]

The above code translates into the following CSS.

```css
@media screen and (min-width: 320px), screen and (max-height: 400px) {
    body {
        font-size: 14px;
    }
}
```

mediaQuery: List String -> List Css.Global.Snippet -> Css.Global.Snippet
-}
mediaQuery : List String -> List Elm.Expression -> Elm.Expression
mediaQuery mediaQueryArg mediaQueryArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "mediaQuery"
            , annotation =
                Just
                    (Type.function
                        [ Type.list Type.string
                        , Type.list
                            (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list (List.map Elm.string mediaQueryArg)
        , Elm.list mediaQueryArg0
        ]


{-| Apply styles in a list of snippets to the direct children of a selector

    typeSelector "div"
        [ children
            [ typeSelector "p"
                [ fontSize (px 14)
                ]
            ]
        ]

The above code translates into the following CSS.

```css
div > p {
    font-size: 14px;
}
```

children: List Css.Global.Snippet -> Css.Preprocess.Style
-}
children : List Elm.Expression -> Elm.Expression
children childrenArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "children"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                        ]
                        (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                    )
            }
        )
        [ Elm.list childrenArg ]


{-| Apply styles in a list of snippets to the descendants of a selector

    typeSelector "div"
        [ descendants
            [ typeSelector "p"
                [ fontSize (px 14)
                ]
            ]
        ]

The above code translates into the following CSS.

```css
div p {
    font-size: 14px;
}
```

descendants: List Css.Global.Snippet -> Css.Preprocess.Style
-}
descendants : List Elm.Expression -> Elm.Expression
descendants descendantsArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "descendants"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                        ]
                        (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                    )
            }
        )
        [ Elm.list descendantsArg ]


{-| Apply styles in a list of snippets to the adjacent siblings of a selector

    typeSelector "div"
        [ adjacentSiblings
            [ typeSelector "p"
                [ fontSize (px 14)
                ]
            ]
        ]

The above code translates into the following CSS.

```css
div + p {
    font-size: 14px;
}
```

adjacentSiblings: List Css.Global.Snippet -> Css.Preprocess.Style
-}
adjacentSiblings : List Elm.Expression -> Elm.Expression
adjacentSiblings adjacentSiblingsArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "adjacentSiblings"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                        ]
                        (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                    )
            }
        )
        [ Elm.list adjacentSiblingsArg ]


{-| Apply styles in a list of snippets to the general siblings of a selector

    typeSelector "div"
        [ generalSiblings
            [ typeSelector "p"
                [ fontSize (px 14)
                ]
            ]
        ]

The above code translates into the following CSS.

```css
div ~ p {
    font-size: 14px;
}
```

generalSiblings: List Css.Global.Snippet -> Css.Preprocess.Style
-}
generalSiblings : List Elm.Expression -> Elm.Expression
generalSiblings generalSiblingsArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "generalSiblings"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                        ]
                        (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                    )
            }
        )
        [ Elm.list generalSiblingsArg ]


{-| Apply a list of styles to multiple selectors

    each [ typeSelector "div", typeSelector "p" ]
        [ fontSize (px 14)
        ]

The above code translates into the following CSS.

```css
div {
    font-size: 14px;
}

p {
    font-size: 14px;
}
```

each: 
    List (List Css.Preprocess.Style -> Css.Global.Snippet)
    -> List Css.Preprocess.Style
    -> Css.Global.Snippet
-}
each : List Elm.Expression -> List Elm.Expression -> Elm.Expression
each eachArg eachArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "each"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                        , Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list eachArg, Elm.list eachArg0 ]


{-| Apply styles to the current selector plus an attribute selector

    typeSelector "div"
        [ fontSize (px 14)
        , withAttribute "data-some-attribute"
            [ display none
            ]
        , withAttribute "data-hello=world"
            [ color red
            ]
        ]

The above code translates into the following CSS.

```css
div {
    font-size: 14px;
}

div[data-some-attribute] {
    display: none;
}

div[data-hello=world] {
    color: red;
}
```

withAttribute: String -> List Css.Preprocess.Style -> Css.Preprocess.Style
-}
withAttribute : String -> List Elm.Expression -> Elm.Expression
withAttribute withAttributeArg withAttributeArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "withAttribute"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                    )
            }
        )
        [ Elm.string withAttributeArg, Elm.list withAttributeArg0 ]


{-| Apply styles to the current selector plus an additional class

    typeSelector "div"
        [ fontSize (px 14)
        , withClass "is-bold"
            [ fontWeight bold
            ]
        ]

The above code translates into the following CSS.

```css
div {
    font-size: 14px;
}

div.is-bold {
    font-weight: bold;
}
```

withClass: String -> List Css.Preprocess.Style -> Css.Preprocess.Style
-}
withClass : String -> List Elm.Expression -> Elm.Expression
withClass withClassArg withClassArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "withClass"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                    )
            }
        )
        [ Elm.string withClassArg, Elm.list withClassArg0 ]


{-| Define a custom element.

    global
        [ typeSelector "aside" [ display block ]
        ]

...outputs

    aside {
        display: block;
    }

typeSelector: String -> List Css.Preprocess.Style -> Css.Global.Snippet
-}
typeSelector : String -> List Elm.Expression -> Elm.Expression
typeSelector typeSelectorArg typeSelectorArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "typeSelector"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.string typeSelectorArg, Elm.list typeSelectorArg0 ]


{-| Selector for a html element.

html: List Css.Preprocess.Style -> Css.Global.Snippet
-}
html : List Elm.Expression -> Elm.Expression
html htmlArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "html"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list htmlArg ]


{-| Selector for a body element.

body: List Css.Preprocess.Style -> Css.Global.Snippet
-}
body : List Elm.Expression -> Elm.Expression
body bodyArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "body"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list bodyArg ]


{-| Selector for an article element.

article: List Css.Preprocess.Style -> Css.Global.Snippet
-}
article : List Elm.Expression -> Elm.Expression
article articleArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "article"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list articleArg ]


{-| Selector for a header element.

header: List Css.Preprocess.Style -> Css.Global.Snippet
-}
header : List Elm.Expression -> Elm.Expression
header headerArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "header"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list headerArg ]


{-| Selector for a footer element.

footer: List Css.Preprocess.Style -> Css.Global.Snippet
-}
footer : List Elm.Expression -> Elm.Expression
footer footerArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "footer"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list footerArg ]


{-| Selector for an h1 element.

h1: List Css.Preprocess.Style -> Css.Global.Snippet
-}
h1 : List Elm.Expression -> Elm.Expression
h1 h1Arg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "h1"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list h1Arg ]


{-| Selector for an h2 element.

h2: List Css.Preprocess.Style -> Css.Global.Snippet
-}
h2 : List Elm.Expression -> Elm.Expression
h2 h2Arg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "h2"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list h2Arg ]


{-| Selector for an h3 element.

h3: List Css.Preprocess.Style -> Css.Global.Snippet
-}
h3 : List Elm.Expression -> Elm.Expression
h3 h3Arg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "h3"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list h3Arg ]


{-| Selector for an h4 element.

h4: List Css.Preprocess.Style -> Css.Global.Snippet
-}
h4 : List Elm.Expression -> Elm.Expression
h4 h4Arg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "h4"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list h4Arg ]


{-| Selector for an h5 element.

h5: List Css.Preprocess.Style -> Css.Global.Snippet
-}
h5 : List Elm.Expression -> Elm.Expression
h5 h5Arg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "h5"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list h5Arg ]


{-| Selector for an h6 element.

h6: List Css.Preprocess.Style -> Css.Global.Snippet
-}
h6 : List Elm.Expression -> Elm.Expression
h6 h6Arg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "h6"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list h6Arg ]


{-| Selector for a nav element.

nav: List Css.Preprocess.Style -> Css.Global.Snippet
-}
nav : List Elm.Expression -> Elm.Expression
nav navArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "nav"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list navArg ]


{-| Selector for a [menu](https://developer.mozilla.org/en/docs/Web/HTML/Element/menu) element.

menu: List Css.Preprocess.Style -> Css.Global.Snippet
-}
menu : List Elm.Expression -> Elm.Expression
menu menuArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "menu"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list menuArg ]


{-| Selector for a section element.

section: List Css.Preprocess.Style -> Css.Global.Snippet
-}
section : List Elm.Expression -> Elm.Expression
section sectionArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "section"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list sectionArg ]


{-| Selector for a [aside](https://developer.mozilla.org/en/docs/Web/HTML/Element/aside) element.

aside: List Css.Preprocess.Style -> Css.Global.Snippet
-}
aside : List Elm.Expression -> Elm.Expression
aside asideArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "aside"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list asideArg ]


{-| Selector for a [time](https://developer.mozilla.org/en/docs/Web/HTML/Element/time) element.

time: List Css.Preprocess.Style -> Css.Global.Snippet
-}
time : List Elm.Expression -> Elm.Expression
time timeArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "time"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list timeArg ]


{-| Selector for a details element.

details: List Css.Preprocess.Style -> Css.Global.Snippet
-}
details : List Elm.Expression -> Elm.Expression
details detailsArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "details"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list detailsArg ]


{-| Selector for a summary element.

summary: List Css.Preprocess.Style -> Css.Global.Snippet
-}
summary : List Elm.Expression -> Elm.Expression
summary summaryArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "summary"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list summaryArg ]


{-| Selector for a div element.

div: List Css.Preprocess.Style -> Css.Global.Snippet
-}
div : List Elm.Expression -> Elm.Expression
div divArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "div"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list divArg ]


{-| Selector for an hr element.

hr: List Css.Preprocess.Style -> Css.Global.Snippet
-}
hr : List Elm.Expression -> Elm.Expression
hr hrArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "hr"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list hrArg ]


{-| Selector for an li element.

li: List Css.Preprocess.Style -> Css.Global.Snippet
-}
li : List Elm.Expression -> Elm.Expression
li liArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "li"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list liArg ]


{-| Selector for a main element.

main_: List Css.Preprocess.Style -> Css.Global.Snippet
-}
main_ : List Elm.Expression -> Elm.Expression
main_ main_Arg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "main_"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list main_Arg ]


{-| Selector for an ol element.

ol: List Css.Preprocess.Style -> Css.Global.Snippet
-}
ol : List Elm.Expression -> Elm.Expression
ol olArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "ol"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list olArg ]


{-| Selector for a p element.

p: List Css.Preprocess.Style -> Css.Global.Snippet
-}
p : List Elm.Expression -> Elm.Expression
p pArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "p"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list pArg ]


{-| Selector for a ul element.

ul: List Css.Preprocess.Style -> Css.Global.Snippet
-}
ul : List Elm.Expression -> Elm.Expression
ul ulArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "ul"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list ulArg ]


{-| Selector for a pre element.

pre: List Css.Preprocess.Style -> Css.Global.Snippet
-}
pre : List Elm.Expression -> Elm.Expression
pre preArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "pre"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list preArg ]


{-| Selector for a dl element.

    <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dl>

dl: List Css.Preprocess.Style -> Css.Global.Snippet
-}
dl : List Elm.Expression -> Elm.Expression
dl dlArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "dl"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list dlArg ]


{-| Selector for a dt element.

    <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dt>

dt: List Css.Preprocess.Style -> Css.Global.Snippet
-}
dt : List Elm.Expression -> Elm.Expression
dt dtArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "dt"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list dtArg ]


{-| Selector for a dd element.

    <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dd>

dd: List Css.Preprocess.Style -> Css.Global.Snippet
-}
dd : List Elm.Expression -> Elm.Expression
dd ddArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "dd"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list ddArg ]


{-| Selector for a blockquote element.

blockquote: List Css.Preprocess.Style -> Css.Global.Snippet
-}
blockquote : List Elm.Expression -> Elm.Expression
blockquote blockquoteArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "blockquote"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list blockquoteArg ]


{-| Selector for an `<a>` element.

a: List Css.Preprocess.Style -> Css.Global.Snippet
-}
a : List Elm.Expression -> Elm.Expression
a aArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "a"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list aArg ]


{-| Selector for a code element.

code: List Css.Preprocess.Style -> Css.Global.Snippet
-}
code : List Elm.Expression -> Elm.Expression
code codeArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "code"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list codeArg ]


{-| Selector for a small element.

small: List Css.Preprocess.Style -> Css.Global.Snippet
-}
small : List Elm.Expression -> Elm.Expression
small smallArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "small"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list smallArg ]


{-| Selector for a span element.

span: List Css.Preprocess.Style -> Css.Global.Snippet
-}
span : List Elm.Expression -> Elm.Expression
span spanArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "span"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list spanArg ]


{-| Selector for a strong element.

strong: List Css.Preprocess.Style -> Css.Global.Snippet
-}
strong : List Elm.Expression -> Elm.Expression
strong strongArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "strong"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list strongArg ]


{-| Selector for a i element.

i: List Css.Preprocess.Style -> Css.Global.Snippet
-}
i : List Elm.Expression -> Elm.Expression
i iArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "i"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list iArg ]


{-| Selector for a em element.

em: List Css.Preprocess.Style -> Css.Global.Snippet
-}
em : List Elm.Expression -> Elm.Expression
em emArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "em"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list emArg ]


{-| Selector for a [q](https://developer.mozilla.org/en/docs/Web/HTML/Element/q) element.

q: List Css.Preprocess.Style -> Css.Global.Snippet
-}
q : List Elm.Expression -> Elm.Expression
q qArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "q"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list qArg ]


{-| Selector for a img element.

img: List Css.Preprocess.Style -> Css.Global.Snippet
-}
img : List Elm.Expression -> Elm.Expression
img imgArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "img"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list imgArg ]


{-| Selector for an audio element.

audio: List Css.Preprocess.Style -> Css.Global.Snippet
-}
audio : List Elm.Expression -> Elm.Expression
audio audioArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "audio"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list audioArg ]


{-| Selector for a video element.

video: List Css.Preprocess.Style -> Css.Global.Snippet
-}
video : List Elm.Expression -> Elm.Expression
video videoArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "video"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list videoArg ]


{-| Selector for a canvas element.

canvas: List Css.Preprocess.Style -> Css.Global.Snippet
-}
canvas : List Elm.Expression -> Elm.Expression
canvas canvasArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "canvas"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list canvasArg ]


{-| Selector for a caption element.

caption: List Css.Preprocess.Style -> Css.Global.Snippet
-}
caption : List Elm.Expression -> Elm.Expression
caption captionArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "caption"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list captionArg ]


{-| Selector for a col element.

col: List Css.Preprocess.Style -> Css.Global.Snippet
-}
col : List Elm.Expression -> Elm.Expression
col colArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "col"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list colArg ]


{-| Selector for a colgroup element.

colgroup: List Css.Preprocess.Style -> Css.Global.Snippet
-}
colgroup : List Elm.Expression -> Elm.Expression
colgroup colgroupArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "colgroup"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list colgroupArg ]


{-| Selector for a table element.

table: List Css.Preprocess.Style -> Css.Global.Snippet
-}
table : List Elm.Expression -> Elm.Expression
table tableArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "table"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list tableArg ]


{-| Selector for a tbody element.

tbody: List Css.Preprocess.Style -> Css.Global.Snippet
-}
tbody : List Elm.Expression -> Elm.Expression
tbody tbodyArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "tbody"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list tbodyArg ]


{-| Selector for a td element.

td: List Css.Preprocess.Style -> Css.Global.Snippet
-}
td : List Elm.Expression -> Elm.Expression
td tdArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "td"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list tdArg ]


{-| Selector for a tfoot element.

tfoot: List Css.Preprocess.Style -> Css.Global.Snippet
-}
tfoot : List Elm.Expression -> Elm.Expression
tfoot tfootArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "tfoot"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list tfootArg ]


{-| Selector for a th element.

th: List Css.Preprocess.Style -> Css.Global.Snippet
-}
th : List Elm.Expression -> Elm.Expression
th thArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "th"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list thArg ]


{-| Selector for a thead element.

thead: List Css.Preprocess.Style -> Css.Global.Snippet
-}
thead : List Elm.Expression -> Elm.Expression
thead theadArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "thead"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list theadArg ]


{-| Selector for a tr element.

tr: List Css.Preprocess.Style -> Css.Global.Snippet
-}
tr : List Elm.Expression -> Elm.Expression
tr trArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "tr"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list trArg ]


{-| Selector for a button element.

button: List Css.Preprocess.Style -> Css.Global.Snippet
-}
button : List Elm.Expression -> Elm.Expression
button buttonArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "button"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list buttonArg ]


{-| Selector for a fieldset element.

fieldset: List Css.Preprocess.Style -> Css.Global.Snippet
-}
fieldset : List Elm.Expression -> Elm.Expression
fieldset fieldsetArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "fieldset"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list fieldsetArg ]


{-| Selector for a form element.

form: List Css.Preprocess.Style -> Css.Global.Snippet
-}
form : List Elm.Expression -> Elm.Expression
form formArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "form"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list formArg ]


{-| Selector for an input element.

input: List Css.Preprocess.Style -> Css.Global.Snippet
-}
input : List Elm.Expression -> Elm.Expression
input inputArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "input"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list inputArg ]


{-| Selector for a label element.

label: List Css.Preprocess.Style -> Css.Global.Snippet
-}
label : List Elm.Expression -> Elm.Expression
label labelArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "label"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list labelArg ]


{-| Selector for a legend element.

legend: List Css.Preprocess.Style -> Css.Global.Snippet
-}
legend : List Elm.Expression -> Elm.Expression
legend legendArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "legend"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list legendArg ]


{-| Selector for an optgroup element.

optgroup: List Css.Preprocess.Style -> Css.Global.Snippet
-}
optgroup : List Elm.Expression -> Elm.Expression
optgroup optgroupArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "optgroup"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list optgroupArg ]


{-| Selector for an option element.

option: List Css.Preprocess.Style -> Css.Global.Snippet
-}
option : List Elm.Expression -> Elm.Expression
option optionArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "option"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list optionArg ]


{-| Selector for a progress element.

progress: List Css.Preprocess.Style -> Css.Global.Snippet
-}
progress : List Elm.Expression -> Elm.Expression
progress progressArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "progress"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list progressArg ]


{-| Selector for a select element.

select: List Css.Preprocess.Style -> Css.Global.Snippet
-}
select : List Elm.Expression -> Elm.Expression
select selectArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "select"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list selectArg ]


{-| Selector for a textarea element.

textarea: List Css.Preprocess.Style -> Css.Global.Snippet
-}
textarea : List Elm.Expression -> Elm.Expression
textarea textareaArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "textarea"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list textareaArg ]


{-| Selector for a svg element.

svg: List Css.Preprocess.Style -> Css.Global.Snippet
-}
svg : List Elm.Expression -> Elm.Expression
svg svgArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "svg"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list svgArg ]


{-| Selector for a path element.

path: List Css.Preprocess.Style -> Css.Global.Snippet
-}
path : List Elm.Expression -> Elm.Expression
path pathArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "path"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list pathArg ]


{-| Selector for a rect element.

rect: List Css.Preprocess.Style -> Css.Global.Snippet
-}
rect : List Elm.Expression -> Elm.Expression
rect rectArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "rect"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list rectArg ]


{-| Selector for a circle element.

circle: List Css.Preprocess.Style -> Css.Global.Snippet
-}
circle : List Elm.Expression -> Elm.Expression
circle circleArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "circle"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list circleArg ]


{-| Selector for a ellipse element.

ellipse: List Css.Preprocess.Style -> Css.Global.Snippet
-}
ellipse : List Elm.Expression -> Elm.Expression
ellipse ellipseArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "ellipse"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list ellipseArg ]


{-| Selector for a line element.

line: List Css.Preprocess.Style -> Css.Global.Snippet
-}
line : List Elm.Expression -> Elm.Expression
line lineArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "line"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list lineArg ]


{-| Selector for a polyline element.

polyline: List Css.Preprocess.Style -> Css.Global.Snippet
-}
polyline : List Elm.Expression -> Elm.Expression
polyline polylineArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "polyline"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list polylineArg ]


{-| Selector for a polygon element.

polygon: List Css.Preprocess.Style -> Css.Global.Snippet
-}
polygon : List Elm.Expression -> Elm.Expression
polygon polygonArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "polygon"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
        )
        [ Elm.list polygonArg ]


annotation_ : { snippet : Type.Annotation }
annotation_ =
    { snippet =
        Type.alias
            moduleName_
            "Snippet"
            []
            (Type.namedWith [ "Css", "Preprocess" ] "Snippet" [])
    }


call_ :
    { global : Elm.Expression -> Elm.Expression
    , class : Elm.Expression -> Elm.Expression -> Elm.Expression
    , id : Elm.Expression -> Elm.Expression -> Elm.Expression
    , selector : Elm.Expression -> Elm.Expression -> Elm.Expression
    , everything : Elm.Expression -> Elm.Expression
    , media : Elm.Expression -> Elm.Expression -> Elm.Expression
    , mediaQuery : Elm.Expression -> Elm.Expression -> Elm.Expression
    , children : Elm.Expression -> Elm.Expression
    , descendants : Elm.Expression -> Elm.Expression
    , adjacentSiblings : Elm.Expression -> Elm.Expression
    , generalSiblings : Elm.Expression -> Elm.Expression
    , each : Elm.Expression -> Elm.Expression -> Elm.Expression
    , withAttribute : Elm.Expression -> Elm.Expression -> Elm.Expression
    , withClass : Elm.Expression -> Elm.Expression -> Elm.Expression
    , typeSelector : Elm.Expression -> Elm.Expression -> Elm.Expression
    , html : Elm.Expression -> Elm.Expression
    , body : Elm.Expression -> Elm.Expression
    , article : Elm.Expression -> Elm.Expression
    , header : Elm.Expression -> Elm.Expression
    , footer : Elm.Expression -> Elm.Expression
    , h1 : Elm.Expression -> Elm.Expression
    , h2 : Elm.Expression -> Elm.Expression
    , h3 : Elm.Expression -> Elm.Expression
    , h4 : Elm.Expression -> Elm.Expression
    , h5 : Elm.Expression -> Elm.Expression
    , h6 : Elm.Expression -> Elm.Expression
    , nav : Elm.Expression -> Elm.Expression
    , menu : Elm.Expression -> Elm.Expression
    , section : Elm.Expression -> Elm.Expression
    , aside : Elm.Expression -> Elm.Expression
    , time : Elm.Expression -> Elm.Expression
    , details : Elm.Expression -> Elm.Expression
    , summary : Elm.Expression -> Elm.Expression
    , div : Elm.Expression -> Elm.Expression
    , hr : Elm.Expression -> Elm.Expression
    , li : Elm.Expression -> Elm.Expression
    , main_ : Elm.Expression -> Elm.Expression
    , ol : Elm.Expression -> Elm.Expression
    , p : Elm.Expression -> Elm.Expression
    , ul : Elm.Expression -> Elm.Expression
    , pre : Elm.Expression -> Elm.Expression
    , dl : Elm.Expression -> Elm.Expression
    , dt : Elm.Expression -> Elm.Expression
    , dd : Elm.Expression -> Elm.Expression
    , blockquote : Elm.Expression -> Elm.Expression
    , a : Elm.Expression -> Elm.Expression
    , code : Elm.Expression -> Elm.Expression
    , small : Elm.Expression -> Elm.Expression
    , span : Elm.Expression -> Elm.Expression
    , strong : Elm.Expression -> Elm.Expression
    , i : Elm.Expression -> Elm.Expression
    , em : Elm.Expression -> Elm.Expression
    , q : Elm.Expression -> Elm.Expression
    , img : Elm.Expression -> Elm.Expression
    , audio : Elm.Expression -> Elm.Expression
    , video : Elm.Expression -> Elm.Expression
    , canvas : Elm.Expression -> Elm.Expression
    , caption : Elm.Expression -> Elm.Expression
    , col : Elm.Expression -> Elm.Expression
    , colgroup : Elm.Expression -> Elm.Expression
    , table : Elm.Expression -> Elm.Expression
    , tbody : Elm.Expression -> Elm.Expression
    , td : Elm.Expression -> Elm.Expression
    , tfoot : Elm.Expression -> Elm.Expression
    , th : Elm.Expression -> Elm.Expression
    , thead : Elm.Expression -> Elm.Expression
    , tr : Elm.Expression -> Elm.Expression
    , button : Elm.Expression -> Elm.Expression
    , fieldset : Elm.Expression -> Elm.Expression
    , form : Elm.Expression -> Elm.Expression
    , input : Elm.Expression -> Elm.Expression
    , label : Elm.Expression -> Elm.Expression
    , legend : Elm.Expression -> Elm.Expression
    , optgroup : Elm.Expression -> Elm.Expression
    , option : Elm.Expression -> Elm.Expression
    , progress : Elm.Expression -> Elm.Expression
    , select : Elm.Expression -> Elm.Expression
    , textarea : Elm.Expression -> Elm.Expression
    , svg : Elm.Expression -> Elm.Expression
    , path : Elm.Expression -> Elm.Expression
    , rect : Elm.Expression -> Elm.Expression
    , circle : Elm.Expression -> Elm.Expression
    , ellipse : Elm.Expression -> Elm.Expression
    , line : Elm.Expression -> Elm.Expression
    , polyline : Elm.Expression -> Elm.Expression
    , polygon : Elm.Expression -> Elm.Expression
    }
call_ =
    { global =
        \globalArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "global"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Global" ]
                                        "Snippet"
                                        []
                                    )
                                ]
                                (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ globalArg ]
    , class =
        \classArg classArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "class"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string
                                , Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ classArg, classArg0 ]
    , id =
        \idArg idArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "id"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string
                                , Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ idArg, idArg0 ]
    , selector =
        \selectorArg selectorArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "selector"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string
                                , Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ selectorArg, selectorArg0 ]
    , everything =
        \everythingArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "everything"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ everythingArg ]
    , media =
        \mediaArg mediaArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "media"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Media" ]
                                        "MediaQuery"
                                        []
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Css", "Global" ]
                                        "Snippet"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ mediaArg, mediaArg0 ]
    , mediaQuery =
        \mediaQueryArg mediaQueryArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "mediaQuery"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list Type.string
                                , Type.list
                                    (Type.namedWith
                                        [ "Css", "Global" ]
                                        "Snippet"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ mediaQueryArg, mediaQueryArg0 ]
    , children =
        \childrenArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "children"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Global" ]
                                        "Snippet"
                                        []
                                    )
                                ]
                                (Type.namedWith
                                    [ "Css", "Preprocess" ]
                                    "Style"
                                    []
                                )
                            )
                    }
                )
                [ childrenArg ]
    , descendants =
        \descendantsArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "descendants"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Global" ]
                                        "Snippet"
                                        []
                                    )
                                ]
                                (Type.namedWith
                                    [ "Css", "Preprocess" ]
                                    "Style"
                                    []
                                )
                            )
                    }
                )
                [ descendantsArg ]
    , adjacentSiblings =
        \adjacentSiblingsArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "adjacentSiblings"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Global" ]
                                        "Snippet"
                                        []
                                    )
                                ]
                                (Type.namedWith
                                    [ "Css", "Preprocess" ]
                                    "Style"
                                    []
                                )
                            )
                    }
                )
                [ adjacentSiblingsArg ]
    , generalSiblings =
        \generalSiblingsArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "generalSiblings"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Global" ]
                                        "Snippet"
                                        []
                                    )
                                ]
                                (Type.namedWith
                                    [ "Css", "Preprocess" ]
                                    "Style"
                                    []
                                )
                            )
                    }
                )
                [ generalSiblingsArg ]
    , each =
        \eachArg eachArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "each"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.function
                                        [ Type.list
                                            (Type.namedWith
                                                [ "Css", "Preprocess" ]
                                                "Style"
                                                []
                                            )
                                        ]
                                        (Type.namedWith
                                            [ "Css", "Global" ]
                                            "Snippet"
                                            []
                                        )
                                    )
                                , Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ eachArg, eachArg0 ]
    , withAttribute =
        \withAttributeArg withAttributeArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "withAttribute"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string
                                , Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith
                                    [ "Css", "Preprocess" ]
                                    "Style"
                                    []
                                )
                            )
                    }
                )
                [ withAttributeArg, withAttributeArg0 ]
    , withClass =
        \withClassArg withClassArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "withClass"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string
                                , Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith
                                    [ "Css", "Preprocess" ]
                                    "Style"
                                    []
                                )
                            )
                    }
                )
                [ withClassArg, withClassArg0 ]
    , typeSelector =
        \typeSelectorArg typeSelectorArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "typeSelector"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string
                                , Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ typeSelectorArg, typeSelectorArg0 ]
    , html =
        \htmlArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "html"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ htmlArg ]
    , body =
        \bodyArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "body"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ bodyArg ]
    , article =
        \articleArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "article"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ articleArg ]
    , header =
        \headerArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "header"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ headerArg ]
    , footer =
        \footerArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "footer"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ footerArg ]
    , h1 =
        \h1Arg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "h1"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ h1Arg ]
    , h2 =
        \h2Arg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "h2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ h2Arg ]
    , h3 =
        \h3Arg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "h3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ h3Arg ]
    , h4 =
        \h4Arg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "h4"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ h4Arg ]
    , h5 =
        \h5Arg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "h5"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ h5Arg ]
    , h6 =
        \h6Arg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "h6"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ h6Arg ]
    , nav =
        \navArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "nav"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ navArg ]
    , menu =
        \menuArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "menu"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ menuArg ]
    , section =
        \sectionArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "section"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ sectionArg ]
    , aside =
        \asideArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "aside"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ asideArg ]
    , time =
        \timeArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "time"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ timeArg ]
    , details =
        \detailsArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "details"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ detailsArg ]
    , summary =
        \summaryArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "summary"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ summaryArg ]
    , div =
        \divArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "div"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ divArg ]
    , hr =
        \hrArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "hr"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ hrArg ]
    , li =
        \liArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "li"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ liArg ]
    , main_ =
        \main_Arg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "main_"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ main_Arg ]
    , ol =
        \olArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "ol"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ olArg ]
    , p =
        \pArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "p"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ pArg ]
    , ul =
        \ulArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "ul"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ ulArg ]
    , pre =
        \preArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "pre"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ preArg ]
    , dl =
        \dlArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "dl"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ dlArg ]
    , dt =
        \dtArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "dt"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ dtArg ]
    , dd =
        \ddArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "dd"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ ddArg ]
    , blockquote =
        \blockquoteArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "blockquote"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ blockquoteArg ]
    , a =
        \aArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "a"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ aArg ]
    , code =
        \codeArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "code"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ codeArg ]
    , small =
        \smallArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "small"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ smallArg ]
    , span =
        \spanArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "span"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ spanArg ]
    , strong =
        \strongArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "strong"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ strongArg ]
    , i =
        \iArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "i"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ iArg ]
    , em =
        \emArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "em"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ emArg ]
    , q =
        \qArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "q"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ qArg ]
    , img =
        \imgArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "img"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ imgArg ]
    , audio =
        \audioArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "audio"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ audioArg ]
    , video =
        \videoArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "video"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ videoArg ]
    , canvas =
        \canvasArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "canvas"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ canvasArg ]
    , caption =
        \captionArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "caption"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ captionArg ]
    , col =
        \colArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "col"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ colArg ]
    , colgroup =
        \colgroupArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "colgroup"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ colgroupArg ]
    , table =
        \tableArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "table"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ tableArg ]
    , tbody =
        \tbodyArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "tbody"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ tbodyArg ]
    , td =
        \tdArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "td"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ tdArg ]
    , tfoot =
        \tfootArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "tfoot"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ tfootArg ]
    , th =
        \thArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "th"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ thArg ]
    , thead =
        \theadArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "thead"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ theadArg ]
    , tr =
        \trArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "tr"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ trArg ]
    , button =
        \buttonArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "button"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ buttonArg ]
    , fieldset =
        \fieldsetArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "fieldset"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ fieldsetArg ]
    , form =
        \formArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "form"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ formArg ]
    , input =
        \inputArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "input"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ inputArg ]
    , label =
        \labelArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "label"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ labelArg ]
    , legend =
        \legendArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "legend"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ legendArg ]
    , optgroup =
        \optgroupArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "optgroup"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ optgroupArg ]
    , option =
        \optionArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "option"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ optionArg ]
    , progress =
        \progressArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "progress"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ progressArg ]
    , select =
        \selectArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "select"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ selectArg ]
    , textarea =
        \textareaArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "textarea"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ textareaArg ]
    , svg =
        \svgArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "svg"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ svgArg ]
    , path =
        \pathArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "path"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ pathArg ]
    , rect =
        \rectArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "rect"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ rectArg ]
    , circle =
        \circleArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "circle"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ circleArg ]
    , ellipse =
        \ellipseArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "ellipse"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ ellipseArg ]
    , line =
        \lineArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "line"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ lineArg ]
    , polyline =
        \polylineArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "polyline"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ polylineArg ]
    , polygon =
        \polygonArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Css", "Global" ]
                    , name = "polygon"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                    }
                )
                [ polygonArg ]
    }


values_ :
    { global : Elm.Expression
    , class : Elm.Expression
    , id : Elm.Expression
    , selector : Elm.Expression
    , everything : Elm.Expression
    , media : Elm.Expression
    , mediaQuery : Elm.Expression
    , children : Elm.Expression
    , descendants : Elm.Expression
    , adjacentSiblings : Elm.Expression
    , generalSiblings : Elm.Expression
    , each : Elm.Expression
    , withAttribute : Elm.Expression
    , withClass : Elm.Expression
    , typeSelector : Elm.Expression
    , html : Elm.Expression
    , body : Elm.Expression
    , article : Elm.Expression
    , header : Elm.Expression
    , footer : Elm.Expression
    , h1 : Elm.Expression
    , h2 : Elm.Expression
    , h3 : Elm.Expression
    , h4 : Elm.Expression
    , h5 : Elm.Expression
    , h6 : Elm.Expression
    , nav : Elm.Expression
    , menu : Elm.Expression
    , section : Elm.Expression
    , aside : Elm.Expression
    , time : Elm.Expression
    , details : Elm.Expression
    , summary : Elm.Expression
    , div : Elm.Expression
    , hr : Elm.Expression
    , li : Elm.Expression
    , main_ : Elm.Expression
    , ol : Elm.Expression
    , p : Elm.Expression
    , ul : Elm.Expression
    , pre : Elm.Expression
    , dl : Elm.Expression
    , dt : Elm.Expression
    , dd : Elm.Expression
    , blockquote : Elm.Expression
    , a : Elm.Expression
    , code : Elm.Expression
    , small : Elm.Expression
    , span : Elm.Expression
    , strong : Elm.Expression
    , i : Elm.Expression
    , em : Elm.Expression
    , q : Elm.Expression
    , img : Elm.Expression
    , audio : Elm.Expression
    , video : Elm.Expression
    , canvas : Elm.Expression
    , caption : Elm.Expression
    , col : Elm.Expression
    , colgroup : Elm.Expression
    , table : Elm.Expression
    , tbody : Elm.Expression
    , td : Elm.Expression
    , tfoot : Elm.Expression
    , th : Elm.Expression
    , thead : Elm.Expression
    , tr : Elm.Expression
    , button : Elm.Expression
    , fieldset : Elm.Expression
    , form : Elm.Expression
    , input : Elm.Expression
    , label : Elm.Expression
    , legend : Elm.Expression
    , optgroup : Elm.Expression
    , option : Elm.Expression
    , progress : Elm.Expression
    , select : Elm.Expression
    , textarea : Elm.Expression
    , svg : Elm.Expression
    , path : Elm.Expression
    , rect : Elm.Expression
    , circle : Elm.Expression
    , ellipse : Elm.Expression
    , line : Elm.Expression
    , polyline : Elm.Expression
    , polygon : Elm.Expression
    }
values_ =
    { global =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "global"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                        ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , class =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "class"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , id =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "id"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , selector =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "selector"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , everything =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "everything"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , media =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "media"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Media" ] "MediaQuery" [])
                        , Type.list
                            (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , mediaQuery =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "mediaQuery"
            , annotation =
                Just
                    (Type.function
                        [ Type.list Type.string
                        , Type.list
                            (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , children =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "children"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                        ]
                        (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                    )
            }
    , descendants =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "descendants"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                        ]
                        (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                    )
            }
    , adjacentSiblings =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "adjacentSiblings"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                        ]
                        (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                    )
            }
    , generalSiblings =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "generalSiblings"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                        ]
                        (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                    )
            }
    , each =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "each"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Css", "Preprocess" ]
                                        "Style"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Css", "Global" ] "Snippet" []
                                )
                            )
                        , Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , withAttribute =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "withAttribute"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                    )
            }
    , withClass =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "withClass"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                    )
            }
    , typeSelector =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "typeSelector"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , html =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "html"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , body =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "body"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , article =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "article"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , header =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "header"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , footer =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "footer"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , h1 =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "h1"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , h2 =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "h2"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , h3 =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "h3"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , h4 =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "h4"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , h5 =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "h5"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , h6 =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "h6"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , nav =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "nav"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , menu =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "menu"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , section =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "section"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , aside =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "aside"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , time =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "time"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , details =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "details"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , summary =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "summary"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , div =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "div"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , hr =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "hr"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , li =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "li"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , main_ =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "main_"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , ol =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "ol"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , p =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "p"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , ul =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "ul"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , pre =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "pre"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , dl =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "dl"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , dt =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "dt"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , dd =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "dd"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , blockquote =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "blockquote"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , a =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "a"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , code =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "code"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , small =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "small"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , span =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "span"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , strong =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "strong"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , i =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "i"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , em =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "em"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , q =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "q"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , img =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "img"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , audio =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "audio"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , video =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "video"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , canvas =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "canvas"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , caption =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "caption"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , col =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "col"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , colgroup =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "colgroup"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , table =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "table"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , tbody =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "tbody"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , td =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "td"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , tfoot =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "tfoot"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , th =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "th"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , thead =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "thead"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , tr =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "tr"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , button =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "button"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , fieldset =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "fieldset"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , form =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "form"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , input =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "input"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , label =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "label"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , legend =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "legend"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , optgroup =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "optgroup"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , option =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "option"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , progress =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "progress"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , select =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "select"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , textarea =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "textarea"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , svg =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "svg"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , path =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "path"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , rect =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "rect"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , circle =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "circle"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , ellipse =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "ellipse"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , line =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "line"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , polyline =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "polyline"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    , polygon =
        Elm.value
            { importFrom = [ "Css", "Global" ]
            , name = "polygon"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Css", "Preprocess" ] "Style" [])
                        ]
                        (Type.namedWith [ "Css", "Global" ] "Snippet" [])
                    )
            }
    }


