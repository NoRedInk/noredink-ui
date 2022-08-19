module Gen.Accessibility.Styled.Landmark exposing (banner, complementary, contentInfo, form, main_, moduleName_, navigation, region, search, values_)

{-| 
@docs moduleName_, banner, complementary, contentInfo, form, main_, navigation, region, search, values_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Accessibility", "Styled", "Landmark" ]


{-| Creates a [`role="banner"`](https://www.w3.org/TR/wai-aria-1.1/#banner) attribute.

Elements with `banner` should contain the page title. In HTML5, the `header` element is considered a `banner` as long as it's contained by `body` but not a descendant of `article`, `aside`, `main`, `nav`, or `section` ([ARIA Landmarks Banner Example ](https://www.w3.org/TR/wai-aria-practices/examples/landmarks/banner.html), see "HTML Techniques" tab).

    import Accessibility as Html exposing (Html, div, h1, img, text)
    import Accessibility.Landmark exposing (banner)
    import Html.Attributes exposing (src)

    view : Html msg
    view =
        div
            [ banner ]
            [ h1 [] [ text "Such Site!" ]
            , img "Such Logo!" [ src "logo.png" ]
            ]

banner: Html.Styled.Attribute msg
-}
banner : Elm.Expression
banner =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Landmark" ]
        , name = "banner"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Creates a [`role="complementary"`](https://www.w3.org/TR/wai-aria-1.1/#complementary) attribute.

"Complements" the main content. If there are more than one complementary elements on the page, please be sure to use `labeledBy` to point to the heading of each complementary element.

The HTML5 `aside` tag has this role by default (you still need to use `labeledBy` if there are multiple asides!).

Check out [ARIA Landmarks Complementary Example](https://www.w3.org/TR/wai-aria-practices/examples/landmarks/complementary.html).

    import Accessibility as Html exposing (Html, div, text)
    import Accessibility.Aria exposing (labeledBy)
    import Accessibility.Landmark exposing (complementary)

    view : Html msg
    view =
        div []
            [ div
                [ complementary, labeledBy "extra-thoughts" ]
                [ h2 [ id "extra-thoughts" ] [ text "Extra thoughts..." ]
                , text "some content"
                ]
            , div
                [ complementary, labeledBy "related-docs" ]
                [ h2 [ id "related-docs" ] [ text "Related Documentation" ] ]
            ]

complementary: Html.Styled.Attribute msg
-}
complementary : Elm.Expression
complementary =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Landmark" ]
        , name = "complementary"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Creates a [`role="contentinfo"`](https://www.w3.org/TR/wai-aria-1.1/#contentinfo) attribute.

Copyrights, privacy statements, etc. There ought only be one element with the content info role per page.

You may already have a content info element role fulfilled on your page via the HTML5 `footer` element--as long as its context is the `body`, not a `section` or `main` or what-have-you (see [ARIA Landmarks Contentinfo Example](https://www.w3.org/TR/wai-aria-practices/examples/landmarks/contentinfo.html) for the full list).

    div [ contentInfo ]
        [ h2 []
            [ text "Link to the Privacy Statement You Probably Really Should Read Someday" ]
        ]

contentInfo: Html.Styled.Attribute msg
-}
contentInfo : Elm.Expression
contentInfo =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Landmark" ]
        , name = "contentInfo"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Creates a [`role="form"`](https://www.w3.org/TR/wai-aria-1.1/#form) attribute.

A form container. The HTML5 alternative is to use a `form` element with a `name`.

For examples, please see [ARIA Landmarks Form Example](https://www.w3.org/TR/wai-aria-practices/examples/landmarks/form.html).

form: Html.Styled.Attribute msg
-}
form : Elm.Expression
form =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Landmark" ]
        , name = "form"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Creates a [`role="main"`](https://www.w3.org/TR/wai-aria-1.1/#main) attribute.

Main content in a document. (There should only be one--if you require more than one element with role main, make sure each is labeled. See [ARIA Landmarks Main Example](https://www.w3.org/TR/wai-aria-practices/examples/landmarks/main.html).)

HTML5's `main` tag is implicitly role `main`.

main_: Html.Styled.Attribute msg
-}
main_ : Elm.Expression
main_ =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Landmark" ]
        , name = "main_"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Creates a [`role="navigation"`](https://www.w3.org/TR/wai-aria-1.1/#navigation) attribute.

Navigation--wrap lists of links intended to help your users navigate your site in an element with this role or use HTML5's `nav` tag.

If there's more than one `nav` list on a given page, please make sure that the navigation is labeled (what kinds of links should your users expect to find in this list?). For examples of how to do this using the `labeledBy` property, check out [ARIA Landmarks Navigation Example](https://www.w3.org/TR/wai-aria-practices/examples/landmarks/navigation.html).

navigation: Html.Styled.Attribute msg
-}
navigation : Elm.Expression
navigation =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Landmark" ]
        , name = "navigation"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Prefer the other Landmark options to `region`. Be sure to add a name when using this attribute!

region: Html.Styled.Attribute msg
-}
region : Elm.Expression
region =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Landmark" ]
        , name = "region"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Creates a [`role="search"`](https://www.w3.org/TR/wai-aria-1.1/#search) attribute.

A search input. [ARIA Landmarks Search Example](https://www.w3.org/TR/wai-aria-practices/examples/landmarks/search.html?)

If you're going to learn about an ARIA Landmark role, this is the one to know,
as HTML5 does NOT have a corresponding element! Add this property to forms to signify
that they describe search functionality.

    import Accessibility.Styled exposing (Html, button, form, inputText, labelBefore, text)
    import Accessibility.Styled.Landmark exposing (search)
    import Html.Attributes exposing (type_)
    import Html.Events exposing (onChange)

    type Msg
        = Search String

    view : String -> Html Msg
    view value =
        form [ search ]
            [ labelBefore []
                (text "Search for something good")
                (inputText value [ onChange Search ])
            , button [ type_ "submit" ] [ text "Search" ]
            ]

search: Html.Styled.Attribute msg
-}
search : Elm.Expression
search =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Landmark" ]
        , name = "search"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


values_ :
    { banner : Elm.Expression
    , complementary : Elm.Expression
    , contentInfo : Elm.Expression
    , form : Elm.Expression
    , main_ : Elm.Expression
    , navigation : Elm.Expression
    , region : Elm.Expression
    , search : Elm.Expression
    }
values_ =
    { banner =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Landmark" ]
            , name = "banner"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , complementary =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Landmark" ]
            , name = "complementary"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , contentInfo =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Landmark" ]
            , name = "contentInfo"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , form =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Landmark" ]
            , name = "form"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , main_ =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Landmark" ]
            , name = "main_"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , navigation =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Landmark" ]
            , name = "navigation"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , region =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Landmark" ]
            , name = "region"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , search =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Landmark" ]
            , name = "search"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    }


