module Gen.Nri.Ui.TextInput.V7 exposing (addressLevel2, addressLine1, annotation_, autofocus, call_, css, currentPassword, custom, disabled, email, errorIf, errorMessage, familyName, float, generateId, givenName, guidance, hiddenLabel, id, loading, map, moduleName_, newPassword, noMargin, nriDescription, number, onBlur, onEnter, onFocus, organization, organizationTitle, placeholder, postalCode, readOnlyText, search, sex, tel, testId, text, value, values_, view, visibleLabel, writing)

{-| 
@docs moduleName_, view, generateId, number, float, text, newPassword, currentPassword, email, search, addressLevel2, addressLine1, familyName, givenName, organization, organizationTitle, postalCode, sex, tel, readOnlyText, value, map, onFocus, onBlur, onEnter, placeholder, autofocus, hiddenLabel, visibleLabel, css, custom, nriDescription, id, testId, noMargin, disabled, loading, errorIf, errorMessage, guidance, writing, annotation_, call_, values_
-}


import Elm
import Elm.Annotation as Type
import Tuple


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Nri", "Ui", "TextInput", "V7" ]


{-| Render the TextInput as HTML.

view: String -> List (Nri.Ui.TextInput.V7.Attribute value msg) -> Html.Styled.Html msg
-}
view : String -> List Elm.Expression -> Elm.Expression
view viewArg viewArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "TextInput", "V7" ]
                                "Attribute"
                                [ Type.var "value", Type.var "msg" ]
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
        [ Elm.string viewArg, Elm.list viewArg0 ]


{-| Gives you the default DOM element id that will be used by a `TextInput.view` with the given label.
This is for use when you need the DOM element id for use in javascript (such as trigger an event to focus a particular text input)

generateId: String -> String
-}
generateId : String -> Elm.Expression
generateId generateIdArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "generateId"
            , annotation = Just (Type.function [ Type.string ] Type.string)
            }
        )
        [ Elm.string generateIdArg ]


{-| An input that allows integer entry

number: (Maybe Int -> msg) -> Nri.Ui.TextInput.V7.Attribute (Maybe Int) msg
-}
number : (Elm.Expression -> Elm.Expression) -> Elm.Expression
number numberArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "number"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.maybe Type.int ] (Type.var "msg")
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.maybe Type.int, Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.functionReduced "numberUnpack" numberArg ]


{-| An input that allows float entry

float: (Maybe Float -> msg) -> Nri.Ui.TextInput.V7.Attribute (Maybe Float) msg
-}
float : (Elm.Expression -> Elm.Expression) -> Elm.Expression
float floatArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "float"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.maybe Type.float ]
                            (Type.var "msg")
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.maybe Type.float, Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.functionReduced "floatUnpack" floatArg ]


{-| An input that allows text entry

text: (String -> msg) -> Nri.Ui.TextInput.V7.Attribute String msg
-}
text : (Elm.Expression -> Elm.Expression) -> Elm.Expression
text textArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "text"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.string ] (Type.var "msg") ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.string, Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.functionReduced "textUnpack" textArg ]


{-| An input that allows password entry with autocomplete value "new-password"

If the user types at least one character into the input box, a
floating control "Show password" will appear. When clicked, the
input type will change from "password" to "text", in order
to enable the user to check what they've typed.

newPassword: 
    { onInput : String -> msg, showPassword : Bool, setShowPassword : Bool -> msg }
    -> Nri.Ui.TextInput.V7.Attribute String msg
-}
newPassword :
    { onInput : Elm.Expression -> Elm.Expression
    , showPassword : Bool
    , setShowPassword : Elm.Expression -> Elm.Expression
    }
    -> Elm.Expression
newPassword newPasswordArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "newPassword"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "onInput"
                              , Type.function [ Type.string ] (Type.var "msg")
                              )
                            , ( "showPassword", Type.bool )
                            , ( "setShowPassword"
                              , Type.function [ Type.bool ] (Type.var "msg")
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.string, Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.record
            [ Tuple.pair
                "onInput"
                (Elm.functionReduced "newPasswordUnpack" newPasswordArg.onInput)
            , Tuple.pair "showPassword" (Elm.bool newPasswordArg.showPassword)
            , Tuple.pair
                "setShowPassword"
                (Elm.functionReduced
                    "newPasswordUnpack"
                    newPasswordArg.setShowPassword
                )
            ]
        ]


{-| An input that allows password entry with autocomplete value "current-password"

If the user types at least one character into the input box, a
floating control "Show password" will appear. When clicked, the
input type will change from "password" to "text", in order
to enable the user to check what they've typed.

currentPassword: 
    { onInput : String -> msg, showPassword : Bool, setShowPassword : Bool -> msg }
    -> Nri.Ui.TextInput.V7.Attribute String msg
-}
currentPassword :
    { onInput : Elm.Expression -> Elm.Expression
    , showPassword : Bool
    , setShowPassword : Elm.Expression -> Elm.Expression
    }
    -> Elm.Expression
currentPassword currentPasswordArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "currentPassword"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "onInput"
                              , Type.function [ Type.string ] (Type.var "msg")
                              )
                            , ( "showPassword", Type.bool )
                            , ( "setShowPassword"
                              , Type.function [ Type.bool ] (Type.var "msg")
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.string, Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.record
            [ Tuple.pair
                "onInput"
                (Elm.functionReduced
                    "currentPasswordUnpack"
                    currentPasswordArg.onInput
                )
            , Tuple.pair
                "showPassword"
                (Elm.bool currentPasswordArg.showPassword)
            , Tuple.pair
                "setShowPassword"
                (Elm.functionReduced
                    "currentPasswordUnpack"
                    currentPasswordArg.setShowPassword
                )
            ]
        ]


{-| An input that is optimized for email entry

NOTE: this uses `inputmode="email"` so that mobile devices will use the email keyboard,
but not `type="email"` because that would enable browser-provided validation which is inconsistent and at odds
with our validation UI.

email: (String -> msg) -> Nri.Ui.TextInput.V7.Attribute String msg
-}
email : (Elm.Expression -> Elm.Expression) -> Elm.Expression
email emailArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "email"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.string ] (Type.var "msg") ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.string, Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.functionReduced "emailUnpack" emailArg ]


{-| An input with ["search" type](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/search) specified.

search: (String -> msg) -> Nri.Ui.TextInput.V7.Attribute String msg
-}
search : (Elm.Expression -> Elm.Expression) -> Elm.Expression
search searchArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "search"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.string ] (Type.var "msg") ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.string, Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.functionReduced "searchUnpack" searchArg ]


{-| An input that allows address-level2 entry

addressLevel2: (String -> msg) -> Nri.Ui.TextInput.V7.Attribute String msg
-}
addressLevel2 : (Elm.Expression -> Elm.Expression) -> Elm.Expression
addressLevel2 addressLevel2Arg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "addressLevel2"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.string ] (Type.var "msg") ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.string, Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.functionReduced "addressLevel2Unpack" addressLevel2Arg ]


{-| An input that allows address-line1 entry

addressLine1: (String -> msg) -> Nri.Ui.TextInput.V7.Attribute String msg
-}
addressLine1 : (Elm.Expression -> Elm.Expression) -> Elm.Expression
addressLine1 addressLine1Arg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "addressLine1"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.string ] (Type.var "msg") ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.string, Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.functionReduced "addressLine1Unpack" addressLine1Arg ]


{-| An input that allows family-name entry

familyName: (String -> msg) -> Nri.Ui.TextInput.V7.Attribute String msg
-}
familyName : (Elm.Expression -> Elm.Expression) -> Elm.Expression
familyName familyNameArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "familyName"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.string ] (Type.var "msg") ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.string, Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.functionReduced "familyNameUnpack" familyNameArg ]


{-| An input that allows given-name entry

givenName: (String -> msg) -> Nri.Ui.TextInput.V7.Attribute String msg
-}
givenName : (Elm.Expression -> Elm.Expression) -> Elm.Expression
givenName givenNameArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "givenName"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.string ] (Type.var "msg") ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.string, Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.functionReduced "givenNameUnpack" givenNameArg ]


{-| An input that allows organization entry

organization: (String -> msg) -> Nri.Ui.TextInput.V7.Attribute String msg
-}
organization : (Elm.Expression -> Elm.Expression) -> Elm.Expression
organization organizationArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "organization"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.string ] (Type.var "msg") ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.string, Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.functionReduced "organizationUnpack" organizationArg ]


{-| An input that allows organization-title entry

organizationTitle: (String -> msg) -> Nri.Ui.TextInput.V7.Attribute String msg
-}
organizationTitle : (Elm.Expression -> Elm.Expression) -> Elm.Expression
organizationTitle organizationTitleArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "organizationTitle"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.string ] (Type.var "msg") ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.string, Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.functionReduced "organizationTitleUnpack" organizationTitleArg ]


{-| An input that allows postal-code entry

postalCode: (String -> msg) -> Nri.Ui.TextInput.V7.Attribute String msg
-}
postalCode : (Elm.Expression -> Elm.Expression) -> Elm.Expression
postalCode postalCodeArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "postalCode"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.string ] (Type.var "msg") ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.string, Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.functionReduced "postalCodeUnpack" postalCodeArg ]


{-| An input that allows sex entry

sex: (String -> msg) -> Nri.Ui.TextInput.V7.Attribute String msg
-}
sex : (Elm.Expression -> Elm.Expression) -> Elm.Expression
sex sexArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "sex"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.string ] (Type.var "msg") ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.string, Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.functionReduced "sexUnpack" sexArg ]


{-| An input that allows tel entry

tel: (String -> msg) -> Nri.Ui.TextInput.V7.Attribute String msg
-}
tel : (Elm.Expression -> Elm.Expression) -> Elm.Expression
tel telArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "tel"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.string ] (Type.var "msg") ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.string, Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.functionReduced "telUnpack" telArg ]


{-| A read-only input for text values

readOnlyText: Nri.Ui.TextInput.V7.Attribute String msg
-}
readOnlyText : Elm.Expression
readOnlyText =
    Elm.value
        { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
        , name = "readOnlyText"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "TextInput", "V7" ]
                    "Attribute"
                    [ Type.string, Type.var "msg" ]
                )
        }


{-| value: value -> Nri.Ui.TextInput.V7.Attribute value msg -}
value : Elm.Expression -> Elm.Expression
value valueArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "value"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "value" ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.var "value", Type.var "msg" ]
                        )
                    )
            }
        )
        [ valueArg ]


{-| map: 
    (a -> b)
    -> (b -> String)
    -> Nri.Ui.TextInput.V7.Attribute a msg
    -> Nri.Ui.TextInput.V7.Attribute b msg
-}
map :
    (Elm.Expression -> Elm.Expression)
    -> (Elm.Expression -> Elm.Expression)
    -> Elm.Expression
    -> Elm.Expression
map mapArg mapArg0 mapArg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "map"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.var "a" ] (Type.var "b")
                        , Type.function [ Type.var "b" ] Type.string
                        , Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.var "a", Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.var "b", Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.functionReduced "mapUnpack" mapArg
        , Elm.functionReduced "mapUnpack" mapArg0
        , mapArg1
        ]


{-| Causes the TextInput to produce the given `msg` when the field is focused.

onFocus: msg -> Nri.Ui.TextInput.V7.Attribute value msg
-}
onFocus : Elm.Expression -> Elm.Expression
onFocus onFocusArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "onFocus"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.var "value", Type.var "msg" ]
                        )
                    )
            }
        )
        [ onFocusArg ]


{-| Causes the TextInput to produce the given `msg` when the field is blurred.

onBlur: msg -> Nri.Ui.TextInput.V7.Attribute value msg
-}
onBlur : Elm.Expression -> Elm.Expression
onBlur onBlurArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "onBlur"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.var "value", Type.var "msg" ]
                        )
                    )
            }
        )
        [ onBlurArg ]


{-| onEnter: msg -> Nri.Ui.TextInput.V7.Attribute value msg -}
onEnter : Elm.Expression -> Elm.Expression
onEnter onEnterArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "onEnter"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.var "value", Type.var "msg" ]
                        )
                    )
            }
        )
        [ onEnterArg ]


{-| If not explicit placeholder is given, the input label will be used as the placeholder.

placeholder: String -> Nri.Ui.TextInput.V7.Attribute value msg
-}
placeholder : String -> Elm.Expression
placeholder placeholderArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "placeholder"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.var "value", Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string placeholderArg ]


{-| Sets the `autofocus` attribute of the resulting HTML input.

autofocus: Nri.Ui.TextInput.V7.Attribute value msg
-}
autofocus : Elm.Expression
autofocus =
    Elm.value
        { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
        , name = "autofocus"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "TextInput", "V7" ]
                    "Attribute"
                    [ Type.var "value", Type.var "msg" ]
                )
        }


{-| Hides the visible label. (There will still be an invisible label for screen readers.)

hiddenLabel: Nri.Ui.TextInput.V7.Attribute value msg
-}
hiddenLabel : Elm.Expression
hiddenLabel =
    Elm.value
        { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
        , name = "hiddenLabel"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "TextInput", "V7" ]
                    "Attribute"
                    [ Type.var "value", Type.var "msg" ]
                )
        }


{-| Default behavior.

visibleLabel: Nri.Ui.TextInput.V7.Attribute value msg
-}
visibleLabel : Elm.Expression
visibleLabel =
    Elm.value
        { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
        , name = "visibleLabel"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "TextInput", "V7" ]
                    "Attribute"
                    [ Type.var "value", Type.var "msg" ]
                )
        }


{-| Adds CSS to the element containing the input.

If you want to customize colors, borders, font sizes, etc, you should instead add to the TextInput API
to support what you need.

css: List Css.Style -> Nri.Ui.TextInput.V7.Attribute value msg
-}
css : List Elm.Expression -> Elm.Expression
css cssArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "css"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.var "value", Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list cssArg ]


{-| Use this helper to add custom attributes.

Do NOT use this helper to add css styles, as they may not be applied the way
you want/expect if underlying styles change.
Instead, please use the `css` helper.

custom: 
    List (Html.Styled.Attribute Basics.Never)
    -> Nri.Ui.TextInput.V7.Attribute value msg
-}
custom : List Elm.Expression -> Elm.Expression
custom customArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "custom"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Html", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.var "value", Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list customArg ]


{-| nriDescription: String -> Nri.Ui.TextInput.V7.Attribute value msg -}
nriDescription : String -> Elm.Expression
nriDescription nriDescriptionArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "nriDescription"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.var "value", Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string nriDescriptionArg ]


{-| Set a custom ID for this text input and label. If you don't set this,
we'll automatically generate one from the label you pass in, but this can
cause problems if you have more than one text input with the same label on
the page. Use this to be more specific and avoid issues with duplicate IDs!

id: String -> Nri.Ui.TextInput.V7.Attribute value msg
-}
id : String -> Elm.Expression
id idArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "id"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.var "value", Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string idArg ]


{-| testId: String -> Nri.Ui.TextInput.V7.Attribute value msg -}
testId : String -> Elm.Expression
testId testIdArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "testId"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.var "value", Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string testIdArg ]


{-| Remove default spacing from the Input.

noMargin: Bool -> Nri.Ui.TextInput.V7.Attribute value msg
-}
noMargin : Bool -> Elm.Expression
noMargin noMarginArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "noMargin"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.var "value", Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.bool noMarginArg ]


{-| This disables the input

disabled: Nri.Ui.TextInput.V7.Attribute value msg
-}
disabled : Elm.Expression
disabled =
    Elm.value
        { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
        , name = "disabled"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "TextInput", "V7" ]
                    "Attribute"
                    [ Type.var "value", Type.var "msg" ]
                )
        }


{-| Use this while the form the input is a part of is being submitted.

loading: Nri.Ui.TextInput.V7.Attribute value msg
-}
loading : Elm.Expression
loading =
    Elm.value
        { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
        , name = "loading"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "TextInput", "V7" ]
                    "Attribute"
                    [ Type.var "value", Type.var "msg" ]
                )
        }


{-| Sets whether or not the field will be highlighted as having a validation error.

errorIf: Bool -> Nri.Ui.TextInput.V7.Attribute value msg
-}
errorIf : Bool -> Elm.Expression
errorIf errorIfArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "errorIf"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.var "value", Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.bool errorIfArg ]


{-| If `Just`, the field will be highlighted as having a validation error,
and the given error message will be shown.

errorMessage: Maybe String -> Nri.Ui.TextInput.V7.Attribute value msg
-}
errorMessage : Elm.Expression -> Elm.Expression
errorMessage errorMessageArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "errorMessage"
            , annotation =
                Just
                    (Type.function
                        [ Type.maybe Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.var "value", Type.var "msg" ]
                        )
                    )
            }
        )
        [ errorMessageArg ]


{-| A guidance message shows below the input, unless an error message is showing instead.

guidance: String -> Nri.Ui.TextInput.V7.Attribute value msg
-}
guidance : String -> Elm.Expression
guidance guidanceArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "guidance"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.var "value", Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string guidanceArg ]


{-| Uses the "Writing" input style.

writing: Nri.Ui.TextInput.V7.Attribute value msg
-}
writing : Elm.Expression
writing =
    Elm.value
        { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
        , name = "writing"
        , annotation =
            Just
                (Type.namedWith
                    [ "Nri", "Ui", "TextInput", "V7" ]
                    "Attribute"
                    [ Type.var "value", Type.var "msg" ]
                )
        }


annotation_ :
    { attribute : Type.Annotation -> Type.Annotation -> Type.Annotation }
annotation_ =
    { attribute =
        \attributeArg0 attributeArg1 ->
            Type.namedWith
                [ "Nri", "Ui", "TextInput", "V7" ]
                "Attribute"
                [ attributeArg0, attributeArg1 ]
    }


call_ :
    { view : Elm.Expression -> Elm.Expression -> Elm.Expression
    , generateId : Elm.Expression -> Elm.Expression
    , number : Elm.Expression -> Elm.Expression
    , float : Elm.Expression -> Elm.Expression
    , text : Elm.Expression -> Elm.Expression
    , newPassword : Elm.Expression -> Elm.Expression
    , currentPassword : Elm.Expression -> Elm.Expression
    , email : Elm.Expression -> Elm.Expression
    , search : Elm.Expression -> Elm.Expression
    , addressLevel2 : Elm.Expression -> Elm.Expression
    , addressLine1 : Elm.Expression -> Elm.Expression
    , familyName : Elm.Expression -> Elm.Expression
    , givenName : Elm.Expression -> Elm.Expression
    , organization : Elm.Expression -> Elm.Expression
    , organizationTitle : Elm.Expression -> Elm.Expression
    , postalCode : Elm.Expression -> Elm.Expression
    , sex : Elm.Expression -> Elm.Expression
    , tel : Elm.Expression -> Elm.Expression
    , value : Elm.Expression -> Elm.Expression
    , map : Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , onFocus : Elm.Expression -> Elm.Expression
    , onBlur : Elm.Expression -> Elm.Expression
    , onEnter : Elm.Expression -> Elm.Expression
    , placeholder : Elm.Expression -> Elm.Expression
    , css : Elm.Expression -> Elm.Expression
    , custom : Elm.Expression -> Elm.Expression
    , nriDescription : Elm.Expression -> Elm.Expression
    , id : Elm.Expression -> Elm.Expression
    , testId : Elm.Expression -> Elm.Expression
    , noMargin : Elm.Expression -> Elm.Expression
    , errorIf : Elm.Expression -> Elm.Expression
    , errorMessage : Elm.Expression -> Elm.Expression
    , guidance : Elm.Expression -> Elm.Expression
    }
call_ =
    { view =
        \viewArg viewArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
                    , name = "view"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string
                                , Type.list
                                    (Type.namedWith
                                        [ "Nri", "Ui", "TextInput", "V7" ]
                                        "Attribute"
                                        [ Type.var "value", Type.var "msg" ]
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
                [ viewArg, viewArg0 ]
    , generateId =
        \generateIdArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
                    , name = "generateId"
                    , annotation =
                        Just (Type.function [ Type.string ] Type.string)
                    }
                )
                [ generateIdArg ]
    , number =
        \numberArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
                    , name = "number"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.maybe Type.int ]
                                    (Type.var "msg")
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "TextInput", "V7" ]
                                    "Attribute"
                                    [ Type.maybe Type.int, Type.var "msg" ]
                                )
                            )
                    }
                )
                [ numberArg ]
    , float =
        \floatArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
                    , name = "float"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.maybe Type.float ]
                                    (Type.var "msg")
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "TextInput", "V7" ]
                                    "Attribute"
                                    [ Type.maybe Type.float, Type.var "msg" ]
                                )
                            )
                    }
                )
                [ floatArg ]
    , text =
        \textArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
                    , name = "text"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function [ Type.string ] (Type.var "msg")
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "TextInput", "V7" ]
                                    "Attribute"
                                    [ Type.string, Type.var "msg" ]
                                )
                            )
                    }
                )
                [ textArg ]
    , newPassword =
        \newPasswordArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
                    , name = "newPassword"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.record
                                    [ ( "onInput"
                                      , Type.function
                                            [ Type.string ]
                                            (Type.var "msg")
                                      )
                                    , ( "showPassword", Type.bool )
                                    , ( "setShowPassword"
                                      , Type.function
                                            [ Type.bool ]
                                            (Type.var "msg")
                                      )
                                    ]
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "TextInput", "V7" ]
                                    "Attribute"
                                    [ Type.string, Type.var "msg" ]
                                )
                            )
                    }
                )
                [ newPasswordArg ]
    , currentPassword =
        \currentPasswordArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
                    , name = "currentPassword"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.record
                                    [ ( "onInput"
                                      , Type.function
                                            [ Type.string ]
                                            (Type.var "msg")
                                      )
                                    , ( "showPassword", Type.bool )
                                    , ( "setShowPassword"
                                      , Type.function
                                            [ Type.bool ]
                                            (Type.var "msg")
                                      )
                                    ]
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "TextInput", "V7" ]
                                    "Attribute"
                                    [ Type.string, Type.var "msg" ]
                                )
                            )
                    }
                )
                [ currentPasswordArg ]
    , email =
        \emailArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
                    , name = "email"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function [ Type.string ] (Type.var "msg")
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "TextInput", "V7" ]
                                    "Attribute"
                                    [ Type.string, Type.var "msg" ]
                                )
                            )
                    }
                )
                [ emailArg ]
    , search =
        \searchArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
                    , name = "search"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function [ Type.string ] (Type.var "msg")
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "TextInput", "V7" ]
                                    "Attribute"
                                    [ Type.string, Type.var "msg" ]
                                )
                            )
                    }
                )
                [ searchArg ]
    , addressLevel2 =
        \addressLevel2Arg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
                    , name = "addressLevel2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function [ Type.string ] (Type.var "msg")
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "TextInput", "V7" ]
                                    "Attribute"
                                    [ Type.string, Type.var "msg" ]
                                )
                            )
                    }
                )
                [ addressLevel2Arg ]
    , addressLine1 =
        \addressLine1Arg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
                    , name = "addressLine1"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function [ Type.string ] (Type.var "msg")
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "TextInput", "V7" ]
                                    "Attribute"
                                    [ Type.string, Type.var "msg" ]
                                )
                            )
                    }
                )
                [ addressLine1Arg ]
    , familyName =
        \familyNameArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
                    , name = "familyName"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function [ Type.string ] (Type.var "msg")
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "TextInput", "V7" ]
                                    "Attribute"
                                    [ Type.string, Type.var "msg" ]
                                )
                            )
                    }
                )
                [ familyNameArg ]
    , givenName =
        \givenNameArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
                    , name = "givenName"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function [ Type.string ] (Type.var "msg")
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "TextInput", "V7" ]
                                    "Attribute"
                                    [ Type.string, Type.var "msg" ]
                                )
                            )
                    }
                )
                [ givenNameArg ]
    , organization =
        \organizationArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
                    , name = "organization"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function [ Type.string ] (Type.var "msg")
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "TextInput", "V7" ]
                                    "Attribute"
                                    [ Type.string, Type.var "msg" ]
                                )
                            )
                    }
                )
                [ organizationArg ]
    , organizationTitle =
        \organizationTitleArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
                    , name = "organizationTitle"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function [ Type.string ] (Type.var "msg")
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "TextInput", "V7" ]
                                    "Attribute"
                                    [ Type.string, Type.var "msg" ]
                                )
                            )
                    }
                )
                [ organizationTitleArg ]
    , postalCode =
        \postalCodeArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
                    , name = "postalCode"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function [ Type.string ] (Type.var "msg")
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "TextInput", "V7" ]
                                    "Attribute"
                                    [ Type.string, Type.var "msg" ]
                                )
                            )
                    }
                )
                [ postalCodeArg ]
    , sex =
        \sexArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
                    , name = "sex"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function [ Type.string ] (Type.var "msg")
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "TextInput", "V7" ]
                                    "Attribute"
                                    [ Type.string, Type.var "msg" ]
                                )
                            )
                    }
                )
                [ sexArg ]
    , tel =
        \telArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
                    , name = "tel"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function [ Type.string ] (Type.var "msg")
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "TextInput", "V7" ]
                                    "Attribute"
                                    [ Type.string, Type.var "msg" ]
                                )
                            )
                    }
                )
                [ telArg ]
    , value =
        \valueArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
                    , name = "value"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "value" ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "TextInput", "V7" ]
                                    "Attribute"
                                    [ Type.var "value", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ valueArg ]
    , map =
        \mapArg mapArg0 mapArg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
                    , name = "map"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function [ Type.var "a" ] (Type.var "b")
                                , Type.function [ Type.var "b" ] Type.string
                                , Type.namedWith
                                    [ "Nri", "Ui", "TextInput", "V7" ]
                                    "Attribute"
                                    [ Type.var "a", Type.var "msg" ]
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "TextInput", "V7" ]
                                    "Attribute"
                                    [ Type.var "b", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ mapArg, mapArg0, mapArg1 ]
    , onFocus =
        \onFocusArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
                    , name = "onFocus"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "msg" ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "TextInput", "V7" ]
                                    "Attribute"
                                    [ Type.var "value", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ onFocusArg ]
    , onBlur =
        \onBlurArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
                    , name = "onBlur"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "msg" ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "TextInput", "V7" ]
                                    "Attribute"
                                    [ Type.var "value", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ onBlurArg ]
    , onEnter =
        \onEnterArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
                    , name = "onEnter"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "msg" ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "TextInput", "V7" ]
                                    "Attribute"
                                    [ Type.var "value", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ onEnterArg ]
    , placeholder =
        \placeholderArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
                    , name = "placeholder"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "TextInput", "V7" ]
                                    "Attribute"
                                    [ Type.var "value", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ placeholderArg ]
    , css =
        \cssArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
                    , name = "css"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith [ "Css" ] "Style" [])
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "TextInput", "V7" ]
                                    "Attribute"
                                    [ Type.var "value", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ cssArg ]
    , custom =
        \customArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
                    , name = "custom"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Html", "Styled" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "TextInput", "V7" ]
                                    "Attribute"
                                    [ Type.var "value", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ customArg ]
    , nriDescription =
        \nriDescriptionArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
                    , name = "nriDescription"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "TextInput", "V7" ]
                                    "Attribute"
                                    [ Type.var "value", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ nriDescriptionArg ]
    , id =
        \idArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
                    , name = "id"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "TextInput", "V7" ]
                                    "Attribute"
                                    [ Type.var "value", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ idArg ]
    , testId =
        \testIdArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
                    , name = "testId"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "TextInput", "V7" ]
                                    "Attribute"
                                    [ Type.var "value", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ testIdArg ]
    , noMargin =
        \noMarginArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
                    , name = "noMargin"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.bool ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "TextInput", "V7" ]
                                    "Attribute"
                                    [ Type.var "value", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ noMarginArg ]
    , errorIf =
        \errorIfArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
                    , name = "errorIf"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.bool ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "TextInput", "V7" ]
                                    "Attribute"
                                    [ Type.var "value", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ errorIfArg ]
    , errorMessage =
        \errorMessageArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
                    , name = "errorMessage"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.maybe Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "TextInput", "V7" ]
                                    "Attribute"
                                    [ Type.var "value", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ errorMessageArg ]
    , guidance =
        \guidanceArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
                    , name = "guidance"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Nri", "Ui", "TextInput", "V7" ]
                                    "Attribute"
                                    [ Type.var "value", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ guidanceArg ]
    }


values_ :
    { view : Elm.Expression
    , generateId : Elm.Expression
    , number : Elm.Expression
    , float : Elm.Expression
    , text : Elm.Expression
    , newPassword : Elm.Expression
    , currentPassword : Elm.Expression
    , email : Elm.Expression
    , search : Elm.Expression
    , addressLevel2 : Elm.Expression
    , addressLine1 : Elm.Expression
    , familyName : Elm.Expression
    , givenName : Elm.Expression
    , organization : Elm.Expression
    , organizationTitle : Elm.Expression
    , postalCode : Elm.Expression
    , sex : Elm.Expression
    , tel : Elm.Expression
    , readOnlyText : Elm.Expression
    , value : Elm.Expression
    , map : Elm.Expression
    , onFocus : Elm.Expression
    , onBlur : Elm.Expression
    , onEnter : Elm.Expression
    , placeholder : Elm.Expression
    , autofocus : Elm.Expression
    , hiddenLabel : Elm.Expression
    , visibleLabel : Elm.Expression
    , css : Elm.Expression
    , custom : Elm.Expression
    , nriDescription : Elm.Expression
    , id : Elm.Expression
    , testId : Elm.Expression
    , noMargin : Elm.Expression
    , disabled : Elm.Expression
    , loading : Elm.Expression
    , errorIf : Elm.Expression
    , errorMessage : Elm.Expression
    , guidance : Elm.Expression
    , writing : Elm.Expression
    }
values_ =
    { view =
        Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "TextInput", "V7" ]
                                "Attribute"
                                [ Type.var "value", Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Html", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , generateId =
        Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "generateId"
            , annotation = Just (Type.function [ Type.string ] Type.string)
            }
    , number =
        Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "number"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.maybe Type.int ] (Type.var "msg")
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.maybe Type.int, Type.var "msg" ]
                        )
                    )
            }
    , float =
        Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "float"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.maybe Type.float ]
                            (Type.var "msg")
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.maybe Type.float, Type.var "msg" ]
                        )
                    )
            }
    , text =
        Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "text"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.string ] (Type.var "msg") ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.string, Type.var "msg" ]
                        )
                    )
            }
    , newPassword =
        Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "newPassword"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "onInput"
                              , Type.function [ Type.string ] (Type.var "msg")
                              )
                            , ( "showPassword", Type.bool )
                            , ( "setShowPassword"
                              , Type.function [ Type.bool ] (Type.var "msg")
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.string, Type.var "msg" ]
                        )
                    )
            }
    , currentPassword =
        Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "currentPassword"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "onInput"
                              , Type.function [ Type.string ] (Type.var "msg")
                              )
                            , ( "showPassword", Type.bool )
                            , ( "setShowPassword"
                              , Type.function [ Type.bool ] (Type.var "msg")
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.string, Type.var "msg" ]
                        )
                    )
            }
    , email =
        Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "email"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.string ] (Type.var "msg") ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.string, Type.var "msg" ]
                        )
                    )
            }
    , search =
        Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "search"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.string ] (Type.var "msg") ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.string, Type.var "msg" ]
                        )
                    )
            }
    , addressLevel2 =
        Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "addressLevel2"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.string ] (Type.var "msg") ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.string, Type.var "msg" ]
                        )
                    )
            }
    , addressLine1 =
        Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "addressLine1"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.string ] (Type.var "msg") ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.string, Type.var "msg" ]
                        )
                    )
            }
    , familyName =
        Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "familyName"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.string ] (Type.var "msg") ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.string, Type.var "msg" ]
                        )
                    )
            }
    , givenName =
        Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "givenName"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.string ] (Type.var "msg") ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.string, Type.var "msg" ]
                        )
                    )
            }
    , organization =
        Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "organization"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.string ] (Type.var "msg") ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.string, Type.var "msg" ]
                        )
                    )
            }
    , organizationTitle =
        Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "organizationTitle"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.string ] (Type.var "msg") ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.string, Type.var "msg" ]
                        )
                    )
            }
    , postalCode =
        Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "postalCode"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.string ] (Type.var "msg") ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.string, Type.var "msg" ]
                        )
                    )
            }
    , sex =
        Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "sex"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.string ] (Type.var "msg") ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.string, Type.var "msg" ]
                        )
                    )
            }
    , tel =
        Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "tel"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.string ] (Type.var "msg") ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.string, Type.var "msg" ]
                        )
                    )
            }
    , readOnlyText =
        Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "readOnlyText"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "TextInput", "V7" ]
                        "Attribute"
                        [ Type.string, Type.var "msg" ]
                    )
            }
    , value =
        Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "value"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "value" ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.var "value", Type.var "msg" ]
                        )
                    )
            }
    , map =
        Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "map"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.var "a" ] (Type.var "b")
                        , Type.function [ Type.var "b" ] Type.string
                        , Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.var "a", Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.var "b", Type.var "msg" ]
                        )
                    )
            }
    , onFocus =
        Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "onFocus"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.var "value", Type.var "msg" ]
                        )
                    )
            }
    , onBlur =
        Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "onBlur"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.var "value", Type.var "msg" ]
                        )
                    )
            }
    , onEnter =
        Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "onEnter"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.var "value", Type.var "msg" ]
                        )
                    )
            }
    , placeholder =
        Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "placeholder"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.var "value", Type.var "msg" ]
                        )
                    )
            }
    , autofocus =
        Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "autofocus"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "TextInput", "V7" ]
                        "Attribute"
                        [ Type.var "value", Type.var "msg" ]
                    )
            }
    , hiddenLabel =
        Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "hiddenLabel"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "TextInput", "V7" ]
                        "Attribute"
                        [ Type.var "value", Type.var "msg" ]
                    )
            }
    , visibleLabel =
        Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "visibleLabel"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "TextInput", "V7" ]
                        "Attribute"
                        [ Type.var "value", Type.var "msg" ]
                    )
            }
    , css =
        Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "css"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Css" ] "Style" []) ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.var "value", Type.var "msg" ]
                        )
                    )
            }
    , custom =
        Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "custom"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Html", "Styled" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.var "value", Type.var "msg" ]
                        )
                    )
            }
    , nriDescription =
        Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "nriDescription"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.var "value", Type.var "msg" ]
                        )
                    )
            }
    , id =
        Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "id"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.var "value", Type.var "msg" ]
                        )
                    )
            }
    , testId =
        Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "testId"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.var "value", Type.var "msg" ]
                        )
                    )
            }
    , noMargin =
        Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "noMargin"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.var "value", Type.var "msg" ]
                        )
                    )
            }
    , disabled =
        Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "disabled"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "TextInput", "V7" ]
                        "Attribute"
                        [ Type.var "value", Type.var "msg" ]
                    )
            }
    , loading =
        Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "loading"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "TextInput", "V7" ]
                        "Attribute"
                        [ Type.var "value", Type.var "msg" ]
                    )
            }
    , errorIf =
        Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "errorIf"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.var "value", Type.var "msg" ]
                        )
                    )
            }
    , errorMessage =
        Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "errorMessage"
            , annotation =
                Just
                    (Type.function
                        [ Type.maybe Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.var "value", Type.var "msg" ]
                        )
                    )
            }
    , guidance =
        Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "guidance"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Nri", "Ui", "TextInput", "V7" ]
                            "Attribute"
                            [ Type.var "value", Type.var "msg" ]
                        )
                    )
            }
    , writing =
        Elm.value
            { importFrom = [ "Nri", "Ui", "TextInput", "V7" ]
            , name = "writing"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "TextInput", "V7" ]
                        "Attribute"
                        [ Type.var "value", Type.var "msg" ]
                    )
            }
    }


