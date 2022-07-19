module Example exposing
    ( Example, ConfigurableExample
    , fullName, preview, viewExampleNav, viewExample
    , wrapSettings, wrapMsg, wrapState
    , wrapConfigurableMsg, wrapConfigurableState
    )

{-|

@docs Example, ConfigurableExample
@docs fullName, preview, viewExampleNav, viewExample
@docs wrapSettings, wrapMsg, wrapState
@docs wrapConfigurableMsg, wrapConfigurableState

-}

import Accessibility.Styled.Aria as Aria
import Category exposing (Category)
import Css exposing (..)
import Debug.Control as Control exposing (Control)
import EllieLink
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Html.Styled.Lazy as Lazy
import KeyboardSupport exposing (KeyboardSupport)
import Nri.Ui.ClickableText.V3 as ClickableText
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Container.V2 as Container


type alias Example state msg =
    { name : String
    , version : Int
    , state : state
    , update : msg -> state -> ( state, Cmd msg )
    , subscriptions : state -> Sub msg
    , preview : List (Html Never)
    , view : EllieLink.Config -> state -> List (Html msg)
    , categories : List Category
    , keyboardSupport : List KeyboardSupport
    }


fullName : { example | version : Int, name : String } -> String
fullName example =
    "Nri.Ui." ++ example.name ++ ".V" ++ String.fromInt example.version


type alias ConfigurableExample settings state msg =
    { name : String
    , version : Int
    , state : state
    , update : msg -> state -> ( state, Cmd msg )
    , subscriptions : state -> Sub msg
    , preview : List (Html Never)
    , view : EllieLink.Config -> state -> List (Html msg)
    , categories : List Category
    , keyboardSupport : List KeyboardSupport
    , settings : Control settings
    , -- For example code:
      mainType : String
    , extraImports : List String
    , toExampleCode : settings -> List { sectionName : String, code : String }
    }


wrapSettings :
    (settings -> settings2)
    -> (settings2 -> Maybe settings)
    -> ConfigurableExample settings state msg
    -> ConfigurableExample settings2 state msg
wrapSettings wrapSettings_ unwrapSettings example =
    { name = example.name
    , version = example.version
    , state = example.state
    , update = example.update
    , subscriptions = example.subscriptions
    , preview = example.preview
    , view = example.view
    , categories = example.categories
    , keyboardSupport = example.keyboardSupport
    , settings = Control.map wrapSettings_ example.settings
    , mainType = example.mainType
    , extraImports = example.extraImports
    , toExampleCode =
        \settings ->
            case unwrapSettings settings of
                Just s ->
                    example.toExampleCode s

                Nothing ->
                    []
    }


wrapConfigurableMsg :
    (msg -> msg2)
    -> (msg2 -> Maybe msg)
    -> ConfigurableExample settings state msg
    -> ConfigurableExample settings state msg2
wrapConfigurableMsg wrapMsg_ unwrapMsg example =
    { name = example.name
    , version = example.version
    , state = example.state
    , update =
        \msg2 state ->
            case unwrapMsg msg2 of
                Just msg ->
                    example.update msg state
                        |> Tuple.mapSecond (Cmd.map wrapMsg_)

                Nothing ->
                    ( state, Cmd.none )
    , subscriptions = \state -> Sub.map wrapMsg_ (example.subscriptions state)
    , preview = example.preview
    , view =
        \ellieLinkConfig state ->
            List.map (Html.map wrapMsg_)
                (example.view ellieLinkConfig state)
    , categories = example.categories
    , keyboardSupport = example.keyboardSupport
    , settings = example.settings
    , mainType = example.mainType
    , extraImports = example.extraImports
    , toExampleCode = example.toExampleCode
    }


wrapConfigurableState :
    (state -> state2)
    -> (state2 -> Maybe state)
    -> ConfigurableExample settings state msg
    -> ConfigurableExample settings state2 msg
wrapConfigurableState wrapState_ unwrapState example =
    { name = example.name
    , version = example.version
    , state = wrapState_ example.state
    , update =
        \msg state2 ->
            case unwrapState state2 of
                Just state ->
                    example.update msg state
                        |> Tuple.mapFirst wrapState_

                Nothing ->
                    ( state2, Cmd.none )
    , subscriptions =
        unwrapState
            >> Maybe.map example.subscriptions
            >> Maybe.withDefault Sub.none
    , preview = example.preview
    , view =
        \ellieLinkConfig state ->
            Maybe.map (example.view ellieLinkConfig) (unwrapState state)
                |> Maybe.withDefault []
    , categories = example.categories
    , keyboardSupport = example.keyboardSupport
    , settings = example.settings
    , mainType = example.mainType
    , extraImports = example.extraImports
    , toExampleCode = example.toExampleCode
    }


wrapMsg :
    (msg -> msg2)
    -> (msg2 -> Maybe msg)
    -> Example state msg
    -> Example state msg2
wrapMsg wrapMsg_ unwrapMsg example =
    { name = example.name
    , version = example.version
    , state = example.state
    , update =
        \msg2 state ->
            case unwrapMsg msg2 of
                Just msg ->
                    example.update msg state
                        |> Tuple.mapSecond (Cmd.map wrapMsg_)

                Nothing ->
                    ( state, Cmd.none )
    , subscriptions = \state -> Sub.map wrapMsg_ (example.subscriptions state)
    , preview = example.preview
    , view =
        \ellieLinkConfig state ->
            List.map (Html.map wrapMsg_)
                (example.view ellieLinkConfig state)
    , categories = example.categories
    , keyboardSupport = example.keyboardSupport
    }


wrapState :
    (state -> state2)
    -> (state2 -> Maybe state)
    -> Example state msg
    -> Example state2 msg
wrapState wrapState_ unwrapState example =
    { name = example.name
    , version = example.version
    , state = wrapState_ example.state
    , update =
        \msg state2 ->
            case unwrapState state2 of
                Just state ->
                    example.update msg state
                        |> Tuple.mapFirst wrapState_

                Nothing ->
                    ( state2, Cmd.none )
    , subscriptions =
        unwrapState
            >> Maybe.map example.subscriptions
            >> Maybe.withDefault Sub.none
    , preview = example.preview
    , view =
        \ellieLinkConfig state ->
            Maybe.map (example.view ellieLinkConfig) (unwrapState state)
                |> Maybe.withDefault []
    , categories = example.categories
    , keyboardSupport = example.keyboardSupport
    }


preview :
    { navigate : Example state msg -> msg2
    , exampleHref : Example state msg -> String
    }
    -> Example state msg
    -> Html msg2
preview navConfig =
    Lazy.lazy (preview_ navConfig)


preview_ :
    { navigate : Example state msg -> msg2
    , exampleHref : Example state msg -> String
    }
    -> Example state msg
    -> Html msg2
preview_ { navigate, exampleHref } example =
    Container.view
        [ Container.gray
        , Container.css
            [ Css.flexBasis (Css.px 150)
            , Css.hover
                [ Css.backgroundColor Colors.glacier
                , Css.cursor Css.pointer
                ]
            ]
        , Container.custom [ Events.onClick (navigate example) ]
        , Container.html
            (ClickableText.link example.name
                [ ClickableText.href (exampleHref example)
                , ClickableText.css [ Css.marginBottom (Css.px 10) ]
                , ClickableText.nriDescription "doodad-link"
                ]
                :: [ Html.div
                        [ Attributes.css
                            [ Css.displayFlex
                            , Css.flexDirection Css.column
                            ]
                        , Aria.hidden True
                        ]
                        (List.map (Html.map never) example.preview)
                   ]
            )
        ]


viewExampleNav : Example state msg -> Html a
viewExampleNav example =
    let
        navMenu items =
            Html.nav [ Aria.label (fullName example) ]
                [ Html.ul
                    [ Attributes.css
                        [ margin zero
                        , padding zero
                        , displayFlex
                        , alignItems center
                        , justifyContent flexStart
                        , flexWrap Css.wrap
                        ]
                    ]
                    (List.map
                        (\i ->
                            Html.li
                                [ Attributes.css
                                    [ Css.listStyle Css.none ]
                                ]
                                [ i ]
                        )
                        items
                    )
                ]
    in
    Html.div
        [ Attributes.css
            [ Css.paddingBottom (Css.px 10)
            , Css.marginBottom (Css.px 20)
            , Css.borderBottom3 (Css.px 1) Css.solid Colors.gray92
            ]
        ]
        [ navMenu [ docsLink example, srcLink example ]
        ]


viewExample : EllieLink.Config -> Example state msg -> Html msg
viewExample ellieLinkConfig example =
    Html.div [] (example.view ellieLinkConfig example.state)


docsLink : Example state msg -> Html a
docsLink example =
    let
        link =
            "https://package.elm-lang.org/packages/NoRedInk/noredink-ui/latest/"
                ++ String.replace "." "-" (fullName example)
    in
    ClickableText.link "Docs"
        [ ClickableText.linkExternal link
        ]


srcLink : Example state msg -> Html a
srcLink example =
    let
        link =
            String.replace "." "/" (fullName example)
                ++ ".elm"
                |> (++) "https://github.com/NoRedInk/noredink-ui/blob/master/src/"
    in
    ClickableText.link "Source"
        [ ClickableText.linkExternal link
        , ClickableText.css [ Css.marginLeft (Css.px 20) ]
        ]
