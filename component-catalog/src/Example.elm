module Example exposing (Example, extraLinks, fromRouteName, fullName, preview, routeName, view, wrapMsg, wrapState)

import Accessibility.Styled.Aria as Aria
import Category exposing (Category)
import Css
import Css.Global
import Css.Media exposing (withMedia)
import EllieLink
import EventExtras
import ExampleSection
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Html.Styled.Lazy as Lazy
import KeyboardSupport exposing (KeyboardSupport)
import Nri.Ui.ClickableText.V4 as ClickableText
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Container.V2 as Container
import Nri.Ui.Header.V1 as Header
import Nri.Ui.MediaQuery.V1 exposing (mobile)


type alias Example state msg =
    { name : String
    , version : Int
    , init : state
    , update : msg -> state -> ( state, Cmd msg )
    , subscriptions : state -> Sub msg
    , preview : List (Html Never)
    , view : EllieLink.Config -> state -> List (Html msg)
    , about : List (Html Never)
    , categories : List Category
    , keyboardSupport : List KeyboardSupport
    }


fullName : { example | version : Int, name : String } -> String
fullName example =
    "Nri.Ui." ++ example.name ++ ".V" ++ String.fromInt example.version


routeName : { example | name : String } -> String
routeName example =
    String.replace " " "-" example.name


fromRouteName : String -> String
fromRouteName name =
    String.replace "-" " " name


wrapMsg :
    (msg -> msg2)
    -> (msg2 -> Maybe msg)
    -> Example state msg
    -> Example state msg2
wrapMsg wrapMsg_ unwrapMsg example =
    { name = example.name
    , version = example.version
    , init = example.init
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
    , about = example.about
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
    , init = wrapState_ example.init
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
    , about = example.about
    , categories = example.categories
    , keyboardSupport = example.keyboardSupport
    }


preview :
    { swallowEvent : msg2
    , navigate : Example state msg -> msg2
    , exampleHref : Example state msg -> String
    }
    -> Example state msg
    -> Html msg2
preview navConfig =
    Lazy.lazy (preview_ navConfig)


preview_ :
    { swallowEvent : msg2
    , navigate : Example state msg -> msg2
    , exampleHref : Example state msg -> String
    }
    -> Example state msg
    -> Html msg2
preview_ { swallowEvent, navigate, exampleHref } example =
    Container.view
        [ Container.gray
        , Container.css
            [ Css.hover
                [ Css.backgroundColor Colors.glacier
                , Css.cursor Css.pointer
                ]
            ]
        , Container.custom [ Events.onClick (navigate example) ]
        , Container.html
            (Html.span [ EventExtras.onClickStopPropagation swallowEvent ]
                [ ClickableText.link example.name
                    [ ClickableText.href (exampleHref example)
                    , ClickableText.css [ Css.marginBottom (Css.px 10) ]
                    , ClickableText.nriDescription "doodad-link"
                    ]
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


view : EllieLink.Config -> Example state msg -> state -> Html msg
view ellieLinkConfig example state =
    Html.div [ Attributes.id (String.replace "." "-" example.name) ]
        (view_ ellieLinkConfig example state)


view_ : EllieLink.Config -> Example state msg -> state -> List (Html msg)
view_ ellieLinkConfig example state =
    [ Html.div
        [ Attributes.css
            [ Css.displayFlex
            , Css.alignItems Css.stretch
            , Css.flexWrap Css.wrap
            , Css.property "gap" "10px"
            , withMedia [ mobile ] [ Css.flexDirection Css.column, Css.alignItems Css.stretch ]
            ]
        ]
        [ ExampleSection.sectionWithCss "About"
            [ Css.flex (Css.int 1) ]
            viewAbout
            example.about
        , KeyboardSupport.view example.keyboardSupport
        ]
    , Html.div [ Attributes.css [ Css.marginBottom (Css.px 200) ] ]
        (example.view ellieLinkConfig state)
    ]


viewAbout : List (Html Never) -> Html msg
viewAbout about =
    Html.div
        [ Attributes.css
            [ Css.margin2 (Css.px 10) Css.zero
            , Css.Global.descendants
                [ Css.Global.code
                    [ Css.fontSize (Css.px 13.5) ]
                ]
            ]
        ]
        about
        |> Html.map never


extraLinks : (msg -> msg2) -> Example state msg -> Header.Attribute route msg2
extraLinks f example =
    Header.extraNav (fullName example)
        [ Html.map f (docsLink example)
        , Html.map f (srcLink example)
        ]


docsLink : Example state msg -> Html msg2
docsLink example =
    let
        link =
            "https://package.elm-lang.org/packages/NoRedInk/noredink-ui/latest/"
                ++ String.replace "." "-" (fullName example)
    in
    ClickableText.link "Docs" [ ClickableText.linkExternal link ]


srcLink : Example state msg -> Html msg2
srcLink example =
    let
        link =
            String.replace "." "/" (fullName example)
                ++ ".elm"
                |> (++) "https://github.com/NoRedInk/noredink-ui/blob/master/src/"
    in
    ClickableText.link "Source" [ ClickableText.linkExternal link ]
