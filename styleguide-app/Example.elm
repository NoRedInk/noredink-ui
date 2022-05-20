module Example exposing (Example, fullName, preview, view, wrapMsg, wrapState)

import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Widget as Widget
import Category exposing (Category)
import Css exposing (..)
import EllieLink
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Html.Styled.Lazy as Lazy
import KeyboardSupport exposing (KeyboardSupport)
import Nri.Ui.ClickableSvg.V2 as ClickableSvg
import Nri.Ui.ClickableText.V3 as ClickableText
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Container.V2 as Container
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.UiIcon.V1 as UiIcon


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
                        , Widget.hidden True
                        ]
                        (List.map (Html.map never) example.preview)
                   ]
            )
        ]


view : EllieLink.Config -> Example state msg -> Html msg
view ellieLinkConfig example =
    Html.div [ Attributes.id (String.replace "." "-" example.name) ]
        (view_ ellieLinkConfig example)


view_ : EllieLink.Config -> Example state msg -> List (Html msg)
view_ ellieLinkConfig example =
    let
        navMenu items =
            Html.nav [ Widget.label (fullName example) ]
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
    [ Html.header
        [ Attributes.css
            [ Css.paddingBottom (Css.px 10)
            , Css.marginBottom (Css.px 20)
            , Css.borderBottom3 (Css.px 1) Css.solid Colors.gray92
            ]
        ]
        [ navMenu [ docsLink example, srcLink example ]
        ]
    , KeyboardSupport.view example.keyboardSupport
    , Html.main_ [] (example.view ellieLinkConfig example.state)
    ]


docsLink : Example state msg -> Html msg
docsLink example =
    let
        link =
            "https://package.elm-lang.org/packages/NoRedInk/noredink-ui/latest/"
                ++ String.replace "." "-" (fullName example)
    in
    ClickableText.link "Docs"
        [ ClickableText.linkExternal link
        ]


srcLink : Example state msg -> Html msg
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
