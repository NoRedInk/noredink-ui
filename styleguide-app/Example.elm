module Example exposing (Example, fullName, preview, view, wrapMsg, wrapState)

import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Style as Style
import Category exposing (Category)
import Css exposing (..)
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
import Routes exposing (Route)


type alias Example state msg =
    { name : String
    , version : Int
    , state : state
    , update : msg -> state -> ( state, Cmd msg )
    , subscriptions : state -> Sub msg
    , preview : List (Html Never)
    , view : state -> List (Html msg)
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
    , view = \state -> List.map (Html.map wrapMsg_) (example.view state)
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
        unwrapState
            >> Maybe.map example.view
            >> Maybe.withDefault []
    , categories = example.categories
    , keyboardSupport = example.keyboardSupport
    }


preview : (Route -> msg2) -> Example state msg -> Html msg2
preview navigate =
    Lazy.lazy (preview_ navigate)


preview_ : (Route -> msg2) -> Example state msg -> Html msg2
preview_ navigate example =
    Container.view
        [ Container.gray
        , Container.css
            [ Css.flexBasis (Css.px 150)
            , Css.hover
                [ Css.backgroundColor Colors.glacier
                , Css.cursor Css.pointer
                ]
            ]
        , Container.custom [ Events.onClick (navigate (Routes.Doodad example.name)) ]
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
                        ]
                        (List.map (Html.map never) example.preview)
                   ]
            )
        ]


view : Maybe Route -> Example state msg -> Html msg
view previousRoute example =
    Container.view
        [ Container.pillow
        , Container.css
            [ Css.position Css.relative
            , Css.margin (Css.px 10)
            , Css.minHeight (Css.calc (Css.vh 100) Css.minus (Css.px 20))
            , Css.boxSizing Css.borderBox
            ]
        , Container.html (view_ previousRoute example)
        ]


view_ : Maybe Route -> Example state msg -> List (Html msg)
view_ previousRoute example =
    let
        navMenu items =
            Html.nav [ Aria.label "Example" ]
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
        [ navMenu
            [ Heading.h1
                [ Heading.custom [ Aria.currentPage ] ]
                [ Html.text (fullName example)
                ]
            , docsLink example
            , srcLink example
            , closeExample previousRoute example
            ]
        ]
    , KeyboardSupport.view example.keyboardSupport
    , Html.main_ [] (example.view example.state)
    ]


closeExample : Maybe Route -> Example state msg -> Html msg
closeExample previousRoute example =
    ClickableSvg.link ("Close " ++ example.name ++ " example")
        UiIcon.x
        [ ClickableSvg.href
            (Maybe.withDefault Routes.All previousRoute
                |> Routes.toString
            )
        , ClickableSvg.exactSize 20
        , ClickableSvg.css
            [ Css.position Css.absolute
            , Css.top (Css.px 15)
            , Css.right (Css.px 15)
            ]
        ]


exampleHref : Example state msg -> String
exampleHref example =
    Routes.toString (Routes.Doodad example.name)


docsLink : Example state msg -> Html msg
docsLink example =
    let
        link =
            "https://package.elm-lang.org/packages/NoRedInk/noredink-ui/latest/"
                ++ String.replace "." "-" (fullName example)
    in
    ClickableText.link "Docs"
        [ ClickableText.linkExternal link
        , ClickableText.css [ Css.marginLeft (Css.px 20) ]
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
