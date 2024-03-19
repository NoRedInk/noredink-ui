module UsageExample exposing (UsageExample, fromRouteName, fullName, noop, preview, routeName, stateless, view, wrapMsg, wrapState)

import Category exposing (Category)
import Css
import Css.Media exposing (withMedia)
import EventExtras
import ExampleSection
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Html.Styled.Lazy as Lazy
import Nri.Ui.ClickableText.V4 as ClickableText
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Container.V2 as Container
import Nri.Ui.MediaQuery.V1 exposing (mobile)
import Nri.Ui.Text.V6 as Text


type alias UsageExample state msg =
    { name : String
    , init : state
    , update : msg -> state -> ( state, Cmd msg )
    , subscriptions : state -> Sub msg
    , view : state -> List (Html msg)
    , about : List (Html Never)
    , categories : List Category
    }


fullName : { example | name : String } -> String
fullName example =
    example.name


routeName : { example | name : String } -> String
routeName example =
    String.replace " " "-" example.name


fromRouteName : String -> String
fromRouteName name =
    String.replace "-" " " name


noop : noop -> UsageExample state msg -> UsageExample state noop
noop msg =
    wrapMsg (always msg) (always Nothing)


wrapMsg :
    (msg -> msg2)
    -> (msg2 -> Maybe msg)
    -> UsageExample state msg
    -> UsageExample state msg2
wrapMsg wrapMsg_ unwrapMsg example =
    { name = example.name
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
    , view =
        \state ->
            List.map (Html.map wrapMsg_)
                (example.view state)
    , about = example.about
    , categories = example.categories
    }


stateless : stateless -> UsageExample () msg -> UsageExample stateless msg
stateless state =
    wrapState (always state) (always (Just ()))


wrapState :
    (state -> state2)
    -> (state2 -> Maybe state)
    -> UsageExample state msg
    -> UsageExample state2 msg
wrapState wrapState_ unwrapState example =
    { name = example.name
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
    , view =
        \state ->
            Maybe.map example.view (unwrapState state)
                |> Maybe.withDefault []
    , about = example.about
    , categories = example.categories
    }


preview :
    { swallowEvent : msg2
    , navigate : UsageExample state msg -> msg2
    , exampleHref : UsageExample state msg -> String
    }
    -> UsageExample state msg
    -> Html msg2
preview navConfig =
    Lazy.lazy (preview_ navConfig)


preview_ :
    { swallowEvent : msg2
    , navigate : UsageExample state msg -> msg2
    , exampleHref : UsageExample state msg -> String
    }
    -> UsageExample state msg
    -> Html msg2
preview_ { swallowEvent, navigate, exampleHref } example =
    Container.view
        [ Container.gray
        , Container.css
            [ Css.flexBasis (Css.px 200)
            , Css.flexShrink Css.zero
            , Css.hover
                [ Css.backgroundColor Colors.glacier
                , Css.cursor Css.pointer
                ]
            ]
        , Container.custom [ Events.onClick (navigate example) ]
        , Container.html
            [ Html.span [ EventExtras.onClickStopPropagation swallowEvent ]
                [ ClickableText.link example.name
                    [ ClickableText.href (exampleHref example)
                    , ClickableText.nriDescription "usage-example-link"
                    ]
                ]
            ]
        ]


view : UsageExample state msg -> state -> Html msg
view example state =
    Html.div [ Attributes.id (String.replace " " "-" example.name) ]
        (view_ example state)


view_ : UsageExample state msg -> state -> List (Html msg)
view_ example state =
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
        ]
    , Html.div [ Attributes.css [ Css.marginBottom (Css.px 200) ] ]
        (example.view state)
    ]


viewAbout : List (Html Never) -> Html msg
viewAbout about =
    Text.mediumBody [ Text.html about ]
        |> Html.map never
