module Example exposing (Example, view, wrapMsg, wrapState)

import AtomicDesignType exposing (AtomicDesignType)
import Category exposing (Category)
import Css exposing (..)
import Css.Global exposing (a, descendants)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import KeyboardSupport exposing (KeyboardSupport)
import Nri.Ui.Colors.V1 exposing (..)
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Html.Attributes.V2 as AttributeExtras exposing (targetBlank)


type alias Example state msg =
    { name : String
    , version : Int
    , state : state
    , update : msg -> state -> ( state, Cmd msg )
    , subscriptions : state -> Sub msg
    , view : state -> List (Html msg)
    , categories : List Category
    , atomicDesignType : AtomicDesignType
    , keyboardSupport : List KeyboardSupport
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
    , view = \state -> List.map (Html.map wrapMsg_) (example.view state)
    , categories = example.categories
    , atomicDesignType = example.atomicDesignType
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
    , view =
        unwrapState
            >> Maybe.map example.view
            >> Maybe.withDefault []
    , categories = example.categories
    , atomicDesignType = example.atomicDesignType
    , keyboardSupport = example.keyboardSupport
    }


view : Example state msg -> Html msg
view example =
    let
        fullName =
            "Nri.Ui." ++ example.name ++ ".V" ++ String.fromInt example.version
    in
    Html.div
        [ -- this class makes the axe accessibility checking output easier to parse
          String.replace "." "-" example.name
            |> (++) "module-example__"
            |> Attributes.class
        ]
        [ Html.div
            [ Attributes.css
                [ displayFlex
                , alignItems center
                , justifyContent flexStart
                , flexWrap Css.wrap
                , Fonts.baseFont
                , descendants [ Css.Global.a [ textDecoration none ] ]
                ]
            ]
            [ Html.styled Html.h2
                [ color navy
                , fontSize (px 20)
                , marginTop zero
                , marginBottom zero
                , Fonts.baseFont
                ]
                []
                [ Html.a
                    [ Attributes.href ("#/doodad/" ++ example.name)
                    , Attributes.class "module-example__doodad-link"
                    , -- this data attribute is used to name the Percy screenshots
                      String.replace "." "-" example.name
                        |> Attributes.attribute "data-percy-name"
                    ]
                    [ Html.text fullName ]
                ]
            , String.replace "." "-" fullName
                |> (++) "https://package.elm-lang.org/packages/NoRedInk/noredink-ui/latest/"
                |> viewLink "Docs"
            , String.replace "." "/" fullName
                ++ ".elm"
                |> (++) "https://github.com/NoRedInk/noredink-ui/blob/master/src/"
                |> viewLink "Source"
            ]
        , KeyboardSupport.view example.keyboardSupport
        , Html.div
            [ Attributes.css
                [ padding (px 40)
                , boxShadow5 zero (px 2) (px 4) zero (rgba 0 0 0 0.25)
                , border3 (px 1) solid gray92
                , borderRadius (px 20)
                , margin3 (px 10) zero (px 40)
                ]
            ]
            (example.view example.state)
        ]


viewLink : String -> String -> Html msg
viewLink text href =
    Html.a
        ([ Attributes.href href
         , Attributes.css [ Css.display Css.block, marginLeft (px 20) ]
         ]
            ++ targetBlank
        )
        [ Html.text text
        ]
