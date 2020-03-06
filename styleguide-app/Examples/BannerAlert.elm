module Examples.BannerAlert exposing (example, State, init, Msg, update)

{-|

@docs example, State, init, Msg, update

-}

import Css
import Html.Styled exposing (div, h3, text)
import Html.Styled.Attributes as Attributes
import ModuleExample as ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.BannerAlert.V6 as BannerAlert
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.UiIcon.V1 as UiIcon


example : (Msg -> msg) -> State -> ModuleExample msg
example parentMsg state =
    { name = "Nri.Ui.BannerAlert.V6"
    , category = Messaging
    , content =
        [ if state.show then
            div
                []
                [ h3 [] [ text "alert" ]
                , BannerAlert.alert
                    [ Html.Styled.text "This is a dismissable alert message!" ]
                    (Just Dismiss)
                ]

          else
            div
                []
                [ h3 [] [ text "success" ]
                , BannerAlert.success
                    [ Html.Styled.text "The alert message was dismissed. 👍" ]
                    Nothing
                ]
        , h3 [] [ text "error" ]
        , BannerAlert.error
            [ Html.Styled.text "This is an error message!" ]
            Nothing
        , h3 [] [ text "with link and icon" ]
        , BannerAlert.error
            [ Html.Styled.div
                []
                [ Html.Styled.text "Click "
                , Html.Styled.a
                    [ Attributes.href "http://www.noredink.com"
                    , Attributes.target "_blank"
                    ]
                    [ Html.Styled.text "here" ]
                , Html.Styled.text " "
                , Html.Styled.div
                    [ Attributes.css
                        [ Css.display Css.inlineBlock
                        , Css.width (Css.px 20)
                        ]
                    ]
                    [ Svg.toHtml UiIcon.gear ]
                , Html.Styled.text " to check out NoRedInk."
                ]
            ]
            Nothing
        , h3 [] [ text "with multi-line link" ]
        , BannerAlert.error
            [ Html.Styled.div
                []
                [ Html.Styled.text "Click "
                , Html.Styled.a
                    [ Attributes.href "http://www.noredink.com"
                    , Attributes.target "_blank"
                    ]
                    [ Html.Styled.text "donec ullamcorper nulla non metus auctor fringilla donec ullamcorper nulla non metus auctor fringilla" ]
                , Html.Styled.text " to check out NoRedInk."
                ]
            ]
            Nothing
        , h3 [] [ text "neutral" ]
        , BannerAlert.neutral
            [ Html.Styled.text "This is a neutral message!" ]
            Nothing
        , h3 [] [ text "success" ]
        , BannerAlert.success
            [ Html.Styled.text """This is a success message!
            Let's see what happens if there is a very long message!
            Wow, how successful! You're the biggest success I've ever seen!
            You should feel great about yourself! Give yourself a very big round of applause!
            """ ]
            Nothing
        ]
            |> List.map (Html.Styled.map parentMsg)
    }


type alias State =
    { show : Bool }


init : State
init =
    { show = True }


type Msg
    = NoOp
    | Dismiss


update : Msg -> State -> State
update msg state =
    case msg of
        NoOp ->
            state

        Dismiss ->
            { state | show = False }
