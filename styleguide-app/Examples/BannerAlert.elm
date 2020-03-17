module Examples.BannerAlert exposing (example, State, init, Msg, update)

{-|

@docs example, State, init, Msg, update

-}

import Css
import Html.Styled exposing (a, div, h3, pre, text)
import Html.Styled.Attributes as Attributes
import ModuleExample as ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.BannerAlert.V6 as BannerAlert
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Pennant.V1 as Pennant
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
                , BannerAlert.alert [ text "Dismiss this alert message to see a success message!" ] (Just Dismiss)
                , pre [] [ text "BannerAlert.alert [ text \"Dismiss this alert message to see a success message!\" ] (Just Dismiss)" ]
                ]

          else
            div
                []
                [ h3 [] [ text "success" ]
                , BannerAlert.success [ text "Nice! The alert message was dismissed. 👍" ] Nothing
                , pre [] [ text "BannerAlert.success [ text \"Nice! The alert message was dismissed. 👍\" ] Nothing" ]
                ]
        , h3 [] [ text "error" ]
        , BannerAlert.error [ text "This is an error message!" ] Nothing
        , pre [] [ text "BannerAlert.error [ text \"This is an error message!\" ] Nothing" ]
        , h3 [] [ text "neutral" ]
        , BannerAlert.neutral [ text "This is a neutral message!" ] Nothing
        , pre [] [ text "BannerAlert.neutral [ text \"This is a neutral message!\" ] Nothing" ]
        , h3 [] [ text "custom" ]
        , BannerAlert.custom
            { color = Colors.aquaDark
            , backgroundColor = Colors.gray92
            , icon = Pennant.premiumFlag
            , content = [ text "I'm a custom banner!" ]
            , dismiss = Nothing
            }
        , pre [] [ text "TODO" ]
        , h3 [] [ text "with multi-line link and icon" ]
        , BannerAlert.success
            [ text "Click "
            , a [ Attributes.href "http://www.noredink.com", Attributes.target "_blank" ]
                [ text
                    """here, yes, HERE, right here on this very long success message.
                    Wow, how successful! You're the biggest success I've ever seen!
                    You should feel great about yourself! Give yourself a very big round of applause!
                    """
                , div [ Attributes.css [ Css.display Css.inlineBlock, Css.width (Css.px 20) ] ]
                    [ Svg.toHtml UiIcon.gear ]
                ]
            , text " to check out NoRedInk."
            ]
            Nothing
        , pre []
            [ text
                """BannerAlert.success
    [ text "Click "
    , a [ Attributes.href "http://www.noredink.com", Attributes.target "_blank" ]
        [ text
            \"\"\"here, yes, HERE, right here on this very long success message.
            Wow, how successful! You're the biggest success I've ever seen!
            You should feel great about yourself! Give yourself a very big round of applause!
            \"\"\"
        , div [ Attributes.css [ Css.display Css.inlineBlock, Css.width (Css.px 20) ] ]
            [ Svg.toHtml UiIcon.gear ]
        ]
    , text " to check out NoRedInk."
    ]
    Nothing
    """
            ]
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
