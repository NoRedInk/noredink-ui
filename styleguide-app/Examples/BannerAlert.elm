module Examples.BannerAlert exposing (example, State, init, Msg, update)

{-|

@docs example, State, init, Msg, update

-}

import Html.Styled exposing (div, h3, text)
import ModuleExample as ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.BannerAlert.V6 as BannerAlert


example : (Msg -> msg) -> State -> ModuleExample msg
example parentMsg state =
    { name = "Nri.Ui.BannerAlert.V6"
    , category = Messaging
    , content =
        [ if state.show then
            div
                []
                [ h3 [] [ text "alert" ]
                , BannerAlert.alert (BannerAlert.Plain "This is a dismissable alert message!") (Just Dismiss)
                ]

          else
            div
                []
                [ h3 [] [ text "success" ]
                , BannerAlert.success (BannerAlert.Plain "The alert message was dismissed. 👍") Nothing
                ]
        , h3 [] [ text "error" ]
        , BannerAlert.error (BannerAlert.Plain "This is an error message!") Nothing
        , h3 [] [ text "error with link" ]
        , BannerAlert.error
            (BannerAlert.WithLink
                { prefixText = "Click "
                , linkText = "here"
                , linkUrl = "http://www.noredink.com"
                , postfixText = " to check out NoRedInk."
                , target = BannerAlert.Blank
                }
            )
            Nothing
        , h3 [] [ text "neutral" ]
        , BannerAlert.neutral (BannerAlert.Plain "This is a neutral message!") Nothing
        , h3 [] [ text "success" ]
        , BannerAlert.success
            (BannerAlert.Plain """This is a success message!
            Let's see what happens if there is a very long message!
            Wow, how successful! You're the biggest success I've ever seen!
            You should feel great about yourself! Give yourself a very big round of applause!
            """)
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
