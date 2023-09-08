module UsageExamples.ClickableCardWithTooltip exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Category exposing (Category(..))
import Css
import Html.Styled exposing (Html)
import Html.Styled.Events as Events
import Nri.Ui.ClickableText.V3 as ClickableText
import Nri.Ui.Container.V2 as Container
import Nri.Ui.Text.V6 as Text
import Nri.Ui.Tooltip.V3 as Tooltip
import UsageExample exposing (UsageExample)


example : UsageExample State Msg
example =
    { name = "Clickable Card with Tooltip"
    , categories = [ Messaging ]
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , about = []
    , view = view
    }


type alias State =
    { openTooltip : Maybe Tooltip
    , parentClicks : Int
    }


init : State
init =
    { openTooltip = Nothing
    , parentClicks = 0
    }


type alias Tooltip =
    ()


type Msg
    = ToggleTooltip Tooltip Bool
    | ParentClick


update : Msg -> State -> ( State, Cmd Msg )
update msg model =
    case msg of
        ToggleTooltip tooltip True ->
            ( { model | openTooltip = Just tooltip }, Cmd.none )

        ToggleTooltip _ False ->
            ( { model | openTooltip = Nothing }, Cmd.none )

        ParentClick ->
            ( { model | parentClicks = model.parentClicks + 1 }
            , Cmd.none
            )


view : State -> List (Html Msg)
view model =
    [ Container.view
        [ Container.buttony
        , Container.html
            [ Text.smallBody
                [ Text.html
                    [ ClickableText.button "Click me" [ ClickableText.appearsInline ]
                    , viewTooltip model
                    ]
                ]
            , Text.smallBody
                [ Text.plaintext "…or click anywhere in the Container!"
                , Text.id "container-element"
                ]
            ]
        , Container.custom [ Events.onClick ParentClick ]
        , Container.css [ Css.maxWidth (Css.px 500) ]
        ]
    , Text.mediumBody [ Text.plaintext ("Parent Clicks: " ++ String.fromInt model.parentClicks) ]
    ]


viewTooltip : State -> Html Msg
viewTooltip model =
    Tooltip.viewToggleTip { label = "Tooltip trigger", lastId = Nothing }
        [ Tooltip.plaintext "Notice that even though this tooltip is in a clickable card, you can still interact with me!"
        , Tooltip.onToggle (ToggleTooltip ())
        , Tooltip.open (model.openTooltip == Just ())
        , Tooltip.onRight
        ]
