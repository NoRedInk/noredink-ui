module UsageExamples.ClickableCardWithTooltip exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Category exposing (Category(..))
import Html.Styled exposing (Html)
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
    {}


init : State
init =
    {}


type alias Msg =
    ()


update : Msg -> State -> ( State, Cmd Msg )
update msg model =
    case msg of
        () ->
            ( model, Cmd.none )


view : State -> List (Html Msg)
view model =
    []
