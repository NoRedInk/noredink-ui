module Main exposing (main)

import Browser
import Css
import Html.Styled exposing (Html, toUnstyled)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Highlightable.V1 as Highlightable
import Nri.Ui.Highlighter.V1 as Highlighter
import Nri.Ui.HighlighterTool.V1 as Tool


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = toUnstyled << view
        , update = update
        , subscriptions = subscriptions
        }


init flags =
    ( Highlighter.init
        { id = "peerreviews"
        , marker =
            { id = 1
            , name = "Claim"
            , colorSolid = Colors.magenta
            , colorLight = Colors.magenta
            }
                |> (createMarkerV1 >> Tool.Marker)
        , highlightables =
            Highlightable.initFragments Nothing
                "Sir Walter Elliot, of Kellynch Hall, in Somersetshire, was a man who, for his own amusement, never took up any book but the Baronetage; there he found occupation for an idle hour, and consolation in a distressed one; there his faculties were roused into admiration and respect, by contemplating the limited remnant of the earliest patents; there any unwelcome sensations, arising from domestic affairs changed naturally into pity and contempt as he turned over the almost endless creations of the last century; and there, if every other leaf were powerless, he could read his own history with an interest which never failed."
        }
    , Cmd.none
    )


createMarkerV1 tag =
    Tool.buildMarker
        { highlightColor = tag.colorLight
        , hoverColor = tag.colorLight
        , hoverHighlightColor = tag.colorLight
        , kind = tag
        , name = Just tag.name
        }


type alias MarkupTag =
    { id : Int
    , name : String
    , colorSolid : Css.Color
    , colorLight : Css.Color
    }


type alias Model =
    Highlighter.Model MarkupTag


view : Model -> Html Msg
view model =
    Highlighter.view model


type alias Msg =
    Highlighter.Msg MarkupTag


update : Msg -> Model -> ( Model, Cmd Msg )
update rootMsg rootModel =
    let
        ( newHighlighter, cmd, _ ) =
            Highlighter.update rootMsg rootModel
    in
    ( newHighlighter
    , cmd
    )


subscriptions : m -> Sub Msg
subscriptions _ =
    Sub.none
