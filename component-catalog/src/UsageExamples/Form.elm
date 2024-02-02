module UsageExamples.Form exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Accessibility.Styled exposing (..)
import Category exposing (Category(..))
import Css
import Dict exposing (Dict)
import Html.Styled exposing (Html)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Select.V9 as Select
import Nri.Ui.TextInput.V7 as TextInput
import UsageExample exposing (UsageExample)


example : UsageExample State Msg
example =
    { name = "Form"
    , categories = [ Inputs ]
    , init = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , about = []
    , view = view
    }


type alias State =
    { title : Maybe String
    , firstName : String
    , lastName : String
    , errors : Dict String String
    }


init : State
init =
    { title = Nothing
    , firstName = ""
    , lastName = ""
    , errors = Dict.empty
    }


type Msg
    = SelectTitle String
    | SetFirstName String
    | SetLastName String


update : Msg -> State -> ( State, Cmd Msg )
update msg model =
    case msg of
        SelectTitle title ->
            ( { model | title = Just title }
            , Cmd.none
            )

        SetFirstName name ->
            ( { model | firstName = name }
            , Cmd.none
            )

        SetLastName name ->
            ( { model | lastName = name }
            , Cmd.none
            )


view : State -> List (Html Msg)
view model =
    [ form []
        [ div
            [ css
                [ Css.displayFlex
                , Css.property "gap" "10px"
                ]
            ]
            [ Select.view "Title"
                [ Select.value model.title
                , Select.choices identity
                    (List.map
                        (\title -> { label = title, value = title })
                        [ "Ms.", "Mrs.", "Mr.", "Mx.", "Dr." ]
                    )
                , Select.id "user_title"
                , Select.errorMessage (Dict.get "title" model.errors)
                ]
                |> map SelectTitle
            , TextInput.view "First name"
                [ TextInput.value model.firstName
                , TextInput.givenName SetFirstName
                , TextInput.errorMessage (Dict.get "first_name" model.errors)
                ]
            , TextInput.view "Last name"
                [ TextInput.value model.lastName
                , TextInput.familyName SetLastName
                , TextInput.errorMessage (Dict.get "last_name" model.errors)
                ]
            ]
        ]
    ]
