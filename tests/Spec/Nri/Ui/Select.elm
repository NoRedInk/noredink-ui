module Spec.Nri.Ui.Select exposing (spec)

import Html.Styled
import Nri.Ui.Select.V8 as Select
import ProgramTest exposing (..)
import Test exposing (..)


spec : Test
spec =
    describe "Nri.Ui.Select.V9"
        []


program : ProgramTest Model Msg ()
program =
    ProgramTest.createSandbox
        { init = { favoriteAnimal = Nothing }
        , update = update
        , view =
            \model ->
                Select.view "Favorite type of animal"
                    [ Select.choices (toString >> String.toLower)
                        (List.map
                            (\animal ->
                                { label = toString animal
                                , value = animal
                                }
                            )
                            allAnimals
                        )
                    , Select.value model.favoriteAnimal
                    ]
                    |> Html.Styled.map SelectFavorite
                    |> Html.Styled.toUnstyled
        }
        |> ProgramTest.start ()


allAnimals : List Animal
allAnimals =
    [ Cat, Dog, Pig, Other ]


type Animal
    = Cat
    | Dog
    | Pig
    | Other


toString : Animal -> String
toString animal =
    case animal of
        Cat ->
            "Cat"

        Dog ->
            "Dog"

        Pig ->
            "Pig"

        Other ->
            "Other"


type alias Model =
    { favoriteAnimal : Maybe Animal }


type Msg
    = SelectFavorite Animal


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectFavorite animal ->
            { model | favoriteAnimal = Just animal }
