module Spec.Nri.Ui.Select exposing (spec)

import Html.Attributes as Attributes
import Html.Styled
import Nri.Ui.Select.V9 as Select
import ProgramTest exposing (..)
import Test exposing (..)
import Test.Html.Selector exposing (..)


spec : Test
spec =
    describe "Nri.Ui.Select.V9"
        [ test "allows selection" <|
            \() ->
                start
                    -- first option is selected automatically
                    |> ensureViewHas
                        [ id "nri-select-cat"
                        , selected True
                        , attribute (Attributes.value "Cat")
                        ]
                    |> selectAnimal Dog
                    |> ensureViewHas
                        [ id "nri-select-dog"
                        , selected True
                        , attribute (Attributes.value "Dog")
                        ]
                    |> selectAnimal Other
                    |> ensureViewHas
                        [ id "nri-select-my-favorite-animal-is-something-else"
                        , selected True
                        , attribute (Attributes.value "My favorite animal is something else")
                        ]
                    |> done
        ]


start : ProgramTest Model Msg ()
start =
    ProgramTest.createSandbox
        { init = { favoriteAnimal = Nothing }
        , update = update
        , view =
            \model ->
                Select.view "Favorite type of animal"
                    [ Select.choices toString
                        (List.map
                            (\animal ->
                                { label = toString animal
                                , value = animal
                                }
                            )
                            allAnimals
                        )
                    , Select.value model.favoriteAnimal
                    , Select.id selectId
                    ]
                    |> Html.Styled.map SelectFavorite
                    |> Html.Styled.toUnstyled
        }
        |> ProgramTest.start ()


selectAnimal : Animal -> ProgramTest a b c -> ProgramTest a b c
selectAnimal animal =
    selectOption selectId "Favorite type of animal" (toString animal) (toString animal)


selectId : String
selectId =
    "favorite-animal-selector"


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
            "My favorite animal is something else"


type alias Model =
    { favoriteAnimal : Maybe Animal }


type Msg
    = SelectFavorite Animal


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectFavorite animal ->
            { model | favoriteAnimal = Just animal }
