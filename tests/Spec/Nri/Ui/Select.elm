module Spec.Nri.Ui.Select exposing (spec)

import Accessibility.Aria as Aria
import Accessibility.Key as Key
import Accessibility.Role as Role
import Html.Attributes as Attributes
import Html.Styled
import Nri.Ui.Select.V9 as Select
import ProgramTest exposing (..)
import Test exposing (..)
import Test.Html.Selector exposing (..)


spec : Test
spec =
    describe "Nri.Ui.Select.V9"
        [ test "allows selection when no grouping and default value" <|
            \() ->
                ungrouped
                    -- first option is selected automatically
                    |> ensureViewHas
                        [ id "id-nri-select--Cat"
                        , selected True
                        , attribute (Attributes.value "Cat")
                        ]
                    |> selectAnimal Dog
                    |> ensureViewHas
                        [ id "id-nri-select--Dog"
                        , selected True
                        , attribute (Attributes.value "Dog")
                        ]
                    |> selectAnimal Other
                    |> ensureViewHas
                        [ id "id-nri-select--My-favorite-animal-is-something-else"
                        , selected True
                        , attribute (Attributes.value "My favorite animal is something else")
                        ]
                    |> done
        , test "allows selection when grouping" <|
            \() ->
                grouped
                    -- first option is selected automatically
                    |> ensureViewHas
                        [ id "id-nri-select--Cat"
                        , selected True
                        , attribute (Attributes.value "Cat")
                        ]
                    |> selectAnimal Dog
                    |> ensureViewHas
                        [ id "id-nri-select--Dog"
                        , selected True
                        , attribute (Attributes.value "Dog")
                        ]
                    |> selectAnimal Axolotl
                    |> ensureViewHas
                        [ id "id-nri-select--Axolotl"
                        , selected True
                        , attribute (Attributes.value "Axolotl")
                        ]
                    |> selectAnimal Other
                    |> ensureViewHas
                        [ id "id-nri-select--My-favorite-animal-is-something-else"
                        , selected True
                        , attribute (Attributes.value "My favorite animal is something else")
                        ]
                    |> done
        , test "has the correct attributes when disabled" <|
            \() ->
                disabled
                    |> ensureViewHas
                        [ attribute (Aria.disabled True)
                        , attribute Role.listBox
                        , attribute (Key.tabbable True)
                        ]
                    |> done

        {- Attempted to simulate click and keydown events on the enabled select element,
           similar to the approach in tests/Spec/Nri/Ui/Switch.elm
           Got the following error:

           simulateDomEvent "keydown":
           Event.expectEvent: I found a node, but it does not listen for "keydown" events like I expected it would.
        -}
        ]


ungrouped : ProgramTest Model Msg ()
ungrouped =
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
                            (mammals ++ amphibians ++ [ Other ])
                        )
                    , Select.value model.favoriteAnimal
                    , Select.id selectId
                    ]
                    |> Html.Styled.map SelectFavorite
                    |> Html.Styled.toUnstyled
        }
        |> ProgramTest.start ()


grouped : ProgramTest Model Msg ()
grouped =
    let
        toOption =
            \animal ->
                { label = toString animal
                , value = animal
                }
    in
    ProgramTest.createSandbox
        { init = { favoriteAnimal = Nothing }
        , update = update
        , view =
            \model ->
                Select.view "Favorite type of animal even slimy ones"
                    [ Select.choices toString [ toOption Other ]
                    , Select.groupedChoices toString
                        [ { label = "Mammals", choices = List.map toOption mammals }
                        , { label = "Amphibians", choices = List.map toOption amphibians }
                        ]
                    , Select.value model.favoriteAnimal
                    , Select.id selectId
                    ]
                    |> Html.Styled.map SelectFavorite
                    |> Html.Styled.toUnstyled
        }
        |> ProgramTest.start ()


disabled : ProgramTest Model Msg ()
disabled =
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
                            (mammals ++ amphibians ++ [ Other ])
                        )
                    , Select.value model.favoriteAnimal
                    , Select.id selectId
                    , Select.disabled
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


mammals : List Animal
mammals =
    [ Cat, Dog, Pig ]


amphibians : List Animal
amphibians =
    [ Axolotl, Toad, Frog ]


type Animal
    = Cat
    | Dog
    | Pig
    | Axolotl
    | Toad
    | Frog
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

        Axolotl ->
            "Axolotl"

        Toad ->
            "Toad"

        Frog ->
            "Frog"

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
