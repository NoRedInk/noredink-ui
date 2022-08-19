module Gen.Nri.Ui.SegmentedControl.V14 exposing (annotation_, call_, caseOf_, make_, moduleName_, values_, view, viewRadioGroup)

{-| 
@docs moduleName_, view, viewRadioGroup, annotation_, make_, caseOf_, call_, values_
-}


import Elm
import Elm.Annotation as Type
import Elm.Case
import Tuple


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Nri", "Ui", "SegmentedControl", "V14" ]


{-| - `focusAndSelect` : the message to produce when an option is selected by the user
  - `options`: the list of options available
  - `selected`: the value of the currently-selected option
  - `positioning`: how to position and size the segmented control
  - `toUrl`: a optional function that takes a `route` and returns the URL of that route. You should always use pass a `toUrl` function when the segmented control options correspond to routes in your SPA.

view: 
    { focusAndSelect : { select : a, focus : Maybe String } -> msg
    , options : List (Nri.Ui.SegmentedControl.V14.Option a msg)
    , selected : a
    , positioning : Nri.Ui.SegmentedControl.V14.Positioning
    , toUrl : Maybe (a -> String)
    }
    -> Accessibility.Styled.Html msg
-}
view :
    { focusAndSelect : Elm.Expression -> Elm.Expression
    , options : List Elm.Expression
    , selected : Elm.Expression
    , positioning : Elm.Expression
    , toUrl : Elm.Expression
    }
    -> Elm.Expression
view viewArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "SegmentedControl", "V14" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "focusAndSelect"
                              , Type.function
                                    [ Type.record
                                        [ ( "select", Type.var "a" )
                                        , ( "focus", Type.maybe Type.string )
                                        ]
                                    ]
                                    (Type.var "msg")
                              )
                            , ( "options"
                              , Type.list
                                    (Type.namedWith
                                        [ "Nri"
                                        , "Ui"
                                        , "SegmentedControl"
                                        , "V14"
                                        ]
                                        "Option"
                                        [ Type.var "a", Type.var "msg" ]
                                    )
                              )
                            , ( "selected", Type.var "a" )
                            , ( "positioning"
                              , Type.namedWith
                                    [ "Nri", "Ui", "SegmentedControl", "V14" ]
                                    "Positioning"
                                    []
                              )
                            , ( "toUrl"
                              , Type.maybe
                                    (Type.function [ Type.var "a" ] Type.string)
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.record
            [ Tuple.pair
                "focusAndSelect"
                (Elm.functionReduced "viewUnpack" viewArg.focusAndSelect)
            , Tuple.pair "options" (Elm.list viewArg.options)
            , Tuple.pair "selected" viewArg.selected
            , Tuple.pair "positioning" viewArg.positioning
            , Tuple.pair "toUrl" viewArg.toUrl
            ]
        ]


{-| Creates a set of radio buttons styled to look like a segmented control.

  - `onSelect`: the message to produce when an option is selected (clicked) by the user
  - `idString`: function to get the radio value as a string
  - `options`: the list of options available
  - `selected`: if present, the value of the currently-selected option
  - `positioning`: how to position and size the segmented control
  - `legend`:
      - value read to screenreader users to explain the radio group's purpose <https://dequeuniversity.com/rules/axe/3.3/radiogroup?application=axeAPI>
      - after lowercasing & dashifying, this value is used to group the radio buttons together

viewRadioGroup: 
    { onSelect : a -> msg
    , options : List (Nri.Ui.SegmentedControl.V14.Radio a msg)
    , selected : Maybe a
    , positioning : Nri.Ui.SegmentedControl.V14.Positioning
    , legend : String
    }
    -> Accessibility.Styled.Html msg
-}
viewRadioGroup :
    { onSelect : Elm.Expression -> Elm.Expression
    , options : List Elm.Expression
    , selected : Elm.Expression
    , positioning : Elm.Expression
    , legend : String
    }
    -> Elm.Expression
viewRadioGroup viewRadioGroupArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "SegmentedControl", "V14" ]
            , name = "viewRadioGroup"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "onSelect"
                              , Type.function [ Type.var "a" ] (Type.var "msg")
                              )
                            , ( "options"
                              , Type.list
                                    (Type.namedWith
                                        [ "Nri"
                                        , "Ui"
                                        , "SegmentedControl"
                                        , "V14"
                                        ]
                                        "Radio"
                                        [ Type.var "a", Type.var "msg" ]
                                    )
                              )
                            , ( "selected", Type.maybe (Type.var "a") )
                            , ( "positioning"
                              , Type.namedWith
                                    [ "Nri", "Ui", "SegmentedControl", "V14" ]
                                    "Positioning"
                                    []
                              )
                            , ( "legend", Type.string )
                            ]
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.record
            [ Tuple.pair
                "onSelect"
                (Elm.functionReduced
                    "viewRadioGroupUnpack"
                    viewRadioGroupArg.onSelect
                )
            , Tuple.pair "options" (Elm.list viewRadioGroupArg.options)
            , Tuple.pair "selected" viewRadioGroupArg.selected
            , Tuple.pair "positioning" viewRadioGroupArg.positioning
            , Tuple.pair "legend" (Elm.string viewRadioGroupArg.legend)
            ]
        ]


annotation_ :
    { option : Type.Annotation -> Type.Annotation -> Type.Annotation
    , radio : Type.Annotation -> Type.Annotation -> Type.Annotation
    , positioning : Type.Annotation
    , width : Type.Annotation
    }
annotation_ =
    { option =
        \optionArg0 optionArg1 ->
            Type.alias
                moduleName_
                "Option"
                [ optionArg0, optionArg1 ]
                (Type.record
                    [ ( "value", Type.var "value" )
                    , ( "idString", Type.string )
                    , ( "label"
                      , Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                      )
                    , ( "attributes"
                      , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                      )
                    , ( "tabTooltip"
                      , Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Tooltip", "V3" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                      )
                    , ( "icon"
                      , Type.maybe
                            (Type.namedWith
                                [ "Nri", "Ui", "Svg", "V1" ]
                                "Svg"
                                []
                            )
                      )
                    , ( "content"
                      , Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                      )
                    ]
                )
    , radio =
        \radioArg0 radioArg1 ->
            Type.alias
                moduleName_
                "Radio"
                [ radioArg0, radioArg1 ]
                (Type.record
                    [ ( "value", Type.var "value" )
                    , ( "idString", Type.string )
                    , ( "label"
                      , Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                      )
                    , ( "attributes"
                      , Type.list
                            (Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                      )
                    , ( "tooltip"
                      , Type.list
                            (Type.namedWith
                                [ "Nri", "Ui", "Tooltip", "V3" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                      )
                    , ( "icon"
                      , Type.maybe
                            (Type.namedWith
                                [ "Nri", "Ui", "Svg", "V1" ]
                                "Svg"
                                []
                            )
                      )
                    ]
                )
    , positioning =
        Type.namedWith
            [ "Nri", "Ui", "SegmentedControl", "V14" ]
            "Positioning"
            []
    , width =
        Type.namedWith [ "Nri", "Ui", "SegmentedControl", "V14" ] "Width" []
    }


make_ :
    { option :
        { value : Elm.Expression
        , idString : Elm.Expression
        , label : Elm.Expression
        , attributes : Elm.Expression
        , tabTooltip : Elm.Expression
        , icon : Elm.Expression
        , content : Elm.Expression
        }
        -> Elm.Expression
    , radio :
        { value : Elm.Expression
        , idString : Elm.Expression
        , label : Elm.Expression
        , attributes : Elm.Expression
        , tooltip : Elm.Expression
        , icon : Elm.Expression
        }
        -> Elm.Expression
    , left : Elm.Expression -> Elm.Expression
    , center : Elm.Expression
    , fitContent : Elm.Expression
    , fillContainer : Elm.Expression
    }
make_ =
    { option =
        \option_args ->
            Elm.withType
                (Type.alias
                    [ "Nri", "Ui", "SegmentedControl", "V14" ]
                    "Option"
                    [ Type.var "value", Type.var "msg" ]
                    (Type.record
                        [ ( "value", Type.var "value" )
                        , ( "idString", Type.string )
                        , ( "label"
                          , Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                          )
                        , ( "attributes"
                          , Type.list
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                          )
                        , ( "tabTooltip"
                          , Type.list
                                (Type.namedWith
                                    [ "Nri", "Ui", "Tooltip", "V3" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                          )
                        , ( "icon"
                          , Type.maybe
                                (Type.namedWith
                                    [ "Nri", "Ui", "Svg", "V1" ]
                                    "Svg"
                                    []
                                )
                          )
                        , ( "content"
                          , Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                          )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "value" option_args.value
                    , Tuple.pair "idString" option_args.idString
                    , Tuple.pair "label" option_args.label
                    , Tuple.pair "attributes" option_args.attributes
                    , Tuple.pair "tabTooltip" option_args.tabTooltip
                    , Tuple.pair "icon" option_args.icon
                    , Tuple.pair "content" option_args.content
                    ]
                )
    , radio =
        \radio_args ->
            Elm.withType
                (Type.alias
                    [ "Nri", "Ui", "SegmentedControl", "V14" ]
                    "Radio"
                    [ Type.var "value", Type.var "msg" ]
                    (Type.record
                        [ ( "value", Type.var "value" )
                        , ( "idString", Type.string )
                        , ( "label"
                          , Type.namedWith
                                [ "Accessibility", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                          )
                        , ( "attributes"
                          , Type.list
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                          )
                        , ( "tooltip"
                          , Type.list
                                (Type.namedWith
                                    [ "Nri", "Ui", "Tooltip", "V3" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                          )
                        , ( "icon"
                          , Type.maybe
                                (Type.namedWith
                                    [ "Nri", "Ui", "Svg", "V1" ]
                                    "Svg"
                                    []
                                )
                          )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "value" radio_args.value
                    , Tuple.pair "idString" radio_args.idString
                    , Tuple.pair "label" radio_args.label
                    , Tuple.pair "attributes" radio_args.attributes
                    , Tuple.pair "tooltip" radio_args.tooltip
                    , Tuple.pair "icon" radio_args.icon
                    ]
                )
    , left =
        \ar0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "SegmentedControl", "V14" ]
                    , name = "Left"
                    , annotation = Just (Type.namedWith [] "Positioning" [])
                    }
                )
                [ ar0 ]
    , center =
        Elm.value
            { importFrom = [ "Nri", "Ui", "SegmentedControl", "V14" ]
            , name = "Center"
            , annotation = Just (Type.namedWith [] "Positioning" [])
            }
    , fitContent =
        Elm.value
            { importFrom = [ "Nri", "Ui", "SegmentedControl", "V14" ]
            , name = "FitContent"
            , annotation = Just (Type.namedWith [] "Width" [])
            }
    , fillContainer =
        Elm.value
            { importFrom = [ "Nri", "Ui", "SegmentedControl", "V14" ]
            , name = "FillContainer"
            , annotation = Just (Type.namedWith [] "Width" [])
            }
    }


caseOf_ :
    { positioning :
        Elm.Expression
        -> { positioningTags_0_0
            | left : Elm.Expression -> Elm.Expression
            , center : Elm.Expression
        }
        -> Elm.Expression
    , width :
        Elm.Expression
        -> { widthTags_1_0
            | fitContent : Elm.Expression
            , fillContainer : Elm.Expression
        }
        -> Elm.Expression
    }
caseOf_ =
    { positioning =
        \positioningExpression positioningTags ->
            Elm.Case.custom
                positioningExpression
                (Type.namedWith
                    [ "Nri", "Ui", "SegmentedControl", "V14" ]
                    "Positioning"
                    []
                )
                [ Elm.Case.branch1
                    "Left"
                    ( "nri.Ui.SegmentedControl.V14.Width"
                    , Type.namedWith
                        [ "Nri", "Ui", "SegmentedControl", "V14" ]
                        "Width"
                        []
                    )
                    positioningTags.left
                , Elm.Case.branch0 "Center" positioningTags.center
                ]
    , width =
        \widthExpression widthTags ->
            Elm.Case.custom
                widthExpression
                (Type.namedWith
                    [ "Nri", "Ui", "SegmentedControl", "V14" ]
                    "Width"
                    []
                )
                [ Elm.Case.branch0 "FitContent" widthTags.fitContent
                , Elm.Case.branch0 "FillContainer" widthTags.fillContainer
                ]
    }


call_ :
    { view : Elm.Expression -> Elm.Expression
    , viewRadioGroup : Elm.Expression -> Elm.Expression
    }
call_ =
    { view =
        \viewArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "SegmentedControl", "V14" ]
                    , name = "view"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.record
                                    [ ( "focusAndSelect"
                                      , Type.function
                                            [ Type.record
                                                [ ( "select", Type.var "a" )
                                                , ( "focus"
                                                  , Type.maybe Type.string
                                                  )
                                                ]
                                            ]
                                            (Type.var "msg")
                                      )
                                    , ( "options"
                                      , Type.list
                                            (Type.namedWith
                                                [ "Nri"
                                                , "Ui"
                                                , "SegmentedControl"
                                                , "V14"
                                                ]
                                                "Option"
                                                [ Type.var "a", Type.var "msg" ]
                                            )
                                      )
                                    , ( "selected", Type.var "a" )
                                    , ( "positioning"
                                      , Type.namedWith
                                            [ "Nri"
                                            , "Ui"
                                            , "SegmentedControl"
                                            , "V14"
                                            ]
                                            "Positioning"
                                            []
                                      )
                                    , ( "toUrl"
                                      , Type.maybe
                                            (Type.function
                                                [ Type.var "a" ]
                                                Type.string
                                            )
                                      )
                                    ]
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ viewArg ]
    , viewRadioGroup =
        \viewRadioGroupArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "SegmentedControl", "V14" ]
                    , name = "viewRadioGroup"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.record
                                    [ ( "onSelect"
                                      , Type.function
                                            [ Type.var "a" ]
                                            (Type.var "msg")
                                      )
                                    , ( "options"
                                      , Type.list
                                            (Type.namedWith
                                                [ "Nri"
                                                , "Ui"
                                                , "SegmentedControl"
                                                , "V14"
                                                ]
                                                "Radio"
                                                [ Type.var "a", Type.var "msg" ]
                                            )
                                      )
                                    , ( "selected", Type.maybe (Type.var "a") )
                                    , ( "positioning"
                                      , Type.namedWith
                                            [ "Nri"
                                            , "Ui"
                                            , "SegmentedControl"
                                            , "V14"
                                            ]
                                            "Positioning"
                                            []
                                      )
                                    , ( "legend", Type.string )
                                    ]
                                ]
                                (Type.namedWith
                                    [ "Accessibility", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ viewRadioGroupArg ]
    }


values_ : { view : Elm.Expression, viewRadioGroup : Elm.Expression }
values_ =
    { view =
        Elm.value
            { importFrom = [ "Nri", "Ui", "SegmentedControl", "V14" ]
            , name = "view"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "focusAndSelect"
                              , Type.function
                                    [ Type.record
                                        [ ( "select", Type.var "a" )
                                        , ( "focus", Type.maybe Type.string )
                                        ]
                                    ]
                                    (Type.var "msg")
                              )
                            , ( "options"
                              , Type.list
                                    (Type.namedWith
                                        [ "Nri"
                                        , "Ui"
                                        , "SegmentedControl"
                                        , "V14"
                                        ]
                                        "Option"
                                        [ Type.var "a", Type.var "msg" ]
                                    )
                              )
                            , ( "selected", Type.var "a" )
                            , ( "positioning"
                              , Type.namedWith
                                    [ "Nri", "Ui", "SegmentedControl", "V14" ]
                                    "Positioning"
                                    []
                              )
                            , ( "toUrl"
                              , Type.maybe
                                    (Type.function [ Type.var "a" ] Type.string)
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , viewRadioGroup =
        Elm.value
            { importFrom = [ "Nri", "Ui", "SegmentedControl", "V14" ]
            , name = "viewRadioGroup"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "onSelect"
                              , Type.function [ Type.var "a" ] (Type.var "msg")
                              )
                            , ( "options"
                              , Type.list
                                    (Type.namedWith
                                        [ "Nri"
                                        , "Ui"
                                        , "SegmentedControl"
                                        , "V14"
                                        ]
                                        "Radio"
                                        [ Type.var "a", Type.var "msg" ]
                                    )
                              )
                            , ( "selected", Type.maybe (Type.var "a") )
                            , ( "positioning"
                              , Type.namedWith
                                    [ "Nri", "Ui", "SegmentedControl", "V14" ]
                                    "Positioning"
                                    []
                              )
                            , ( "legend", Type.string )
                            ]
                        ]
                        (Type.namedWith
                            [ "Accessibility", "Styled" ]
                            "Html"
                            [ Type.var "msg" ]
                        )
                    )
            }
    }


