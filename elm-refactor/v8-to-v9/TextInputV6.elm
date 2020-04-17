module Main exposing (..)

{-| NOTE: requires elm-refactor alpha-220-g24db2f5 or later.
-}

import Nri.Ui.TextInput.V6 as TextInput



--
-- Nri.Ui.TextInput.V3 to V6
--


upgrade_Nri_Ui_TextInput_V3_view config =
    TextInput.view
        config.label
        (config.type_ config.onInput)
        (List.filterMap identity
            [ case config.isInError of
                False ->
                    Nothing

                _ ->
                    Just (TextInput.errorIf config.isInError)
            , if config.placeholder == config.label then
                Nothing

              else
                Just (TextInput.placeholder config.placeholder)
            , if config.autofocus then
                Just TextInput.autofocus

              else
                Nothing
            , if config.showLabel then
                Nothing

              else
                Just TextInput.hiddenLabel
            ]
        )
        config.value


upgrade_Nri_Ui_TextInput_V3_writing config =
    TextInput.view
        config.label
        (config.type_ config.onInput)
        (List.filterMap identity
            [ Just TextInput.writing
            , Just (TextInput.errorIf config.isInError)
            , if config.placeholder == config.label then
                Nothing

              else
                Just (TextInput.placeholder config.placeholder)
            , if config.autofocus then
                Just TextInput.autofocus

              else
                Nothing
            , if config.showLabel then
                Nothing

              else
                Just TextInput.hiddenLabel
            ]
        )
        config.value


upgrade_Nri_Ui_TextInput_V3_number =
    TextInput.number


upgrade_Nri_Ui_TextInput_V3_text =
    TextInput.text



--
-- Nri.Ui.TextInput.V4 to V6
--


upgrade_Nri_Ui_TextInput_V4_view config =
    TextInput.view
        config.label
        (config.type_ config.onInput)
        (List.filterMap identity
            [ case config.isInError of
                False ->
                    Nothing

                _ ->
                    Just (TextInput.errorIf config.isInError)
            , case config.onBlur of
                Nothing ->
                    Nothing

                Just onBlur ->
                    Just (TextInput.onBlur onBlur)
            , if config.placeholder == config.label then
                Nothing

              else
                Just (TextInput.placeholder config.placeholder)
            , case config.autofocus of
                False ->
                    Nothing

                True ->
                    Just TextInput.autofocus
            , case config.showLabel of
                True ->
                    Nothing

                False ->
                    Just TextInput.hiddenLabel
            ]
        )
        config.value


upgrade_Nri_Ui_TextInput_V4_writing config =
    TextInput.view
        config.label
        (config.type_ config.onInput)
        (List.filterMap identity
            [ Just TextInput.writing
            , case config.isInError of
                False ->
                    Nothing

                _ ->
                    Just (TextInput.errorIf config.isInError)
            , case config.onBlur of
                Nothing ->
                    Nothing

                Just onBlur ->
                    Just (TextInput.onBlur onBlur)
            , if config.placeholder == config.label then
                Nothing

              else
                Just (TextInput.placeholder config.placeholder)
            , case config.autofocus of
                False ->
                    Nothing

                True ->
                    Just TextInput.autofocus
            , case config.showLabel of
                True ->
                    Nothing

                False ->
                    Just TextInput.hiddenLabel
            ]
        )
        config.value


upgrade_Nri_Ui_TextInput_V4_generateId =
    TextInput.generateId


upgrade_Nri_Ui_TextInput_V4_number =
    TextInput.number


upgrade_Nri_Ui_TextInput_V4_text =
    TextInput.text



--
-- Nri.Ui.TextInput.V5 to V6
--


upgrade_Nri_Ui_TextInput_V5_text =
    TextInput.text


upgrade_Nri_Ui_TextInput_V5_number =
    TextInput.number


upgrade_Nri_Ui_TextInput_V5_float =
    TextInput.float


upgrade_Nri_Ui_TextInput_V5_password =
    TextInput.password


upgrade_Nri_Ui_TextInput_V5_email =
    TextInput.email


upgrade_Nri_Ui_TextInput_V5_view model =
    TextInput.view model.label
        (model.type_ model.onInput)
        (List.filterMap identity
            [ case model.isInError of
                False ->
                    Nothing

                _ ->
                    Just (TextInput.errorIf model.isInError)
            , case model.showLabel of
                True ->
                    Nothing

                False ->
                    Just TextInput.hiddenLabel
            , if model.placeholder == model.label then
                Nothing

              else
                Just (TextInput.placeholder model.placeholder)
            , Maybe.map TextInput.onBlur model.onBlur
            , case model.autofocus of
                True ->
                    Just TextInput.autofocus

                False ->
                    Nothing
            ]
        )
        model.value


upgrade_Nri_Ui_TextInput_V5_writing model =
    TextInput.view model.label
        (model.type_ model.onInput)
        (List.filterMap identity
            [ Just TextInput.writing
            , case model.isInError of
                False ->
                    Nothing

                _ ->
                    Just (TextInput.errorIf model.isInError)
            , case model.showLabel of
                True ->
                    Nothing

                False ->
                    Just TextInput.hiddenLabel
            , if model.placeholder == model.label then
                Nothing

              else
                Just (TextInput.placeholder model.placeholder)
            , Maybe.map TextInput.onBlur model.onBlur
            , case model.autofocus of
                True ->
                    Just TextInput.autofocus

                False ->
                    Nothing
            ]
        )
        model.value


upgrade_Nri_Ui_TextInput_V5_generateId labelText =
    TextInput.generateId labelText
