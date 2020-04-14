module Main exposing (upgrade_Nri_Ui_TextInput_V5_view)

import ElmFix
import Nri.Ui.TextInput.V6 as TextInput


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
        [ case model.isInError of
            False ->
                ElmFix.remove

            _ ->
                TextInput.errorIf model.isInError
        , case model.showLabel of
            True ->
                ElmFix.remove

            False ->
                TextInput.hiddenLabel
        , if model.placeholder == model.label then
            ElmFix.remove

          else
            TextInput.placeholder model.placeholder
        , case model.onBlur of
            Nothing ->
                ElmFix.remove

            Just msg ->
                TextInput.onBlur msg
        , case model.autofocus of
            True ->
                TextInput.autofocus

            False ->
                ElmFix.remove
        ]
        model.value


upgrade_Nri_Ui_TextInput_V5_writing model =
    TextInput.view model.label
        (model.type_ model.onInput)
        [ TextInput.writing
        , case model.isInError of
            False ->
                ElmFix.remove

            _ ->
                TextInput.errorIf model.isInError
        , case model.showLabel of
            True ->
                ElmFix.remove

            False ->
                TextInput.hiddenLabel
        , if model.placeholder == model.label then
            ElmFix.remove

          else
            TextInput.placeholder model.placeholder
        , case model.onBlur of
            Nothing ->
                ElmFix.remove

            Just msg ->
                TextInput.onBlur msg
        , case model.autofocus of
            True ->
                TextInput.autofocus

            False ->
                ElmFix.remove
        ]
        model.value


upgrade_Nri_Ui_TextInput_V5_generateId labelText =
    TextInput.generateId labelText
