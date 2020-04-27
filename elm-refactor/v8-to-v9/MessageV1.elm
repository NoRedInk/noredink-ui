module MessageV1 exposing (..)

{-| NOTE: requires elm-refactor alpha-220-g24db2f5 or later.
-}

import Nri.Ui.Message.V1 as Message


upgrade_Nri_Ui_Alert_V4_error content =
    Message.tiny Message.Error (Message.Markdown content)


upgrade_Nri_Ui_Alert_V4_warning content =
    Message.tiny Message.Warning (Message.Markdown content)


upgrade_Nri_Ui_Alert_V4_tip content =
    Message.tiny Message.Tip (Message.Markdown content)


upgrade_Nri_Ui_Alert_V4_success content =
    Message.tiny Message.Success (Message.Markdown content)


upgrade_Nri_Ui_Alert_V4_somethingWentWrong errorMessageForEngineers =
    Message.somethingWentWrong errorMessageForEngineers
