module MessageV1 exposing (..)

{-| NOTE: requires elm-refactor alpha-220-g24db2f5 or later.
-}

import Nri.Ui.Message.V1 as Message



--
-- Nri.Ui.Alert.V4
--


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



--
-- Nri.Ui.BannerAlert.V6
--


upgrade_Nri_Ui_BannerAlert_V6_alert content maybeOnDismiss =
    Message.banner Message.Warning
        (Message.Html content)
        (List.filterMap identity
            [ Maybe.map Message.onDismiss maybeOnDismiss
            ]
        )


upgrade_Nri_Ui_BannerAlert_V6_error content maybeOnDismiss =
    Message.banner Message.Error
        (Message.Html content)
        (List.filterMap identity
            [ Maybe.map Message.onDismiss maybeOnDismiss
            ]
        )


upgrade_Nri_Ui_BannerAlert_V6_neutral content maybeOnDismiss =
    Message.banner Message.Tip
        (Message.Html content)
        (List.filterMap identity
            [ Maybe.map Message.onDismiss maybeOnDismiss
            ]
        )


upgrade_Nri_Ui_BannerAlert_V6_success content maybeOnDismiss =
    Message.banner Message.Success
        (Message.Html content)
        (List.filterMap identity
            [ Maybe.map Message.onDismiss maybeOnDismiss
            ]
        )


upgrade_Nri_Ui_BannerAlert_V6_custom config =
    Message.banner
        (Message.Custom
            { color = config.color
            , backgroundColor = config.backgroundColor
            , icon = config.icon
            }
        )
        (Message.Html config.content)
        (List.filterMap identity
            [ Maybe.map Message.onDismiss config.dismiss
            ]
        )
