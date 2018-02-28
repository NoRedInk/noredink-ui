module Nri.Ui.DatePickerConstants exposing (..)

{-|

@docs datePickerTag
@docs dialogTag
@docs footerTag

-}


{-| The class of the entire date picker
-}
datePickerTag : String
datePickerTag =
    "date-time-picker"


{-| The class of just the dialog that shows up when you open the datepicker
-}
dialogTag : String
dialogTag =
    "date-time-picker-dialog"


{-| The class of the footer in the dialog.
This is where the pretty-printed date is displayed.
-}
footerTag : String
footerTag =
    "date-time-picker-footer"
