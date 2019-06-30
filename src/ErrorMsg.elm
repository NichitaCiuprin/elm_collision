port module ErrorMsg exposing (ErrorMsg, createErrorMsg, view)

import Html


type alias ErrorMsg =
    String


view : ErrorMsg -> Html.Html a
view errorMsg =
    Html.text errorMsg


createErrorMsg : String -> String -> ErrorMsg
createErrorMsg location info =
    "Error in \"" ++ location ++ "\". " ++ info
