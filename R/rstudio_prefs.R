#' rstudio_prefs
#'
#' @description
#' The RStudio User Preferences, their description and default value.
#'
#' @source <https://github.com/rstudio/rstudio/raw/main/src/cpp/session/resources/schema/user-prefs-schema.json>
#'
#' @format `r rd_describe_schema(rstudio_prefs)`
"rstudio_prefs"

#' rstudio_prefs_v
#'
#' @description
#' The RStudio User Preferences, their description and default value for
#' released versions of RStudio.
#'
#' @source <https://github.com/rstudio/rstudio/raw/main/src/cpp/session/resources/schema/user-prefs-schema.json>
#' @seealso [rstudio_prefs]
#'
#' @format A list with preferences from `r length(rstudio_prefs_v)` released
#'   versions of RStudio.
"rstudio_prefs_v"
