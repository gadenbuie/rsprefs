
factory_pref_set <- function(x) {
  force(x)
  function(value) {
    if (!is.null(x$enum)) {
      value <- rlang::arg_match(
        value,
        values = unlist(x$enum),
        error_call = rlang::call2(rlang::sym(sprintf("prefs_rstudio$%s$set", x$name)))
      )
    }
    rs_write_rstudio_preference(name = x$name, value = value, type = x$type)
  }
}

factory_pref_get <- function(x) {
  force(x)
  function() {
    prefs_rstudio_read(include = x$name)[[x$name]]
  }
}

factory_pref_toggle <- function(x) {
  force(x)
  function(state = NULL) {
    if (!is.null(state)) {
      return(
        rs_write_rstudio_preference(name = x$name, value = state, type = x$type)
      )
    }

    current <- prefs_rstudio_read(include = x$name)[[x$name]]
    rs_write_rstudio_preference(name = x$name, value = !current, type = x$type)
    action <- if (current) cli::col_red("\u2A2F Disabled") else cli::col_green("\u2713 Enabled")
    cli::cli_text("{action} {x$name}")
    invisible(!current)
  }
}

augment_prefs_rstudio <- function(prefs) {
  prefs <- lapply(prefs, function(x) {
    x$get <- factory_pref_get(x)
    x$set <- factory_pref_set(x)
    if (identical(tolower(x$type), "boolean")) {
      x$toggle <- factory_pref_toggle(x)
    }
    x
  })

  as_rs_pref_list(prefs)
}

#' prefs_rstudio
#'
#' @description
#' The RStudio User Preferences, their description and default value.
#'
#' @source <https://github.com/rstudio/rstudio/raw/main/src/cpp/session/resources/schema/user-prefs-schema.json>
#'
#' @format `r rd_describe_schema(prefs_rstudio)`
#' @export
prefs_rstudio <- augment_prefs_rstudio(
  readRDS(system.file("prefs_rstudio.rds", package = "rsprefs"))
)

#' prefs_rstudio_v
#'
#' @description
#' The RStudio User Preferences, their description and default value for
#' released versions of RStudio.
#'
#' @source <https://github.com/rstudio/rstudio/raw/main/src/cpp/session/resources/schema/user-prefs-schema.json>
#' @seealso [prefs_rstudio]
#'
#' @format A list with preferences from `r length(prefs_rstudio_v)` released
#'   versions of RStudio.
"prefs_rstudio_v"
