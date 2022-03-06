`%||%` <- function(x, y) if (is.null(x)) y else x

is_null <- function(x) {
  vapply(x, is.null, logical(1))
}

is_url <- function(x) {
  if (is.null(x)) return(FALSE)
  grepl("^https?://.+", x)
}

in_rstudio <- function(has_fun = "getThemes") {
  rstudioapi::hasFun(has_fun)
}

requires_rstudioapi <- function(
  has_fun = "readRStudioPreference",
  version = "1.2.1335"
) {
  if (!requireNamespace("rstudioapi", quietly = TRUE)) {
    cli::cli_abort("The {.pkg rstudioapi} package is required.")
  }

  if (!is.null(version)) {
    rstudio_version <- rstudioapi::versionInfo()$version
    if (rstudio_version < version) {
      cli::cli_abort(
        "RStudio version {.field {version}} is required, but {rstudio_version} is installed."
      )
    }
  }

  if (!rstudioapi::hasFun(has_fun)) {
    cli::cli_abort(
      "{.pkg rstudioapi} function {.fn {has_fun}} is required, but isn't available in the current version. Please update {.pkg rstudioapi}."
    )
  }
}

rd_describe_schema <- function(schema) {
  #' @format A data frame with 255 rows and 10 variables:
  #' \describe{
  #'   \item{\code{NAME}}{[TYPE] DESCRIPTION}
  #'   ...
  #' }

  title <- sprintf("A list with %d items:", length(schema))
  items <- purrr::map_chr(schema, function(pref) {
    sprintf("\n  \\item{\\code{%s}}{%s}", pref[["name"]], pref[["description"]])
  })

  paste0(title, "\n\\describe{\n", paste(items, collapse = ""), "\n}")
}
