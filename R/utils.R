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
