#' Look up RStudio Preference Schema
#'
#' Finds and downloads the RStudio user preference schema for your current
#' version of the IDE (or the most recent released version of the IDE). The
#' preference details for all released versions of the IDE are included in the
#' \pkg{rsprefs} package. The preferences from the latest version are available
#' in [rstudio_prefs], but note that those preferences may not match your
#' version of RStudio unless you're using the latest preview version available
#' at <https://dailies.rstudio.com>.
#'
#' @param version The version of the RStudio IDE, e.g. `"1.3.959"`,
#'   `"1.4.1717"`, or `"2021.09.0+351"`.
#' @param quiet Suppress console messages and output
#'
#' @examples
#' prefs_schema("2021.09.0+351")
#'
#' @export
prefs_schema <- function(version = NULL, quiet = FALSE) {
  if (!is.null(version) && version %in% names(rsprefs::rstudio_prefs_v)) {
    return(rsprefs::rstudio_prefs_v[[version]])
  }

  v_rstudio <- rstudio_version()
  version <- version %||% as.character(v_rstudio)
  v_closest_release <- rstudio_closest_release(v_rstudio)

  if (length(v_closest_release) == 0) {
    cli::cli_abort(c(
      "{.pkg rsprefs} only works with RStudio version {.field 1.3.959} or later.",
      "x" = "Current version: {.field {v_rstudio}}"
    ))
  }

  if (identical(version, v_closest_release[[1]])) {
    prefs_closest <- rsprefs::rstudio_prefs_v[[names(v_closest_release)[[1]]]]
    return(augment_rstudio_prefs(prefs_closest))
  }

  cache_dir <- rappdirs::user_data_dir("rsprefs")
  path <- fs::path(cache_dir, version, ext = "rds")
  if (fs::file_exists(path)) {
    return(augment_rstudio_prefs(readRDS(path)))
  }

  url <- prefs_schema_url(version)

  success <- FALSE
  schema <- NULL
  tryCatch({
    schema <- prefs_schema_prepare(url, quiet = TRUE)
    success <- TRUE
  }, error = function(err) {
    v_closest_str <- names(v_closest_release)[[1]]
    if (!quiet) {
      # only report underlying problem if it isn't a download error
      err <- if (!identical(match.fun(err$call[[1]]), open.connection)) err
      cli::cli_inform(
        c(
          "{cli::col_yellow('!')} Using preference schema from RStudio {.strong v{v_closest_str}}",
          "x" = "Schema for current version v{version} is not available online (or is not different)."
        ),
        parent = err
      )
    }
    schema <<- rsprefs::rstudio_prefs_v[[v_closest_str]]
  })


  if (success) {
    # if we downloaded a schema, stash it for later
    fs::dir_create(fs::path_dir(path), recurse = TRUE)
    saveRDS(schema, path)
  }

  if (!is.null(schema)) {
    schema <- augment_rstudio_prefs(schema)
  }

  schema
}

prefs_schema_url <- function(tag) {
  if (tag == "latest") {
    tag <- "main"
  } else if (grepl("^\\d", tag) && !grepl("^v", tag)) {
    tag <- paste0("v", tag)
  }

  sprintf(
    "https://github.com/rstudio/rstudio/raw/%s/src/cpp/session/resources/schema/user-prefs-schema.json",
    utils::URLencode(tag, reserved = TRUE)
  )
}

rstudio_version <- function(x = NULL) {
  x <-
    if (!is.null(x)) {
      list(long_version = as.character(x))
    } else {
      rstudioapi::versionInfo()
    }

  ver <- x$long_version %||% x$version
  ver <- as.character(ver)
  ver <- sub("-daily", "", ver, fixed = TRUE)
  # Change +382 build number to .382
  ver <- gsub("[+_-]", ".", ver)
  # keep only first four numbers (e.g. drop trailing ".pro1")
  ver <- strsplit(ver, "[.]")[[1]]
  ver <- paste(ver[1:min(4, length(ver))], collapse = ".")
  package_version(ver)
}

rstudio_closest_release <- function(version = NULL) {
  version <- rstudio_version(version)
  releases <- purrr::set_names(names(rsprefs::rstudio_prefs_v))
  releases <- purrr::map(releases, rstudio_version)
  releases <- purrr::keep(releases, `<=`, version)
  releases[length(releases)]
}

prefs_schema_prepare <- function(url, quiet = TRUE) {
  rsp_schema <-
    if (grepl("\n", url)) {
      tmpfile <- tempfile(fileext = ".json")
      on.exit(unlink(tmpfile))
      writeLines(url, tmpfile)
      jsonlite::read_json(tmpfile)
    } else if (!quiet) {
      cli::cli_process_start("Downloading preferences from: {.url {url}}")
      jsonlite::read_json(url)
    } else {
      suppressWarnings(
        suppressMessages(
          jsonlite::read_json(url)
        )
      )
    }

  prep_properties <- function(x, name, parent = NULL) {
    x[["name"]] <- name
    if ("properties" %in% names(x)) {
      x[["properties"]] <- purrr::imap(x[["properties"]], prep_properties, parent = x)
    }
    if (!is.null(parent)) {
      x[["parent"]] <- c(x[["parent"]], parent[["name"]])
      x[["default"]] <- parent[["default"]][[name]]
    }
    as_rs_pref(x[union("name", names(x))])
  }

  rsp_schema <- purrr::pluck(rsp_schema, "properties")
  rsp_schema <- purrr::imap(rsp_schema, prep_properties)
  as_rs_pref_list(rsp_schema)
}
