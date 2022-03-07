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
#'
#' @examples
#' rstudio_prefs_schema("2021.09.0+351")
#'
#' @export
rstudio_prefs_schema <- function(version = NULL) {
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

  if (identical(v_rstudio, v_closest_release[[1]])) {
    return(rsprefs::rstudio_prefs_v[[names(v_closest_release)[[1]]]])
  }

  cache_dir <- rappdirs::user_data_dir("rsprefs")
  path <- fs::path(cache_dir, version, ext = "rds")
  if (fs::file_exists(path)) {
    return(readRDS(path))
  }

  url <- rstudio_prefs_schema_url(version)

  success <- FALSE
  schema <- NULL
  tryCatch({
    schema <- rs_prefs_schema_prepare(url)
    success <- TRUE
  }, error = function(err) {
    cli::cli_inform(
      "Could not download RStudio preference schema for version v{version}, defaulting to {.strong v{v_closest_release}}.",
      parent = err
    )
    schema <<- rsprefs::rstudio_prefs_v[[names(v_closest_release)[[1]]]]
  })

  if (success) {
    fs::dir_create(fs::path_dir(path), recurse = TRUE)
    saveRDS(schema, path)
  }
  schema
}

rstudio_prefs_schema_url <- function(tag) {
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

  if (is.null(x$long_version)) {
    return(x$version)
  }
  package_version(gsub("[+_-]", ".", x$long_version))
}

rstudio_closest_release <- function(version = NULL) {
  version <- rstudio_version(version)
  releases <- purrr::set_names(names(rsprefs::rstudio_prefs_v))
  releases <- purrr::map(releases, rstudio_version)
  releases <- purrr::keep(releases, `<=`, version)
  releases[length(releases)]
}

rs_prefs_schema_prepare <- function(url) {
  rsp_schema <-
    if (grepl("\n", url)) {
      tmpfile <- tempfile(fileext = ".json")
      on.exit(unlink(tmpfile))
      writeLines(url, tmpfile)
      jsonlite::read_json(tmpfile)
    } else {
      cli::cli_process_start("Downloading preferences from: {.url {url}}")
      jsonlite::read_json(url)
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
