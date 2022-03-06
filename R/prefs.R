rs_prefs_user_path_default <- function() {
  fs::path(
    rappdirs::user_config_dir("rsthemes"),
    "user-prefs.json"
  )
}

rs_prefs_gist_default <- function() {
  getOption("rsprefs.gist_id", NULL)
}

rs_prefs_user_write <- function(
  prefs = rs_prefs_user_read(),
  path = NULL
) {
  if (is_url(path)) {
    cli::cli_abort("Cannot write to a URL, please provide a local {.code path}")
  }

  if (is.null(path)) {
    path <- rs_prefs_user_path_default()
    cli::cli_process_start(
      path,
      msg_done = path,
      msg_failed = "Could not save RStudio user preferences to {.path {path}}"
    )
    fs::dir_create(fs::path_dir(path), recurse = TRUE)
  }

  checkmate::assert_character(path, len = 1, any.missing = FALSE)

  jsonlite::write_json(prefs, path, null = "null", auto_unbox = TRUE, pretty = 2)
  invisible(path)
}

rs_prefs_user_read <- function(path = NULL) {
  path <- path %||% rs_prefs_gist_default() %||% rs_prefs_user_path_default()

  gist <- maybe_gist(path)
  if (is_gist(gist)) {
    return(gist_prefs_read(gist))
  }

  checkmate::assert_character(path, len = 1, any.missing = FALSE)
  if (!is_url(path) && !fs::file_exists(path)) {
    cli::cli_abort("{.path {path}} does not exist")
  }

  jsonlite::read_json(path)
}

rs_prefs_snapshot <- function(
  name,
  path = NULL,
  include = NULL,
  exclude = NULL,
  include_os_settings = FALSE,
  overwrite = FALSE
) {
  requires_rstudioapi(has_fun = "readRStudioPreference")
  checkmate::assert_character(name, len = 1, any.missing = FALSE)

  if (is_url(path)) {
    cli::cli_abort("Cannot snapshot to a URL, please provide a local {.code path}")
  }

  path <- path %||% rs_prefs_user_path_default()

  snaps_all <- if (fs::file_exists(path)) rs_prefs_user_read(path)
  snaps_all <- snaps_all %||% list()

  if (name %in% names(snaps_all) && !isTRUE(overwrite)) {
    cli::cli_abort("Snapshot {.field {name}} exists and {.code overwrite} is not {.code TRUE}")
  }

  prefs_snap <- rs_prefs_rstudio_read(include, exclude, include_os_settings)
  prefs_snap[["$rstudio_version"]] <- as.character(rstudioapi::versionInfo()$version)

  snaps_all[[name]] <- prefs_snap
  rs_prefs_user_write(snaps_all, path = path)
}

rs_prefs_restore <- function(name = NULL, path = NULL, verbose = FALSE) {
  requires_rstudioapi(has_fun = "writeRStudioPreference")
  snaps <- rs_prefs_user_read(path)

  if (is.null(name)) {
    cli::cli_alert_info("Available snapshots: {names(snaps)}")
    return(invisible(names(snaps)))
  }

  checkmate::assert_character(name, len = 1, any.missing = FALSE)

  if (!name %in% names(snaps)) {
    cli::cli_abort(c(
      "Snapshot {.field {name}} isn't the name of an available snapshot.",
      "*" = "Snapshots: {names(snaps)}"
    ))
  }

  snap <- snaps[[name]]
  this_version <- as.character(rstudioapi::versionInfo()$version)
  snap_version <- snap[["$rstudio_version"]] %||% "unknown version"
  if (!identical(snap_version, this_version)) {
    cli::cli_warn(c(
      "Snapshot {.field {name}} is for a different version of RStudio",
      "*" = "Snapshot: {snap_version}",
      "*" = "Current: {this_version}"
    ))
  }

  snap <- snap[!grepl("^[$]", names(snap))]

  rs_prefs_rstudio_write(snap, verbose = verbose)
}

rs_prefs_restore_defaults <- function(verbose = FALSE) {
  requires_rstudioapi(has_fun = "writeRStudioPreference")

  old <- rs_prefs_rstudio_read(source = c("project", "user", "computed"))

  defaults <- purrr::map(
    purrr::set_names(names(old)),
    ~ rs_prefs_schema()[[.x]]$default
  )

  defaults <- purrr::compact(defaults)

  rs_prefs_rstudio_write(defaults, verbose = verbose)

  invisible(old)
}

rstudio_all_prefs <- function() {
  requires_rstudioapi()
  tryCatch({
    rs_allPrefs <- get(".rs.allPrefs", pos = "tools:rstudio")
    x <- rs_allPrefs()
    class(x) <- c("tbl_df", "tbl", "data.frame")
    names(x) <- tolower(names(x))
    x$value <- purrr::map(x$value, jsonlite::fromJSON, simplifyDataFrame = FALSE)
    x
  }, error = function(err) {
    cli::cli_abort("Could not list RStudio preferences", parent = err)
  })
}

rs_prefs_rstudio_read <- function(
  source = "user",
  exclude = NULL,
  include = NULL
) {
  requires_rstudioapi(has_fun = "readRStudioPreference")
  checkmate::assert_character(include, min.len = 1, any.missing = FALSE, null.ok = TRUE)
  checkmate::assert_character(exclude, min.len = 1, any.missing = FALSE, null.ok = TRUE)

  prefs <- rstudio_all_prefs()

  finalize_prefs <- function(prefs) {
    purrr::set_names(prefs$value, prefs$preference)
  }

  if (!is.null(include)) {
    prefs <- finalize_prefs(prefs[prefs$preference %in% include, ])
    return(prefs)
  }

  all_prefs <- prefs[["preference"]]

  source <- match.arg(
    source %||% "all",
    c("all", "default", "project", "user", "computed"),
    several.ok = TRUE
  )

  if ("all" %in% source) {
    source <- NULL
  }

  if (!is.null(source)) {
    source_prefs <- prefs[prefs$source %in% source, ][["preference"]]
    all_prefs <- intersect(all_prefs, source_prefs)
  }

  all_prefs <- setdiff(all_prefs, exclude)

  finalize_prefs(prefs[prefs$preference %in% all_prefs, ])
}

rs_prefs_rstudio_write <- function(prefs, verbose = FALSE) {
  requires_rstudioapi(has_fun = "writeRStudioPreference")

  old <- rs_prefs_rstudio_read(include = names(prefs))

  updated <- 0
  for (name in names(prefs)) {
    if (!name %in% names(rs_prefs_schema())) {
      cli::cli_alert_warning("{.strong {name}} is not a known RStudio preference")
    }
    if (verbose) id <- cli::cli_process_start("{name}")
    tryCatch({
      rs_write_rstudio_preference(name, prefs[[name]])
      updated <- updated + 1
    }, error = function(err) {
      if (verbose) cli::cli_process_failed(id, "{.strong {name}} could not update preference")
      cli::cli_alert_danger("Could not update {.strong {name}}: {err$message}")
    }
    )
    if (verbose) cli::cli_process_done(id, "+ {name}", done_class = "")
  }

  if (length(prefs) == updated) {
    cli::cli_alert_success("Updated {.strong {updated}} preference{?s}")
  } else {
    cli::cli_alert_info("Updated {.strong {updated}} of {length(prefs)} preference{?s}")
  }

  invisible(old)
}

rs_write_rstudio_preference <- function(name, value, type = NULL) {
  cast <- switch(
    type %||% rs_prefs_schema()[[name]][["type"]],
    integer = as.integer,
    real = ,
    number = as.double,
    string = as.character,
    boolean = as.logical,
    identity
  )
  if (identical(name, "editor_theme")) {
    rstudioapi::applyTheme(value)
    return()
  }
  tryCatch(
    rstudioapi::writeRStudioPreference(name, cast(value)),
    error = function(err) {
      is_type_mismatch <- grepl("type mismatch", tolower(err$message))
      if (!is.null(type) || !is_type_mismatch) {
        rlang::abort(err$message)
      }
      expected <- sub("^.+expected <(.+?)>.+$", "\\1", tolower(err$message))
      rs_write_rstudio_preference(name, value, type = expected)
    }
  )
}

rs_prefs_remove_os_settings <- function(pref_names) {
  # We might want to exclude preferences that are generally system-specific
  ignored <- c(
    "initial_working_directory",
    "posix_terminal_shell",
    "custom_shell_command",
    "default_project_location",
    "rmd_preferred_template_path",
    "knit_working_dir",
    "git_exe_path",
    "svn_exe_path",
    "terminal_path",
    "rsa_key_path",
    "terminal_initial_directory",
    "file_monitor_ignored_components",
    "python_path"
  )
  setdiff(pref_names, ignored)
}

as_rs_pref <- function(x) {
  structure(x, class = "rs_pref")
}

#' @export
print.rs_pref <- function(x, ...) {
  parent <- if (!is.null(x$parent)) {
    paste0(paste(x$parent, collapse = "$"), "$")
  } else ""

  cli::cli_text("<{parent}{.strong {x$name}}>")
  cli::cli_text("{.emph {x$description}}")
  cli::cli_div(theme = list(
    span.dt = list(color = "yellow"),
    ".default span.dt" = list(color = "blue"),
    ".current span.dt" = list(color = "cyan"),
    ".type" = list(color = "silver"),
    ".type span.dd" = list("font-style" = "italic")
  ))
  cli::cli_dl()
  if ("type" %in% names(x)) {
    cli::cli_li(c(Type = x$type), class = "type")
  }
  if ("enum" %in% names(x)) {
    cli::cli_li(c(Options = "{jsonlite::toJSON(x$enum, auto_unbox = TRUE)}"))
  }
  if ("properties" %in% names(x)) {
    cli::cli_li(c(Properties = "{.pkg {names(x$properties)}}"))
  }
  default <- jsonlite::toJSON(x$default, auto_unbox = TRUE)
  default <- cli::ansi_strtrim(default, width = cli::console_width() - 14)
  cli::cli_li(
    c(Default = "{default}"),
    class = "default"
  )

  if (in_rstudio(has_fun = "readRStudioPreference")) {
    missing <- structure(list(), class = "missing")
    current <- rstudioapi::readRStudioPreference(x$name, missing)
    if (!identical(current, missing)) {
      current <- jsonlite::toJSON(current, auto_unbox = TRUE)
      current <- cli::ansi_strtrim(current, width = cli::console_width() - 14)
      cli::cli_li(c(Current = "{current}"), class = "current")
    }
  }

  invisible(x)
}
