
#' Snapshot RStudio Preferences
#'
#' @description
#' Save named snapshots of your RStudio preferences that you can apply later.
#'
#' * `snapshot_prefs_save()`: Save your RStudio user preferences with a name that
#'   you can later use to identify these particular settings.
#' * `snapshot_prefs_use()`: Apply a saved snapshot.
#' * `snapshot_prefs_list()`: List available snapshots.
#' * `snapshot_prefs_undo()`: Undo the last applied snapshot.
#'
#' @param name The name of the snapshot to save or apply.
#'
#'   - In `snapshot_prefs_save()`, set to `NULL` to show the preferences that
#'     would be included in the snapshot.
#'   - In `snapshot_prefs_use()`, set to `NULL` to list available snapshots.
#' @param path A GitHub gist ID or local path where the snapshot should be
#'   saved. To create a new public gist, set `path = "new gist"`. For a new
#'   private gist, use `path = "new private gist"`.
#' @param include Names of RStudio preferences to include. If provided, only
#'   these preferences are included. See [prefs_rstudio] for preference names.
#' @param exclude Names of RStudio preferences to exclude from the snapshot. See
#'   [prefs_rstudio] for all of the preference names.
#' @param source The source of the current preference value. Preferences are set
#'   at different levels, from lowest to highest precedence:
#'
#'   - `"default"` are RStudio's built-in defaults
#'   - `"computed"` are detected or supplied from external sources, e.g. the
#'     path to `git` on your system
#'   - `"system"` are derived from system-wide `rstudio-prefs.json`
#'   - `"user"` are set by the user for all sessions (global options)
#'   - `"project"` are set by the current project session
#'
#'   The default is `"user"`, since these are the settings you set yourself for
#'   all projects. You can include any number of sources, or `"all"` to include
#'   all preferences regardless of source.
#' @param overwrite If the snapshot exists, should it be overwritten?
#' @param verbose Prints or suppress informative output
#' @param preview When `TRUE`, previews the preferences that will be or are
#'   included in the snapshot, but does not save or apply them.
#' @param exclude_os_prefs Excludes operating-system or machine-dependent
#'   system preferences from the snapshot or the snapshot restore.
#'
#' @examples
#' if (interactive()) {
#'   tmpfile <- tempfile(fileext = ".json")
#'   snapshot_prefs_save("example", path = tmpfile)
#'   snapshot_prefs_list(tmpfile)
#' }
#'
#' @name snapshot_prefs
NULL

#' @rdname snapshot_prefs
#' @export
snapshot_prefs_save <- function(
  name,
  path = NULL,
  include = NULL,
  exclude = NULL,
  source = "user",
  exclude_os_prefs = TRUE,
  overwrite = FALSE,
  preview = FALSE
) {
  requires_rstudioapi(has_fun = "readRStudioPreference")

  if (isTRUE(preview) || is.null(name)) {
    prefs <- prefs_rstudio_read(source = source, include = include, exclude = exclude)
    return(as_rs_pref_list(prefs, name = if (!missing(name)) name))
  }

  checkmate::assert_character(name, len = 1, any.missing = FALSE, null.ok = TRUE)

  if (is_url(path)) {
    cli::cli_abort("Cannot snapshot to a URL, please provide a local {.code path} or a {.emph gist id}")
  }

  path <- path %||% prefs_gist_default() %||% prefs_user_path_default()
  path <- maybe_gist(path)

  snaps_all <-
    if (is_gist(path) || fs::file_exists(path)) {
      prefs_user_read(path)
    }  else {
      list()
    }

  if (name %in% names(snaps_all) && !isTRUE(overwrite)) {
    cli::cli_abort("Snapshot {.field {name}} exists and {.code overwrite} is not {.code TRUE}")
  }

  prefs_snap <- prefs_rstudio_read(
    source = source,
    include = include,
    exclude = exclude,
    exclude_os_prefs = exclude_os_prefs
  )
  prefs_snap[["$rstudio_version"]] <- as.character(rstudioapi::versionInfo()$version)

  if (is_gist(path)) {
    cli::cli_process_start("Writing snapshot {.field {name}} to gist {.url {path}}")
  } else {
    cli::cli_process_start("Writing snapshot {.field {name}} to {.path {path}}")
  }
  snaps_all[[name]] <- prefs_snap
  prefs_user_write(snaps_all, path = path)
}

#' @rdname snapshot_prefs
#' @export
snapshot_prefs_use <- function(
  name = NULL,
  path = NULL,
  exclude_os_prefs = TRUE,
  verbose = FALSE,
  preview = FALSE
) {
  requires_rstudioapi(has_fun = "writeRStudioPreference")
  snaps <- prefs_user_read(path)

  if (is.null(name)) {
    return(snapshot_prefs_list(path, verbose = missing(verbose) || verbose))
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

  if (isTRUE(preview)) {
    return(as_rs_pref_list(snap, name = name))
  }

  if (isTRUE(exclude_os_prefs)) {
    snap <- prefs_exclude_os_prefs(snap)
  }

  prefs_rstudio_write(snap, verbose = verbose)
}

#' @rdname snapshot_prefs
#' @export
snapshot_prefs_list <- function(path = NULL, verbose = TRUE) {
  path <- path %||% prefs_gist_default() %||% prefs_user_path_default()
  path <- maybe_gist(path)

  snaps <- prefs_user_read(path)

  if (verbose) {
    if (is_gist(path)) {
      cli::cli_text("Snapshot: {.url {path}}")
    } else {
      cli::cli_text("Snapshot: {.path {path}}")
    }
    cli::cli_ul(names(snaps))
    invisible(names(snaps))
  } else {
    names(snaps)
  }
}

#' @rdname snapshot_prefs
#' @export
snapshot_prefs_undo <- function(verbose = TRUE) {
  stack <- ls(.prefs_undo)
  if (!length(stack)) {
    cli::cli_alert_warning("Nothing to undo.")
    return(invisible())
  }
  stack <- sort(stack, decreasing = TRUE)
  last <- .prefs_undo[[stack[[1]]]]
  on.exit(rm(list = stack[[1]], envir = .prefs_undo))
  prefs_rstudio_write(last, with_undo = FALSE)
}

#' Reset Preferences to the RStudio Default
#'
#' Returns currently set preferences to RStudio's original, built-in default.
#'
#' @examples
#' \dontrun{
#' prefs_reset_defaults()
#' }
#'
#' @inheritParams snapshot_prefs_save
#' @inheritDotParams snapshot_prefs_save include exclude exclude_os_prefs
#'
#' @return Resets preferences to their built-in defaults, returning the current
#'   preferences invisibly as a list. You can also return to the preferences
#'   prior to `prefs_reset_defaults()` with [snapshot_prefs_undo()].
#'
#' @export
prefs_reset_defaults <- function(
  source = c("project", "user"),
  verbose = FALSE,
  ...
) {
  requires_rstudioapi(has_fun = "writeRStudioPreference")

  # it doesn't make sense to reset things that are currently defaults
  source <- setdiff(source, "default")

  old <- prefs_rstudio_read(source = source, ...)

  schema <- prefs_schema(quiet = !verbose)

  defaults <- purrr::map(
    purrr::set_names(names(old)),
    ~ schema[[.x]]$default
  )

  defaults <- purrr::compact(defaults)

  prefs_rstudio_write(defaults, verbose = verbose)

  invisible(old)
}


prefs_user_path_default <- function() {
  fs::path(
    rappdirs::user_config_dir("rsprefs"),
    "user-prefs.json"
  )
}

prefs_gist_default <- function() {
  getOption("rsprefs.gist_id", NULL)
}

prefs_user_write <- function(
  prefs = prefs_user_read(),
  path = NULL
) {
  path <- path %||% prefs_gist_default() %||% prefs_user_path_default()

  gist <- maybe_gist(path)
  if (is_gist(gist)) {
    gist_prefs_write(prefs, gist)
    return(invisible(prefs))
  }

  if (identical(path, "new gist") || identical(path, "new public gist")) {
    gist_prefs_create(prefs, public = TRUE)
    return(invisible(prefs))
  }
  if (identical(path, "new private gist")) {
    gist_prefs_create(prefs, public = FALSE)
    return(invisible(prefs))
  }

  if (is_url(path)) {
    cli::cli_abort("Cannot write to a URL, please provide a local {.code path}")
  }

  if (is.null(path)) {
    path <- prefs_user_path_default()
    cli::cli_process_start(
      path,
      msg_done = path,
      msg_failed = "Could not save RStudio user preferences to {.path {path}}"
    )
    fs::dir_create(fs::path_dir(path), recurse = TRUE)
  }

  checkmate::assert_character(path, len = 1, any.missing = FALSE)

  jsonlite::write_json(prefs, path, null = "null", auto_unbox = TRUE, pretty = 2)
  invisible(prefs)
}

prefs_user_read <- function(path = NULL) {
  path <- path %||% prefs_gist_default() %||% prefs_user_path_default()

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

prefs_rstudio_read <- function(
  source = "user",
  exclude = NULL,
  include = NULL,
  exclude_os_prefs = TRUE
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
    c("all", "project", "user", "system", "computed", "default"),
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

  if (isTRUE(exclude_os_prefs)) {
    all_prefs <- prefs_exclude_os_prefs(all_prefs)
  }

  finalize_prefs(prefs[prefs$preference %in% all_prefs, ])
}


.prefs_undo <- new.env(parent = emptyenv())

prefs_rstudio_write <- function(prefs, verbose = FALSE, with_undo = TRUE) {
  requires_rstudioapi(has_fun = "writeRStudioPreference")

  old <- prefs_rstudio_read(include = names(prefs))
  if (with_undo) {
    assign(as.character(as.numeric(Sys.time())), old, envir = .prefs_undo)
  }
  schema <- prefs_schema(quiet = TRUE)

  updated <- 0
  for (name in names(prefs)) {
    if (!name %in% names(schema)) {
      cli::cli_alert_warning("{.strong {name}} is not a preference in the current version of RStudio")
    }
    if (verbose) id <- cli::cli_process_start("{name}")
    tryCatch({
      rs_write_rstudio_preference(name, prefs[[name]], type = schema[[name]]$type)
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

rs_write_rstudio_preference <- function(name, value, type = NULL, try_again = 2L) {
  cast <- switch(
    type %||% prefs_schema(quiet = TRUE)[[name]][["type"]],
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
      if (try_again <= 0L || !is_type_mismatch) {
        rlang::abort(err$message)
      }
      expected <- sub("^.+expected <(.+?)>.+$", "\\1", tolower(err$message))
      rs_write_rstudio_preference(name, value, type = expected, try_again = try_again - 1L)
    }
  )
}

prefs_exclude_os_prefs <- function(pref_names) {
  if (is.list(pref_names)) {
    prefs <- pref_names
    pref_names <- names(prefs)
    return(prefs[prefs_exclude_os_prefs(pref_names)])
  }

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

as_rs_pref_list <- function(x, name = NULL) {
  structure(x, class = "rs_pref_list", snap_name = name)
}

#' @export
print.rs_pref_list <- function(x, ..., n = NULL) {
  snap_name <- attr(x, "snap_name", exact = TRUE)
  if (!is.infinite(n %||% 1)) {
    checkmate::assert_int(n, null.ok = TRUE)
  }
  total <- length(x)
  if (is.null(n)) {
    if (total > 20) {
      x <- x[1:10]
    }
  } else {
    if (n < total) {
      x <- x[seq_len(n)]
    }
  }
  if (!is.null(snap_name)) {
    cli::cli_div(class = "hdr", theme = list(.hdr = list("font-style" = "italic")))
    cli::cli_text('<snapshot {.field "{snap_name}"}>')
    cli::cli_end()
  }
  purrr::iwalk(x, function(pref, name) {
    if (inherits(pref, "rs_pref")) {
      pref_json <- jsonlite::toJSON(pref$default, auto_unbox = TRUE, force = TRUE)
    } else {
      pref_json <- jsonlite::toJSON(pref, auto_unbox = TRUE, force = TRUE)
      if (is.logical(pref)) {
        if (isTRUE(pref)) {
          pref_json <- cli::col_yellow(pref_json)
        } else {
          pref_json <- cli::col_red(pref_json)
        }
      }
      if (is.character(pref)) {
        pref_json <- cli::col_cyan(pref_json)
      }
      if (!is.list(pref)) {
        pref_json <- cli::col_magenta(pref_json)
      }
    }
    pref_json <- cli::ansi_strtrim(
      pref_json,
      width= cli::console_width() - nchar(name) - 4
    )
    name <- cli::col_blue(name)
    cli::cli_text("{name}: {pref_json}")
  })
  if (total > length(x)) {
    cli::cli_text(
      cli::col_silver(sprintf("... and %s more", total - length(x)))
    )
  }
}
