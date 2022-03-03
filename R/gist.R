gist <- function(id, file = "user-prefs.json") {
  structure(
    list(id = id, file = file),
    class = "rsthemes_user_prefs_gist"
  )
}

is_gist <- function(x) {
  inherits(x, "rsthemes_user_prefs_gist")
}

maybe_gist <- function(x) {
  if (is_gist(x) || !is.character(x)) {
    return(x)
  }
  if (fs::file_exists(x)) {
    return(x)
  }
  if (grepl("[^0-9a-f]", x) || nchar(x) < 8) {
    return(x)
  }
  gist(x)
}

#' @export
print.rsthemes_user_prefs_gist <- function(x, ...) {
  url <- gist_url_html(x)
  cli::cli_inform("{.url {url}}")
}

gist_url_html <- function(gist) {
  stopifnot(inherits(gist, "rsthemes_user_prefs_gist"))
  sprintf("https://gist.github.com/%s", gist$id)
}

gist_get <- function(gist) {
  stopifnot(inherits(gist, "rsthemes_user_prefs_gist"))
  tryCatch(
    gh::gh("/gists/{gist_id}", gist_id = gist$id),
    http_error_404 = function(err) {
      cli::cli_abort(
        "Gist {.field {gist$id}} does not exist.",
        parent = err,
        class = "http_error_404"
      )
    },
    error = function(err) {
      cli::cli_abort(
        c(
          "Could not get gist {.field {gist$id}}. Does that gist exist?",
          "*" = "{.url {gist_url_html(gist)}}"
        ),
        parent = err
      )
    }
  )
}

gist_prefs_read <- function(gist) {
  gist <- maybe_gist(gist)
  stopifnot(inherits(gist, "rsthemes_user_prefs_gist"))

  x <- gist_get(gist)

  if (!gist$file %in% names(x[["files"]])) {
    cli::cli_abort("{.file {gist$file}} is not a file in gist {.field {gist$id}}")
  }

  if (!identical(x[["files"]][[gist$file]][["type"]], "application/json")) {
    cli::cli_abort("{.file {gist$file}} is not a JSON file in gist {.field {gist$id}}")
  }

  contents <- x[["files"]][[gist$file]][["content"]]
  tmpfile <- fs::file_temp(ext = "json")
  on.exit(fs::file_delete(tmpfile))
  writeLines(contents, tmpfile)

  jsonlite::read_json(tmpfile)
}

gist_prefs_write <- function(gist, prefs) {
  gist <- maybe_gist(gist)
  stopifnot(inherits(gist, "rsthemes_user_prefs_gist"))

  x <- gist_get(gist)

  files <- list()
  files[[gist[["file"]]]] <- list(
    content = jsonlite::toJSON(prefs, null = "null", auto_unbox = TRUE, pretty = 2)
  )

  res <- gh::gh(
    "PATCH /gists/{gist_id}",
    gist_id = gist$id,
    .params = list(files = files)
  )
  cli::cli_alert_success("Updated {.file {gist$file}} in gist {.field {gist$id}}")
  invisible(res)
}
