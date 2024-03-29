pkgload::load_all(here::here())
library(purrr)

tags_gh <- gh::gh(
  "/repos/rstudio/rstudio/git/refs/tags"
)

tags <-
  tags_gh %>%
  map_dfr(~ list(
    tag = .x$ref,
    sha = .x$object$sha %||% NA_character_,
    tag_url = .x$url
  ))

# tags are released versions of RStudio IDE
# the prefs schema json was added in 1.3
tags <- tags[tags$tag >= "refs/tags/v1.3", ]

tags_names <- sub("refs/tags/v?", "", tags$tag)
tags_ref <- c(set_names(tags$tag, tags_names), latest = "main")

revisions <- map(tags_ref, function(ref) {
  gh::gh(
    "/repos/rstudio/rstudio/contents/{file_path}",
    file_path = "src/cpp/session/resources/schema/user-prefs-schema.json",
    ref = ref
  )
})

versions <- revisions %>% map_dfr(`[`, "content", .id = "ref")
versions$digest <- map_chr(versions$content, digest::digest)
versions <- versions[!duplicated(versions$digest) | versions$ref == "latest", ]

rstudio_prefs <-
  versions[c("content", "ref")] %>%
  reduce(set_names) %>%
  map(function(content) {
    content <- base64enc::base64decode(content)
    content <- rawToChar(content)
    prefs_schema_prepare(content)
  })

# Write a JSON version of prefs for better diffs
jsonlite::write_json(
  rstudio_prefs,
  here::here("data-raw/rstudio_prefs.json"),
  pretty = TRUE,
  auto_unbox = TRUE,
  null = "null",
  force = TRUE
)

prefs_rstudio_v <- rstudio_prefs[setdiff(names(rstudio_prefs), "latest")]
prefs_rstudio <- rstudio_prefs$latest

saveRDS(prefs_rstudio, here::here("inst", "prefs_rstudio.rds"), version = 3)
usethis::use_data(prefs_rstudio_v, overwrite = TRUE)
