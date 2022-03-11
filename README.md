
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rsprefs

<!-- badges: start -->
<!-- badges: end -->

The goal of rsprefs is to help you manage your RStudio preferences.

## Installation

You can install the development version of rsprefs from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("gadenbuie/rsprefs")
```

``` r
library(rsprefs)
```

## Quick Start

1.  Snapshot your RStudio preferences to a new GitHub gist. Set
    `path = "new private gist"` to keep your preferences private.

    ``` r
    snapshot_prefs_save(name = "sync", path = "new gist")
    ```

        ✓ Created gist with id 96e8c7f0a624bfed8018987185feb30b
        ℹ Use this gist automatically by adding the following to your ~/.Rprofile.
        Call `usethis::edit_r_profile()`     

        options(rsprefs.gist_id = "96e8c7f0a624bfed8018987185feb30b")

        ✓ Creating gist with user-prefs.json ... done
        ✓ Writing snapshot sync to new gist ... done

2.  Open your `~/.Rprofile` using `usethis::edit_r_profile()` and add
    the `rsprefs.gist_id` option from the output above.

3.  If you change your IDE preferences you can update your snapshot by
    name, using the gist ID found from the global option.

    ``` r
    snapshot_prefs_save("sync", overwrite = TRUE)
    ```

4.  On a different computer or session, setup the `rsprefs.gist_id`
    option and then apply your snapshot.

    ``` r
    snapshot_prefs_use("sync")
    ```

## Manage Your RStudio Preferences

rsprefs stores snapshots of your RStudio preferences. A snapshot can
contain all of your preferences, or just a small selection of
preferences. Snapshots are named and can be stored locally or in a
[GitHub gist](https://gist.github.com).

### Saving your RStudio preferences locally

By default, rsprefs will store snapshots in a JSON file in
`rappdirs::user_config_dir("rsprefs")`, but you can choose where to
store snapshots with the `path` argument.

In these examples, I’ll use a temporary file for demonstration purposes.
Here I’ll take a snapshot of my current preferences.

``` r
tmp_snap <- "tmp_snap.json"

snapshot_prefs_save("demo", path = tmp_snap)
#> ℹ Writing snapshot demo to 'tmp_snap.json'✓ Writing snapshot demo to 'tmp_snap.json' ... done
```

You can list available preference snapshots with `snapshot_prefs_list()`

``` r
snapshot_prefs_list(path = tmp_snap)
#> Snapshot: 'tmp_snap.json'
#> • demo
```

or you can apply the preferences in a snapshot with
`snapshot_prefs_use()`. (You can also set `preview = TRUE` to preview
the preferences included in the snapshot without applying them.)

``` r
snapshot_prefs_use(
  name = "demo", 
  path = tmp_snap, 
  preview = TRUE
)
#> <snapshot "demo">
#> auto_detect_indentation: true
#> check_arguments_to_r_function_calls: true
#> check_unexpected_assignment_in_function_call: true
#> console_double_click_select: true
#> continue_comments_on_newline: true
#> doc_outline_show: "sections_and_chunks"
#> document_author: "Luke Skywalker"
#> editor_keybindings: "vim"
#> editor_theme: "Horizon Dark {rsthemes}"
#> font_size_points: 12
#> ... and 26 more
```

If you apply the snapshot and change your mind, you can roll back the
changes from the snapshot by calling `snapshot_prefs_undo()`.

### Choose which preferences to include in the snapshot

Snapshots don’t need to include *every* preference in RStudio. Instead,
you can use snapshot to store specific groups of preferences. There are
three arguments that control which preferences are included:

-   `include`: Names of RStudio preferences to include. If provided,
    only these preferences are included.

-   `exclude`: Names of RStudio preferences to exclude from the
    snapshot.

-   `source`: The source of the current preference value. Preferences
    are set at different levels, from lowest to highest precedence:

    -   `"default"` are RStudio’s built-in defaults
    -   `"computed"` are detected or supplied from external sources,
        e.g. the path to `git` on your system
    -   `"system"` are derived from system-wide `rstudio-prefs.json`
    -   `"user"` are set by the user for all sessions (global options)
    -   `"project"` are set by the current project session

    The default is `"user"`, since these are the preferences you’ve most
    intentionally set for your IDE. Also, `"project"` preferences are
    already stored in the `.Rproj` file.

For example, you could store your favorite pane layout as
`"favorite_panes"`:

``` r
snapshot_prefs_save(
  name = "favorite_panes", 
  path = tmp_snap,
  include = "panes"
)
#> ℹ Writing snapshot favorite_panes to 'tmp_snap.json'✓ Writing snapshot favorite_panes to 'tmp_snap.json' ... done

snapshot_prefs_use(
  name = "favorite_panes",
  path = tmp_snap,
  preview = TRUE
)
#> <snapshot "favorite_panes">
#> panes: {"quadrants":["Source","TabSet1","Console","TabSet2"],"tabSet1":["History","Connections","Packages","Presentation"],"tabSet2":["Environm…
```

### Reset RStudio’s Defaults

Occasionally, you might want to reset all of your preferences to
RStudio’s “factory defaults”, for example when teaching or working with
a beginner. To quickly reset all defaults, call

``` r
prefs_reset_defaults()
```

If you want to return to your personal preferences, you can undo the
change to the defaults with

``` r
snapshot_prefs_undo()
```

For a longer-term return to RStudio’s defaults, make sure that you’ve
taken [a snapshot of your personal preferences](#quick-start) before you
reset.

## Explore preferences

You can obtain a list of available preferences for your version of
RStudio using:

``` r
schema <- prefs_schema()
#> ! Using preference schema from RStudio v2022.02.0+443
#> x Schema for current version v2022.6.0.195 is not available online (or is not different). 
```

rsprefs includes the preference information for RStudio releases since
1.3.959. If you’re using dailies or preview versions fo RStudio, rsprefs
will attempt to download the schema that matches your version, falling
back to the latest version if one is not available.

For quick reference, the latest preference details are included in the
`prefs_rstudio` object.

``` r
prefs_rstudio
#> run_rprofile_on_resume: false
#> save_workspace: "ask"
#> load_workspace: true
#> initial_working_directory: ""
#> cran_mirror: {"name":"Global (CDN)","host":"RStudio","url":"https://cran.rstudio.com/","repos":"","country":"us","secondary":""}
#> bioconductor_mirror_name: "Seattle (USA)"
#> bioconductor_mirror_url: "http://www.bioconductor.org"
#> always_save_history: true
#> remove_history_duplicates: false
#> show_last_dot_value: false
#> ... and 223 more
```

This list contains detailed information about every preference, so you
can use autocomplete to explore the preferences.

``` r
prefs_rstudio$highlight_r_function_calls
#> <highlight_r_function_calls>
#> Whether to highlight R function calls in the code editor.
#> Type: boolean
#> Default: false
#> Current: true
```

``` r
prefs_rstudio$insert_native_pipe_operator
#> <insert_native_pipe_operator>
#> Whether the Insert Pipe Operator command should insert the native R pipe operator, |>
#> Type: boolean
#> Default: false
#> Current: false
```

The `prefs_rstudio` object also comes with helper functions to get, set
and toggle preferences:

``` r
# $get() current preference value
original <- prefs_rstudio$insert_native_pipe_operator$get()
original
#> [1] FALSE

# $toggle() boolean preferences
prefs_rstudio$insert_native_pipe_operator$toggle()
#> ✓ Enabled insert_native_pipe_operator

# $set() the preference directly
prefs_rstudio$insert_native_pipe_operator$set(original)

# check that we've come full circle
prefs_rstudio$insert_native_pipe_operator$get()
#> [1] FALSE
```
