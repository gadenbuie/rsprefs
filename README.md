
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

## Quick Start

1.  Snapshot your RStudio preferences to a new GitHub gist. Set
    `path = "new private gist"` to keep your preferences private.

    ``` r
    rsprefs::rs_prefs_snapshot(name = "sync", path = "new gist")
    ```

        ✓ Created gist with id 96e8c7f0a624bfed8018987185feb30b
        ℹ Use this gist automatically by adding the following to your ~/.Rprofile.
        Call `usethis::edit_r_profile()`     

        options(rsprefs.gist_id = "96e8c7f0a624bfed8018987185feb30b")

        ✓ Creating gist with user-prefs.json ... done
        ✓ Writing snapshot sync to new gist ... done

2.  Open your `~/.Rprofile` using `usethis::edit_r_profile()` and add
    the `rsprefs.gist_id` option from the output above.

3.  If you change your preferences you can update your snapshot by name,
    using the gist ID found from the global option.

    ``` r
    rsprefs::rs_prefs_snapshot("sync")
    ```

4.  On a different computer or session, setup the `rsprefs.gist_id`
    option and then apply your snapshot.

    ``` r
    rsprefs::rs_prefs_snapshot_apply("sync")
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

rsprefs::rs_prefs_snapshot("demo", path = tmp_snap)
#> ℹ Writing snapshot demo to 'tmp_snap.json' ... done
```

You can list available preference snapshots with
`rs_prefs_snapshot_list()`

``` r
rsprefs::rs_prefs_snapshot_list(path = tmp_snap)
#> Snapshot: 'tmp_snap.json'
#> • demo
```

or you can apply the preferences in a snapshot with
`rs_prefs_snapshot_apply()`. (You can also set `preview = TRUE` to
preview the preferences included in the snapshot without applying them.)

``` r
rsprefs::rs_prefs_snapshot_apply(
  name = "demo", 
  path = tmp_snap, 
  preview = TRUE
)
#> <snapshot "demo">
#> check_arguments_to_r_function_calls: true
#> check_unexpected_assignment_in_function_call: true
#> console_double_click_select: true
#> continue_comments_on_newline: true
#> default_project_location: "~/Repos/working"
#> document_author: "Luke Skywalker"
#> editor_keybindings: "vim"
#> editor_theme: "Night Owl {rsthemes}"
#> font_size_points: 12
#> highlight_r_function_calls: true
#> ... and 21 more
```

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
rsprefs::rs_prefs_snapshot(
  name = "favorite_panes", 
  path = tmp_snap,
  include = "panes"
)
#> ℹ Writing snapshot favorite_panes to 'tmp_snap.json'✓ Writing snapshot favorite_panes to 'tmp_snap.json' ... done

rsprefs::rs_prefs_snapshot_apply(
  name = "favorite_panes",
  path = tmp_snap,
  preview = TRUE
)
#> <snapshot "favorite_panes">
#> panes: {"quadrants":["Source","TabSet1","Console","TabSet2"],"tabSet1":["Hi…
```

## Explore preferences

You can obtain a list of available preferences for your version of
RStudio using:

``` r
schema <- rsprefs::rstudio_prefs_schema()
schema
#> run_rprofile_on_resume: false
#> save_workspace: "ask"
#> load_workspace: true
#> initial_working_directory: ""
#> cran_mirror: {"name":"Global (CDN)","host":"RStudio","url":"https://cran.rs…
#> bioconductor_mirror_name: "Seattle (USA)"
#> bioconductor_mirror_url: "http://www.bioconductor.org"
#> always_save_history: true
#> remove_history_duplicates: false
#> show_last_dot_value: false
#> ... and 220 more
```

This list contains detailed information about every preference, so you
can use autocomplete to explore the preferences.

``` r
schema$highlight_r_function_calls
#> <highlight_r_function_calls>
#> Whether to highlight R function calls in the code editor.
#> Type: boolean
#> Default: false
#> Current: true
```

``` r
schema$insert_native_pipe_operator
#> <insert_native_pipe_operator>
#> Whether the Insert Pipe Operator command should insert the native R pipe
#> operator, |>
#> Type: boolean
#> Default: false
#> Current: false
```
