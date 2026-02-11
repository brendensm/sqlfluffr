#' @noRd
new_sqlf_config <- function(dialect = NULL, rules = NULL,
                            exclude_rules = NULL, max_line_length = NULL,
                            config_path = NULL, ...) {
  cfg <- list(
    dialect = dialect,
    rules = rules,
    exclude_rules = exclude_rules,
    max_line_length = max_line_length,
    config_path = config_path,
    extra = list(...)
  )
  structure(cfg, class = "sqlf_config")
}

#' Convert a sqlf_config object to INI-style lines for the [sqlfluff] section.
#' @noRd
config_to_lines <- function(cfg) {
  lines <- "[sqlfluff]"
  if (!is.null(cfg$dialect)) {
    lines <- c(lines, paste0("dialect = ", cfg$dialect))
  }
  if (!is.null(cfg$rules)) {
    lines <- c(lines, paste0("rules = ", paste(cfg$rules, collapse = ",")))
  }
  if (!is.null(cfg$exclude_rules)) {
    lines <- c(lines, paste0("exclude_rules = ",
                              paste(cfg$exclude_rules, collapse = ",")))
  }
  if (!is.null(cfg$max_line_length)) {
    lines <- c(lines, paste0("max_line_length = ", cfg$max_line_length))
  }
  for (nm in names(cfg$extra)) {
    lines <- c(lines, paste0(nm, " = ", cfg$extra[[nm]]))
  }
  lines
}

#' Write a project-level sqlfluff configuration file
#'
#' Writes a `.sqlfluff` configuration file that sqlfluff automatically
#' discovers. Once written, [sqlf_lint()], [sqlf_fix()], and [sqlf_parse()]
#' will use these settings without needing explicit arguments.
#'
#' @param dialect SQL dialect name (e.g. `"ansi"`, `"bigquery"`, `"postgres"`,
#'   `"teradata"`). See [sqlf_dialects()] for available options.
#' @param rules Character vector of rule codes to enable.
#' @param exclude_rules Character vector of rule codes to exclude.
#' @param max_line_length Maximum allowed line length.
#' @param glue If `TRUE`, enables [glue::glue_sql()] placeholder handling
#'   by default for all lint, fix, and parse calls.
#' @param path File path to write. Defaults to `".sqlfluff"` in the current
#'   working directory.
#' @param overwrite If `FALSE` (default), refuses to overwrite an existing
#'   file. Set to `TRUE` to replace it.
#' @param ... Additional settings as named arguments. These are added to the
#'   `[sqlfluff]` section of the configuration.
#'
#' @returns The file path, invisibly.
#'
#' @examples
#' path <- tempfile(fileext = ".sqlfluff")
#' sqlf_config(dialect = "postgres", path = path)
#' readLines(path)
#' unlink(path)
#'
#' @export
sqlf_config <- function(dialect = NULL, rules = NULL,
                        exclude_rules = NULL, max_line_length = NULL,
                        glue = NULL, path = ".sqlfluff",
                        overwrite = FALSE, ...) {
  if (!is.null(max_line_length)) {
    if (!is.numeric(max_line_length) || length(max_line_length) != 1L ||
        is.na(max_line_length) || max_line_length < 1) {
      stop("`max_line_length` must be a positive number.", call. = FALSE)
    }
  }

  if (file.exists(path) && !isTRUE(overwrite)) {
    stop("File already exists: ", path,
         "\nUse overwrite = TRUE to replace it.", call. = FALSE)
  }

  cfg <- new_sqlf_config(
    dialect = dialect, rules = rules, exclude_rules = exclude_rules,
    max_line_length = max_line_length, ...
  )
  lines <- config_to_lines(cfg)

  if (isTRUE(glue)) {
    lines <- c(lines, "", "[sqlfluffr]", "glue = true")
  }

  writeLines(lines, path)
  message("Config written to: ", path)
  invisible(path)
}

#' Open the sqlfluff configuration file for editing
#'
#' Opens the `.sqlfluff` configuration file in the default editor.
#' In RStudio, this opens the file in the source pane.
#'
#' @param path Path to the configuration file. Defaults to `".sqlfluff"`.
#'
#' @returns The file path, invisibly.
#'
#' @export
sqlf_config_edit <- function(path = ".sqlfluff") {
  if (!file.exists(path)) {
    stop("No config file found at: ", path,
         "\nCreate one with sqlf_config().", call. = FALSE)
  }
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    rstudioapi::navigateToFile(path)
  } else {
    utils::file.edit(path)
  }
  invisible(path)
}

#' @noRd
build_fluff_config <- function(cfg, glue = FALSE, sql = NULL) {
  core <- get_sqlfluff_core()

  if (!is.null(cfg$config_path)) {
    return(core$FluffConfig$from_path(path = cfg$config_path))
  }

  if (isTRUE(glue) && !is.null(sql)) {
    vars <- extract_glue_vars(sql)
    # Collect user core settings to pass through
    user_core <- list()
    if (!is.null(cfg$rules)) {
      user_core[["rules"]] <- paste(cfg$rules, collapse = ",")
    }
    if (!is.null(cfg$exclude_rules)) {
      user_core[["exclude_rules"]] <- paste(cfg$exclude_rules, collapse = ",")
    }
    if (!is.null(cfg$max_line_length)) {
      user_core[["max_line_length"]] <- as.character(cfg$max_line_length)
    }
    for (nm in names(cfg$extra)) {
      user_core[[nm]] <- as.character(cfg$extra[[nm]])
    }

    config_str <- build_glue_config_str(
      vars = vars,
      dialect = cfg$dialect,
      user_config = if (length(user_core) > 0L) user_core else NULL
    )
    return(core$FluffConfig$from_string(config_str))
  }

  # Build a config string from the helper
  lines <- config_to_lines(cfg)

  if (length(lines) > 1L) {
    config_str <- paste(lines, collapse = "\n")
    return(core$FluffConfig$from_string(config_str))
  }

  NULL
}
