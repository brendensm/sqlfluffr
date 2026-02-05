#' Create a reusable sqlfluff configuration
#'
#' Builds a configuration object that can be passed to [sqlf_lint()],
#' [sqlf_fix()], and [sqlf_parse()].
#'
#' @param dialect SQL dialect name (e.g. `"ansi"`, `"bigquery"`, `"postgres"`).
#' @param rules Character vector of rule codes to enable.
#' @param exclude_rules Character vector of rule codes to exclude.
#' @param max_line_length Maximum allowed line length.
#' @param config_path Path to an existing `.sqlfluff` configuration file.
#' @param ... Additional settings as named arguments. These are added to the
#'   `[sqlfluff]` section of the configuration.
#'
#' @return An object of class `"sqlf_config"`.
#' @export
sqlf_config <- function(dialect = NULL, rules = NULL,
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

#' @export
print.sqlf_config <- function(x, ...) {
  cat("<sqlf_config>\n")
  if (!is.null(x$dialect)) cat("  dialect:", x$dialect, "\n")
  if (!is.null(x$rules)) cat("  rules:", paste(x$rules, collapse = ", "), "\n")
  if (!is.null(x$exclude_rules)) {
    cat("  exclude_rules:", paste(x$exclude_rules, collapse = ", "), "\n")
  }
  if (!is.null(x$max_line_length)) {
    cat("  max_line_length:", x$max_line_length, "\n")
  }
  if (!is.null(x$config_path)) cat("  config_path:", x$config_path, "\n")
  if (length(x$extra) > 0L) {
    cat("  extra:", paste(names(x$extra), collapse = ", "), "\n")
  }
  invisible(x)
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
#' @return The file path, invisibly.
#'
#' @examples
#' \dontrun{
#' # Set up Teradata dialect with glue support for the project
#' sqlf_config_write(dialect = "teradata", glue = TRUE)
#'
#' # Now lint and fix calls use Teradata + glue automatically
#' sqlf_lint(file = "query.sql")
#' sqlf_fix(file = "query.sql")
#' }
#'
#' @export
sqlf_config_write <- function(dialect = NULL, rules = NULL,
                              exclude_rules = NULL, max_line_length = NULL,
                              glue = NULL, path = ".sqlfluff",
                              overwrite = FALSE, ...) {
  if (file.exists(path) && !isTRUE(overwrite)) {
    stop("File already exists: ", path,
         "\nUse overwrite = TRUE to replace it.", call. = FALSE)
  }

  lines <- "[sqlfluff]"
  if (!is.null(dialect)) {
    lines <- c(lines, paste0("dialect = ", dialect))
  }
  if (!is.null(rules)) {
    lines <- c(lines, paste0("rules = ", paste(rules, collapse = ",")))
  }
  if (!is.null(exclude_rules)) {
    lines <- c(lines, paste0("exclude_rules = ",
                              paste(exclude_rules, collapse = ",")))
  }
  if (!is.null(max_line_length)) {
    lines <- c(lines, paste0("max_line_length = ", max_line_length))
  }

  extra <- list(...)
  for (nm in names(extra)) {
    lines <- c(lines, paste0(nm, " = ", extra[[nm]]))
  }

  if (isTRUE(glue)) {
    lines <- c(lines, "", "[sqlfluffr]", "glue = true")
  }

  writeLines(lines, path)
  message("Config written to: ", path)
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

  # Build a config string and use from_string(), which works across

  # sqlfluff versions without relying on the from_kwargs() signature.
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

  if (length(lines) > 1L) {
    config_str <- paste(lines, collapse = "\n")
    return(core$FluffConfig$from_string(config_str))
  }

  NULL
}
