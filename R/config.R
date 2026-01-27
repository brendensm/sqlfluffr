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
