#' @noRd
resolve_sql_input <- function(sql, file) {
  if (!is.null(sql) && !is.null(file)) {
    stop("Provide either `sql` or `file`, not both.", call. = FALSE)
  }

  if (is.null(sql) && is.null(file)) {
    stop("Provide either `sql` or `file`.", call. = FALSE)
  }

  if (!is.null(file)) {
    if (!file.exists(file)) {
      stop("File not found: ", file, call. = FALSE)
    }
    sql <- paste(readLines(file, warn = FALSE), collapse = "\n")
  }

  sql
}

#' Read settings from the [sqlfluff] and [sqlfluffr] sections of a config file.
#' @noRd
read_project_config <- function(path = ".sqlfluff") {
  if (!file.exists(path)) return(list(sqlfluff = list(), sqlfluffr = list()))

  lines <- readLines(path, warn = FALSE)
  current_section <- ""
  sqlfluff_settings <- list()
  sqlfluffr_settings <- list()

  for (line in lines) {
    line <- trimws(line)
    if (line == "" || startsWith(line, "#")) next
    if (startsWith(line, "[")) {
      current_section <- line
      next
    }
    if (grepl("=", line)) {
      parts <- strsplit(line, "=", fixed = TRUE)[[1]]
      key <- trimws(parts[1])
      value <- trimws(paste(parts[-1], collapse = "="))
      if (current_section == "[sqlfluff]") {
        sqlfluff_settings[[key]] <- value
      } else if (current_section == "[sqlfluffr]") {
        sqlfluffr_settings[[key]] <- value
      }
    }
  }

  list(sqlfluff = sqlfluff_settings, sqlfluffr = sqlfluffr_settings)
}

#' @noRd
check_scalar_string <- function(x, name) {
  if (is.null(x)) return(invisible(NULL))
  if (!is.character(x) || length(x) != 1L) {
    stop(sprintf("`%s` must be a single string or NULL.", name), call. = FALSE)
  }
  invisible(NULL)
}

#' @noRd
check_character_or_null <- function(x, name) {
  if (is.null(x)) return(invisible(NULL))
  if (!is.character(x)) {
    stop(sprintf("`%s` must be a character vector or NULL.", name), call. = FALSE)
  }
  invisible(NULL)
}

#' @noRd
resolve_config <- function(dialect, rules, exclude_rules, config, glue, sql) {
  check_scalar_string(dialect, "dialect")
  check_character_or_null(rules, "rules")
  check_character_or_null(exclude_rules, "exclude_rules")

  # If a sqlf_config object is supplied, use it
  if (!is.null(config)) {
    if (!inherits(config, "sqlf_config")) {
      stop("`config` must be created by `sqlf_config()`.", call. = FALSE)
    }
    return(build_fluff_config(config, glue = glue, sql = sql))
  }

  # Fill in NULLs from project .sqlfluff file if it exists
  project <- read_project_config()
  sf_cfg <- project$sqlfluff
  sfr_cfg <- project$sqlfluffr

  if (is.null(dialect)) dialect <- sf_cfg[["dialect"]]
  if (is.null(rules)) rules <- sf_cfg[["rules"]]
  if (is.null(exclude_rules)) exclude_rules <- sf_cfg[["exclude_rules"]]
  if (is.null(glue)) glue <- identical(sfr_cfg[["glue"]], "true")
  max_line_length <- sf_cfg[["max_line_length"]]

  # Collect any extra project settings beyond the known keys
  known_keys <- c("dialect", "rules", "exclude_rules", "max_line_length")
  extra <- sf_cfg[setdiff(names(sf_cfg), known_keys)]

  has_settings <- !is.null(dialect) || !is.null(rules) ||
    !is.null(exclude_rules) || !is.null(max_line_length) || length(extra) > 0L

  # If glue mode, build config with placeholder templater
  if (isTRUE(glue)) {
    cfg <- do.call(new_sqlf_config, c(
      list(dialect = dialect, rules = rules, exclude_rules = exclude_rules,
           max_line_length = max_line_length),
      extra
    ))
    return(build_fluff_config(cfg, glue = TRUE, sql = sql))
  }

  # Otherwise build a simple config from arguments
  if (has_settings) {
    cfg <- do.call(new_sqlf_config, c(
      list(dialect = dialect, rules = rules, exclude_rules = exclude_rules,
           max_line_length = max_line_length),
      extra
    ))
    return(build_fluff_config(cfg, glue = FALSE, sql = sql))
  }

  NULL
}

#' @noRd
py_result_to_df <- function(result, columns) {
  if (length(result) == 0L) {
    empty <- replicate(length(columns), character(0), simplify = FALSE)
    names(empty) <- columns
    return(as.data.frame(empty, stringsAsFactors = FALSE))
  }

  cols <- lapply(columns, function(col) {
    vapply(result, function(item) {
      v <- item[[col]]
      if (is.null(v)) NA_character_ else as.character(v)
    }, character(1))
  })
  names(cols) <- columns
  as.data.frame(cols, stringsAsFactors = FALSE)
}
