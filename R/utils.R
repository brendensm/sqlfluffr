#' @noRd
#' @importFrom stats setNames
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

#' @noRd
resolve_config <- function(dialect, rules, exclude_rules, config, glue, sql) {
  # If a sqlf_config object is supplied, use it
  if (!is.null(config)) {
    if (!inherits(config, "sqlf_config")) {
      stop("`config` must be created by `sqlf_config()`.", call. = FALSE)
    }
    return(build_fluff_config(config, glue = glue, sql = sql))
  }

  # If glue mode, build config with placeholder templater

  if (isTRUE(glue)) {
    cfg <- sqlf_config(dialect = dialect, rules = rules,
                           exclude_rules = exclude_rules)
    return(build_fluff_config(cfg, glue = TRUE, sql = sql))
  }

  # Otherwise build a simple config from arguments
  if (!is.null(dialect) || !is.null(rules) || !is.null(exclude_rules)) {
    cfg <- sqlf_config(dialect = dialect, rules = rules,
                           exclude_rules = exclude_rules)
    return(build_fluff_config(cfg, glue = FALSE, sql = sql))
  }

  NULL
}

#' @noRd
py_result_to_df <- function(result, columns) {
  if (length(result) == 0L) {
    return(as.data.frame(
      setNames(
        replicate(length(columns), character(0), simplify = FALSE),
        columns
      ),
      stringsAsFactors = FALSE
    ))
  }

  rows <- lapply(result, function(item) {
    vals <- lapply(columns, function(col) {
      v <- item[[col]]
      if (is.null(v)) NA else v
    })
    setNames(vals, columns)
  })

  do.call(rbind.data.frame, c(rows, list(stringsAsFactors = FALSE)))
}
