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

#' Read settings from the [sqlfluff] and [sqlfluffr] sections of a config file.
#' @noRd
read_project_config <- function(path = ".sqlfluff") {
  if (!file.exists(path)) return(list())

  lines <- readLines(path, warn = FALSE)
  current_section <- ""
  settings <- list()

  for (line in lines) {
    line <- trimws(line)
    if (line == "" || startsWith(line, "#")) next
    if (startsWith(line, "[")) {
      current_section <- line
      next
    }
    if (current_section %in% c("[sqlfluff]", "[sqlfluffr]") && grepl("=", line)) {
      parts <- strsplit(line, "=", fixed = TRUE)[[1]]
      key <- trimws(parts[1])
      value <- trimws(paste(parts[-1], collapse = "="))
      settings[[key]] <- value
    }
  }

  settings
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

  # Fill in NULLs from project .sqlfluff file if it exists
  project <- read_project_config()
  if (is.null(dialect)) dialect <- project[["dialect"]]
  if (is.null(rules)) rules <- project[["rules"]]
  if (is.null(exclude_rules)) exclude_rules <- project[["exclude_rules"]]
  if (is.null(glue)) glue <- identical(project[["glue"]], "true")
  max_line_length <- project[["max_line_length"]]

  # Collect any extra project settings beyond the known keys
  known_keys <- c("dialect", "rules", "exclude_rules", "max_line_length")
  extra <- project[setdiff(names(project), known_keys)]

  has_settings <- !is.null(dialect) || !is.null(rules) ||
    !is.null(exclude_rules) || !is.null(max_line_length) || length(extra) > 0L

  # If glue mode, build config with placeholder templater
  if (isTRUE(glue)) {
    cfg <- do.call(sqlf_config, c(
      list(dialect = dialect, rules = rules, exclude_rules = exclude_rules,
           max_line_length = max_line_length),
      extra
    ))
    return(build_fluff_config(cfg, glue = TRUE, sql = sql))
  }

  # Otherwise build a simple config from arguments
  if (has_settings) {
    cfg <- do.call(sqlf_config, c(
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
