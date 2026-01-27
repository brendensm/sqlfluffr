#' Lint a SQL query
#'
#' Checks a SQL string or file for style and syntax violations using sqlfluff.
#'
#' @param sql A SQL string to lint.
#' @param file Path to a SQL file to lint.
#' @param dialect SQL dialect (e.g. `"ansi"`, `"bigquery"`, `"postgres"`).
#' @param rules Character vector of rule codes to check.
#' @param exclude_rules Character vector of rule codes to skip.
#' @param config A [sqlf_config()] object.
#' @param glue If `TRUE`, treat `\{var\}` placeholders as `glue::glue_sql`
#'   variables and use the placeholder templater so linting works correctly.
#'
#' @return A data.frame of class `"sqlf_lint_results"` with columns
#'   `line_no`, `line_pos`, `code`, `description`, and `name`.
#'   Returns a zero-row data.frame if there are no violations.
#' @export
sqlf_lint <- function(sql = NULL, file = NULL, dialect = NULL,
                          rules = NULL, exclude_rules = NULL,
                          config = NULL, glue = FALSE) {
  sql_text <- resolve_sql_input(sql, file)
  sf <- get_sqlfluff()

  fluff_config <- resolve_config(
    dialect = dialect, rules = rules, exclude_rules = exclude_rules,
    config = config, glue = glue, sql = sql_text
  )

  result <- sf$lint(sql_text, config = fluff_config)

  cols <- c("start_line_no", "start_line_pos", "code", "description", "name")
  df <- py_result_to_df(result, cols)
  names(df)[names(df) == "start_line_no"] <- "line_no"
  names(df)[names(df) == "start_line_pos"] <- "line_pos"
  class(df) <- c("sqlf_lint_results", class(df))
  df
}

#' @export
print.sqlf_lint_results <- function(x, ...) {
  if (nrow(x) == 0L) {
    cat("No linting violations found.\n")
  } else {
    cat(sprintf("Found %d linting violation(s):\n", nrow(x)))
    print.data.frame(x, right = FALSE, row.names = FALSE)
  }
  invisible(x)
}
