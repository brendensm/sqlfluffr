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
#' @returns A data.frame of class `"sqlf_lint_results"` with columns
#'   `line_no`, `line_pos`, `code`, `description`, and `name`.
#'   Returns a zero-row data.frame if there are no violations.
#'
#' @examplesIf reticulate::py_module_available("sqlfluff")
#' sqlf_lint(sql = "SELECT  a,b from t where x=1\n")
#' sqlf_lint(sql = "SELECT TOP 10 * FROM t\n", dialect = "tsql")
#'
#' @export
sqlf_lint <- function(sql = NULL, file = NULL, dialect = NULL,
                          rules = NULL, exclude_rules = NULL,
                          config = NULL, glue = NULL) {
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
print.sqlf_lint_results <- function(x, n = 10, width = getOption("width", 80), ...) {
  if (nrow(x) == 0L) {
    cat("No linting violations found.\n")
    return(invisible(x))
  }

  n_total <- nrow(x)
  cat(sprintf("# sqlf_lint_results: %d violation%s\n",
              n_total, if (n_total == 1L) "" else "s"))

  # Select display columns (drop 'name' if present)

  display_cols <- intersect(c("line_no", "line_pos", "code", "description"), names(x))
  df <- x[, display_cols, drop = FALSE]

  # Limit rows
  n_show <- min(n, n_total)
  df <- df[seq_len(n_show), , drop = FALSE]

  # Calculate column widths
  col_types <- c(line_no = "int", line_pos = "int", code = "chr", description = "chr")
  col_widths <- vapply(display_cols, function(col) {
    max(nchar(col), nchar(as.character(df[[col]])), nchar(paste0("<", col_types[col], ">")))
  }, integer(1))

  # Calculate available width for description
  fixed_width <- sum(col_widths[display_cols != "description"]) +
                 length(display_cols) * 2 + # spacing
                 nchar(as.character(n_total)) + 1 # row numbers
  desc_width <- max(20, width - fixed_width - 4)
  col_widths["description"] <- min(col_widths["description"], desc_width)

  # Truncate descriptions
  df$description <- vapply(df$description, function(d) {
    if (nchar(d) > desc_width) {
      paste0(substr(d, 1, desc_width - 3), "...")
    } else {
      d
    }
  }, character(1))

  # Print header row
  row_num_width <- nchar(as.character(n_total))
  header <- paste0(
    strrep(" ", row_num_width + 1),
    paste(mapply(function(col, w) format(col, width = w), display_cols, col_widths), collapse = " ")
  )
  cat(header, "\n")

  # Print type row
  types_row <- paste0(
    strrep(" ", row_num_width + 1),
    paste(mapply(function(col, w) format(paste0("<", col_types[col], ">"), width = w),
                 display_cols, col_widths), collapse = " ")
  )
  cat(types_row, "\n")

  # Print data rows
  for (i in seq_len(n_show)) {
    row_num <- format(i, width = row_num_width)
    row_vals <- vapply(display_cols, function(col) {
      val <- as.character(df[[col]][i])
      format(val, width = col_widths[col])
    }, character(1))
    cat(row_num, paste(row_vals, collapse = " "), "\n")
  }

  # Print footer if there are more rows
  remaining <- n_total - n_show
  if (remaining > 0L) {
    cat(sprintf("# ... and %d more row%s\n", remaining, if (remaining == 1L) "" else "s"))
  }

  invisible(x)
}
