#' @noRd
check_parse_errors <- function(sql_text, fluff_config, glue) {
  sf <- get_sqlfluff()

  result <- sf$lint(sql_text, config = fluff_config)
  cols <- c("start_line_no", "start_line_pos", "code", "description", "name")
  df <- py_result_to_df(result, cols)

  prs_errors <- df[grepl("^PRS", df$code), , drop = FALSE]

  if (nrow(prs_errors) == 0L) return(invisible(NULL))

  err_lines <- vapply(seq_len(nrow(prs_errors)), function(i) {
    sprintf("  Line %s, Pos %s [%s]: %s",
            prs_errors$start_line_no[i],
            prs_errors$start_line_pos[i],
            prs_errors$code[i],
            prs_errors$description[i])
  }, character(1))

  msg <- paste0(
    "SQL could not be fixed due to parse error(s):\n",
    paste(err_lines, collapse = "\n")
  )

  if (!isTRUE(glue) && grepl("\\{", sql_text)) {
    msg <- paste0(
      msg,
      "\n\nHint: SQL contains '{...}' patterns. ",
      "If using glue::glue_sql(), try sqlf_fix(..., glue = TRUE)."
    )
  }

  warning(msg, call. = FALSE)
}

#' Fix a SQL query
#'
#' Automatically fixes style and syntax violations in a SQL string or file
#' using sqlfluff.
#'
#' @param sql A SQL string to fix.
#' @param file Path to a SQL file to fix.
#' @param dialect SQL dialect (e.g. `"ansi"`, `"bigquery"`, `"postgres"`).
#' @param rules Character vector of rule codes to apply.
#' @param exclude_rules Character vector of rule codes to skip.
#' @param config A [sqlf_config()] object.
#' @param glue If `TRUE`, treat `\{var\}` placeholders as `glue::glue_sql`
#'   variables and preserve them in the fixed output. `NULL` (the default)
#'   reads the `glue` setting from the project `.sqlfluff` config file;
#'   `FALSE` explicitly disables glue handling regardless of project config.
#' @param overwrite If `TRUE` and `file` was provided, overwrite the file with
#'   fixed SQL. If `FALSE` (default), the fixed SQL is returned without
#'   modifying the file.
#' @param cat If `TRUE` (the default when `overwrite` is `FALSE`), print the
#'   fixed SQL to the console with [cat()] for easy copy-paste. The fixed
#'   string is still returned invisibly.
#'
#' @returns The fixed SQL string (invisibly when printed via `cat`).
#'
#' @examplesIf reticulate::virtualenv_exists("r-sqlfluffr")
#' sqlf_fix(sql = "SELECT  a,b from t where x=1\n")
#'
#' @section Common parsing issues:
#' sqlfluff cannot fix SQL that fails to parse. When this happens, the
#' original SQL is returned unchanged and a warning is issued. Common
#' causes include:
#'
#' \itemize{
#'   \item Missing parentheses after IN: use `WHERE x IN (1)` not
#'     `WHERE x IN 1`.
#'   \item "IS NOT IN" syntax: use `WHERE x NOT IN (...)` instead of
#'     `WHERE x IS NOT IN (...)`.
#'   \item Glue placeholders without `glue = TRUE`: if your SQL contains
#'     `\{var\}` placeholders from [glue::glue_sql()], pass `glue = TRUE`
#'     so sqlfluff treats them as template variables rather than syntax errors.
#' }
#'
#' @export
sqlf_fix <- function(sql = NULL, file = NULL, dialect = NULL,
                         rules = NULL, exclude_rules = NULL,
                         config = NULL, glue = NULL,
                         overwrite = FALSE, cat = !overwrite) {
  sql_text <- resolve_sql_input(sql, file)
  sf <- get_sqlfluff()

  fluff_config <- resolve_config(
    dialect = dialect, rules = rules, exclude_rules = exclude_rules,
    config = config, glue = glue, sql = sql_text
  )

  result <- sf$fix(sql_text, config = fluff_config)

  fixed_sql <- as.character(result)

  if (identical(fixed_sql, sql_text)) {
    check_parse_errors(sql_text, fluff_config, glue)
  }

  if (!is.null(file)) {
    if (isTRUE(overwrite)) {
      writeLines(fixed_sql, file)
      message("File overwritten: ", file)
    } else {
      message("File not modified. Use `overwrite = TRUE` to overwrite.")
    }
    return(invisible(fixed_sql))
  }

  if (isTRUE(cat)) {
    cat(fixed_sql)
    return(invisible(fixed_sql))
  }

  fixed_sql
}
