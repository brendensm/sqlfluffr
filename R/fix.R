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
#'   variables and preserve them in the fixed output.
#' @param force If `TRUE` and `file` was provided, overwrite the file with
#'   fixed SQL. If `FALSE` (default), the fixed SQL is returned without
#'   modifying the file.
#'
#' @return The fixed SQL string (invisibly if the file was overwritten).
#' @export
sqlf_fix <- function(sql = NULL, file = NULL, dialect = NULL,
                         rules = NULL, exclude_rules = NULL,
                         config = NULL, glue = FALSE, force = FALSE) {
  sql_text <- resolve_sql_input(sql, file)
  sf <- get_sqlfluff()

  fluff_config <- resolve_config(
    dialect = dialect, rules = rules, exclude_rules = exclude_rules,
    config = config, glue = glue, sql = sql_text
  )

  result <- sf$fix(sql_text, config = fluff_config)

  fixed_sql <- as.character(result)

  if (!is.null(file)) {
    if (isTRUE(force)) {
      writeLines(fixed_sql, file)
      message("File overwritten: ", file)
      return(invisible(fixed_sql))
    } else {
      message("File not modified. Use `force = TRUE` to overwrite.")
    }
  }

  fixed_sql
}
