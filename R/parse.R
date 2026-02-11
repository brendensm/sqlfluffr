#' Parse a SQL query
#'
#' Parses a SQL string or file into a syntax tree using sqlfluff.
#'
#' @param sql A SQL string to parse.
#' @param file Path to a SQL file to parse.
#' @param dialect SQL dialect (e.g. `"ansi"`, `"bigquery"`, `"postgres"`).
#' @param config A [sqlf_config()] object.
#' @param glue If `TRUE`, treat `\{var\}` placeholders as `glue::glue_sql`
#'   variables before parsing.
#'
#' @returns A nested list representing the parse tree.
#'
#' @examplesIf reticulate::py_module_available("sqlfluff")
#' sqlf_parse(sql = "SELECT 1\n")
#'
#' @export
sqlf_parse <- function(sql = NULL, file = NULL, dialect = NULL,
                           config = NULL, glue = NULL) {
  sql_text <- resolve_sql_input(sql, file)
  sf <- get_sqlfluff()

  fluff_config <- resolve_config(
    dialect = dialect, rules = NULL, exclude_rules = NULL,
    config = config, glue = glue, sql = sql_text
  )

  sf$parse(sql_text, config = fluff_config)
}
