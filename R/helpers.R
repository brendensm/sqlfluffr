#' List available SQL dialects
#'
#' Returns the dialects supported by the installed version of sqlfluff.
#'
#' @return A data.frame with columns `label`, `name`, and `inherits_from`.
#' @export
sqlf_dialects <- function() {
  sf <- get_sqlfluff()
  result <- sf$list_dialects()
  py_result_to_df(result, c("label", "name", "inherits_from"))
}

#' List available linting rules
#'
#' Returns the rules available in the installed version of sqlfluff.
#'
#' @return A data.frame with columns `code` and `description`.
#' @export
sqlf_rules <- function() {
  sf <- get_sqlfluff()
  result <- sf$list_rules()
  py_result_to_df(result, c("code", "description"))
}
