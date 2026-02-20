.sqlfluff_env <- new.env(parent = emptyenv())

VENV_NAME <- "r-sqlfluffr"

#' @noRd
quietly <- function(expr) {
  suppressWarnings(suppressMessages(force(expr)))
}

#' @noRd
is_interactive <- function() interactive()

#' @noRd
ensure_sqlfluff <- function() {
  if (!reticulate::virtualenv_exists(VENV_NAME)) {
    stop("sqlfluff is not installed. Run sqlf_install() to set up.", call. = FALSE)
  }

  reticulate::use_virtualenv(VENV_NAME, required = TRUE)
}

#' @noRd
get_sqlfluff <- function() {
  if (is.null(.sqlfluff_env$sqlfluff)) {
    ensure_sqlfluff()
    .sqlfluff_env$sqlfluff <- quietly(reticulate::import("sqlfluff"))
  }
  .sqlfluff_env$sqlfluff
}

#' @noRd
get_sqlfluff_core <- function() {
  if (is.null(.sqlfluff_env$core)) {
    get_sqlfluff()
    .sqlfluff_env$core <- quietly(reticulate::import("sqlfluff.core"))
  }
  .sqlfluff_env$core
}
