.sqlfluff_env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  # Lazy import: nothing to do here
}

#' @noRd
get_sqlfluff <- function() {
  if (is.null(.sqlfluff_env$sqlfluff)) {
    assert_sqlfluff_installed()
    # Suppress warnings from reticulate's internal pip/uv handling
    .sqlfluff_env$sqlfluff <- suppressWarnings(reticulate::import("sqlfluff"))
  }
  .sqlfluff_env$sqlfluff
}

#' @noRd
get_sqlfluff_core <- function() {
  if (is.null(.sqlfluff_env$core)) {
    assert_sqlfluff_installed()
    # Suppress warnings from reticulate's internal pip/uv handling
    .sqlfluff_env$core <- suppressWarnings(reticulate::import("sqlfluff.core"))
  }
  .sqlfluff_env$core
}
