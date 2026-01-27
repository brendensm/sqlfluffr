.sqlfluff_env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  # Lazy import: nothing to do here
}

#' @noRd
get_sqlfluff <- function() {
  if (is.null(.sqlfluff_env$sqlfluff)) {
    assert_sqlfluff_installed()
    .sqlfluff_env$sqlfluff <- reticulate::import("sqlfluff")
  }
  .sqlfluff_env$sqlfluff
}

#' @noRd
get_sqlfluff_core <- function() {
  if (is.null(.sqlfluff_env$core)) {
    assert_sqlfluff_installed()
    .sqlfluff_env$core <- reticulate::import("sqlfluff.core")
  }
  .sqlfluff_env$core
}
