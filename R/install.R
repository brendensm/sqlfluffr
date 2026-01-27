#' Install sqlfluff Python package
#'
#' Installs the sqlfluff Python package into the Python environment used by
#' reticulate.
#'
#' @param method Installation method passed to
#'   [reticulate::py_install()].
#' @param conda Path to conda executable or `"auto"`.
#' @param pip Whether to use pip (default `TRUE`).
#' @param envname Name of the Python environment to install into.
#'   `NULL` uses the default environment.
#' @param version Optional version string, e.g. `"3.0.0"`. If `NULL`,
#'   installs the latest version.
#'
#' @return Invisibly returns `NULL`. Called for its side effect.
#' @export
install_sqlf <- function(method = "auto", conda = "auto", pip = TRUE,
                             envname = NULL, version = NULL) {
  pkg <- if (!is.null(version)) paste0("sqlfluff==", version) else "sqlfluff"
  reticulate::py_install(
    pkg,
    method = method,
    conda = conda,
    pip = pip,
    envname = envname
  )
  # Clear cached module so it is re-imported on next use
  .sqlfluff_env$sqlfluff <- NULL
  .sqlfluff_env$core <- NULL
  invisible(NULL)
}
