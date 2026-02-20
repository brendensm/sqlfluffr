#' Install sqlfluff Python package
#'
#' Sets up a dedicated Python virtual environment and installs the
#' 'sqlfluff' package into it. This is a one-time setup step that must be
#' run before using any sqlfluffr functions.
#'
#' @param ask If `TRUE` (the default in interactive sessions), prompt for
#'   confirmation before downloading. Set to `FALSE` to skip the prompt.
#' @param force If `TRUE`, reinstall even if 'sqlfluff' is already present.
#'
#' @returns `NULL`, invisibly.
#'
#' @examples
#' \dontrun{
#' sqlf_install()
#' }
#'
#' @export
sqlf_install <- function(ask = interactive(), force = FALSE) {
  envname <- "r-sqlfluffr"

  if (!force && reticulate::virtualenv_exists(envname)) {
    pkgs <- reticulate::py_list_packages(envname)
    if ("sqlfluff" %in% pkgs$package) {
      message("sqlfluff is already installed. Use force = TRUE to reinstall.")
      return(invisible(NULL))
    }
  }

  if (isTRUE(ask)) {
    message("This will install Python and sqlfluff (~50 MB).")
    ans <- utils::menu(c("Yes", "No"), title = "Proceed?")
    if (ans != 1L) {
      stop("Installation cancelled.", call. = FALSE)
    }
  }

  if (!reticulate::virtualenv_exists(envname)) {
    t <- try(reticulate::virtualenv_create(envname), silent = TRUE)
    if (inherits(t, "try-error")) {
      python <- reticulate::install_python()
      reticulate::virtualenv_create(envname, python = python)
    }
  }

  reticulate::py_install("sqlfluff", envname = envname)
  message("sqlfluff installed successfully in virtualenv '", envname, "'.")
  invisible(NULL)
}

#' List available SQL dialects
#'
#' Returns the dialects supported by the installed version of sqlfluff.
#'
#' @returns A data.frame with columns `label`, `name`, and `inherits_from`.
#'
#' @examplesIf reticulate::virtualenv_exists("r-sqlfluffr")
#' sqlf_dialects()
#'
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
#' @returns A data.frame with columns `code` and `description`.
#'
#' @examplesIf reticulate::virtualenv_exists("r-sqlfluffr")
#' sqlf_rules()
#'
#' @export
sqlf_rules <- function() {
  sf <- get_sqlfluff()
  result <- sf$list_rules()
  py_result_to_df(result, c("code", "description"))
}
