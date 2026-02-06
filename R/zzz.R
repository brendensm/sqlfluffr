.sqlfluff_env <- new.env(parent = emptyenv())

# Virtualenv name used by sqlfluffr
SQLFLUFFR_VENV <- "r-sqlfluffr"

#' @noRd
quietly <- function(expr) {
  old_warn <- getOption("warn")
  options(warn = -1)
 on.exit(options(warn = old_warn))
  suppressWarnings(suppressMessages(force(expr)))
}

#' @noRd
sqlfluffr_venv_exists <- function() {
  reticulate::virtualenv_exists(SQLFLUFFR_VENV)
}

#' @noRd
sqlfluff_is_installed <- function() {
  if (!sqlfluffr_venv_exists()) return(FALSE)
  pkgs <- reticulate::py_list_packages(SQLFLUFFR_VENV)
  "sqlfluff" %in% pkgs$package
}

#' Install sqlfluff
#'
#' Installs Python and the sqlfluff package into a dedicated virtual
#' environment. This only needs to be run once per machine.
#'
#' @param force If `TRUE`, reinstall even if sqlfluff is already present.
#'
#' @return Invisible `NULL`.
#'
#' @examples
#' \dontrun{
#' sqlf_install()
#' }
#'
#' @export
sqlf_install <- function(force = FALSE) {
  if (sqlfluff_is_installed() && !force) {
    message("sqlfluff is already installed. Use `force = TRUE` to reinstall.")
    return(invisible(NULL))

  }

  message("Installing sqlfluff...")
  message("This may download Python and dependencies (~50 MB).")

 # Create virtualenv if needed (this may install Python first)
  if (!sqlfluffr_venv_exists()) {
    tryCatch({
      reticulate::virtualenv_create(SQLFLUFFR_VENV)
    }, error = function(e) {
      # No Python found - install it
      message("No Python installation found. Installing Python...")
      python <- reticulate::install_python()
      reticulate::virtualenv_create(SQLFLUFFR_VENV, python = python)
    })
  }

  reticulate::py_install("sqlfluff", envname = SQLFLUFFR_VENV)
  message("sqlfluff installed successfully.")
  invisible(NULL)
}

#' Uninstall sqlfluff
#'
#' Removes the virtual environment created by [sqlf_install()].
#'
#' @param confirm If `TRUE` (default in interactive sessions), ask for
#'   confirmation before removing.
#'
#' @return Invisible `NULL`.
#'
#' @export
sqlf_uninstall <- function(confirm = interactive()) {
  if (!sqlfluffr_venv_exists()) {
    message("No sqlfluff installation found.")
    return(invisible(NULL))
 }

  reticulate::virtualenv_remove(SQLFLUFFR_VENV, confirm = confirm)
  message("sqlfluff uninstalled.")
  invisible(NULL)
}

#' @noRd
prompt_install <- function() {
  if (!interactive()) {
    stop(
      "sqlfluff is not installed.\n",
      "Run `sqlf_install()` to set it up.",
      call. = FALSE
    )
  }

  message("sqlfluff is not installed.")
  message("This may download Python and dependencies (~50 MB).")

  answer <- utils::menu(
    choices = c("Yes", "No"),
    title = "Would you like to install it now?"
  )

  if (answer != 1L) {
    stop("Run `sqlf_install()` when you're ready.", call. = FALSE)
  }

  sqlf_install()
}

#' @noRd
get_sqlfluff <- function() {
  if (is.null(.sqlfluff_env$sqlfluff)) {
    # Check if installed WITHOUT triggering any downloads
    if (!sqlfluff_is_installed()) {
      prompt_install()
    }

    message("Starting sqlfluff (one-time per session)...")
    reticulate::use_virtualenv(SQLFLUFFR_VENV, required = TRUE)
    sf <- quietly(reticulate::import("sqlfluff"))
    .sqlfluff_env$sqlfluff <- sf
  }
  .sqlfluff_env$sqlfluff
}

#' @noRd
get_sqlfluff_core <- function() {
  if (is.null(.sqlfluff_env$core)) {
    get_sqlfluff()  # Ensure sqlfluff is installed first
    .sqlfluff_env$core <- quietly(reticulate::import("sqlfluff.core"))
  }
  .sqlfluff_env$core
}
