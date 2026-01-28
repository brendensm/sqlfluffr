.sqlfluff_env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  # Initialize Python and import sqlfluff at package load
  # This triggers any reticulate noise once, not on every function call
  sf <- try_import_sqlfluff()
  if (!is.null(sf)) {
    .sqlfluff_env$sqlfluff <- sf
  }
}

#' @noRd
quietly <- function(expr) {
  old_warn <- getOption("warn")
  options(warn = -1)
  on.exit(options(warn = old_warn))
  suppressWarnings(suppressMessages(force(expr)))
}

#' @noRd
try_import_sqlfluff <- function() {
  quietly(tryCatch(
    reticulate::import("sqlfluff"),
    error = function(e) NULL
  ))
}

#' @noRd
prompt_install <- function() {
  if (!interactive()) {
    stop(
      "The Python package 'sqlfluff' is not installed.\n",
      "Run sqlfluffr interactively to install, or use reticulate::py_require('sqlfluff').",
      call. = FALSE
    )
  }

  message("The Python package 'sqlfluff' is required.")
  message("This may download Python and dependencies (~25 MB).")

  answer <- utils::menu(
    choices = c("Yes", "No"),
    title = "Would you like to install it?"
  )

  if (answer != 1L) {
    stop("sqlfluff installation cancelled.", call. = FALSE)
  }

  quietly(reticulate::py_require("sqlfluff"))
}

#' @noRd
get_sqlfluff <- function() {
  if (is.null(.sqlfluff_env$sqlfluff)) {
    # Try to import - if already installed, this just works
    sf <- try_import_sqlfluff()

    if (is.null(sf)) {
      # Not installed - prompt and install
      prompt_install()
      sf <- try_import_sqlfluff()
      if (is.null(sf)) {
        stop("Failed to import sqlfluff after installation.", call. = FALSE)
      }
    }

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
