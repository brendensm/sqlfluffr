.sqlfluff_env <- new.env(parent = emptyenv())

#' @noRd
marker_path <- function() {
  file.path(tools::R_user_dir("sqlfluffr", "cache"), "installed")
}

.onLoad <- function(libname, pkgname) {
  if (file.exists(marker_path())) {
    quietly(reticulate::py_require("sqlfluff"))
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
prompt_install <- function() {
  if (!interactive()) {
    stop(
      "The Python package 'sqlfluff' is not installed.\n",
      "Run sqlfluffr in an interactive session to install.",
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

  # Mark as installed so future sessions skip the prompt
  dir.create(dirname(marker_path()), recursive = TRUE, showWarnings = FALSE)
  file.create(marker_path())
}

#' Reset sqlfluffr setup
#'
#' Removes the cached marker file so that the next call to any sqlfluffr
#' function will re-prompt for Python and sqlfluff installation. Use this
#' if your Python environment has changed or something is not working.
#'
#' @returns Invisible `NULL`.
#'
#' @examples
#' \dontrun{
#' sqlf_reset()
#' }
#'
#' @export
sqlf_reset <- function() {
  mp <- marker_path()
  if (file.exists(mp)) {
    unlink(mp)
    message("sqlfluffr setup has been reset. You will be prompted to install on next use.")
  } else {
    message("Nothing to reset.")
  }
  invisible(NULL)
}

#' @noRd
get_sqlfluff <- function() {
  if (is.null(.sqlfluff_env$sqlfluff)) {
    if (!file.exists(marker_path())) {
      prompt_install()
    }

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
