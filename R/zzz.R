.sqlfluff_env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  sf <- try(quietly({
    reticulate::py_require("sqlfluff")
    reticulate::import("sqlfluff")
  }), silent = TRUE)
  if (!inherits(sf, "try-error")) {
    .sqlfluff_env$sqlfluff <- sf
  }
}

#' @noRd
quietly <- function(expr) {
  suppressWarnings(suppressMessages(force(expr)))
}

#' @noRd
is_interactive <- function() interactive()

#' @noRd
prompt_install <- function() {
  if (!is_interactive()) {
    stop(
      "The Python package 'sqlfluff' is not installed.\n",
      "Run sqlfluffr in an interactive session to install.",
      call. = FALSE
    )
  }

  message("The Python package 'sqlfluff' is required.")
  message("This may download Python and dependencies (~50 MB).")

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
    sf <- try(quietly({
      reticulate::py_require("sqlfluff")
      reticulate::import("sqlfluff")
    }), silent = TRUE)
    if (inherits(sf, "try-error")) {
      prompt_install()
      sf <- quietly(reticulate::import("sqlfluff"))
    }
    .sqlfluff_env$sqlfluff <- sf
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
