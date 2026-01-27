#' @noRd
extract_glue_vars <- function(sql) {
  m <- gregexpr("\\{[`'\"*.]*([\\w_.]+)[`'\"*.]*\\}", sql, perl = TRUE)
  matches <- regmatches(sql, m)[[1L]]
  if (length(matches) == 0L) return(character(0))

  # Extract the inner variable name
  inner <- sub("^\\{[`'\"*.]*([\\w_.]+)[`'\"*.]*\\}$", "\\1", matches,
               perl = TRUE)
  unique(inner)
}

#' @noRd
build_glue_config_str <- function(vars, dialect = NULL, user_config = NULL) {
  lines <- character()
  lines <- c(lines, "[sqlfluff]")
  lines <- c(lines, "templater = placeholder")


  if (!is.null(dialect)) {
    lines <- c(lines, paste0("dialect = ", dialect))
  }

  # Merge any user config core settings

  if (!is.null(user_config)) {
    for (nm in names(user_config)) {
      if (!nm %in% c("dialect", "templater")) {
        lines <- c(lines, paste0(nm, " = ", user_config[[nm]]))
      }
    }
  }

  lines <- c(lines, "")
  lines <- c(lines, "[sqlfluff:templater:placeholder]")
  lines <- c(lines,
    "param_regex = \\{[`'\"*.]*(?P<param_name>[\\w_.]+)[`'\"*.]*\\}")

  for (i in seq_along(vars)) {
    lines <- c(lines, paste0(vars[i], " = __GLUE_", i, "__"))
  }

  paste(lines, collapse = "\n")
}
