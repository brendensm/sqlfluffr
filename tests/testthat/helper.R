skip_if_no_sqlfluff <- function() {
  skip_on_cran()
  skip_if_not(
    reticulate::virtualenv_exists("r-sqlfluffr"),
    "sqlfluff virtualenv not available"
  )
}
