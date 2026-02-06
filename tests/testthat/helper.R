skip_if_no_sqlfluff <- function() {
  skip_on_cran()
  skip_if_not(
    reticulate::py_module_available("sqlfluff"),
    "Python package 'sqlfluff' not available"
  )
}
