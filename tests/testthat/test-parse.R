skip_on_cran()
skip_if_not(
  reticulate::py_module_available("sqlfluff"),
  "Python sqlfluff not available"
)

test_that("parsing SELECT 1 returns a non-null result", {
  result <- sqlf_parse(sql = "SELECT 1")
  expect_false(is.null(result))
})
