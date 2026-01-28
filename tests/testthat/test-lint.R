skip_if_not(
  reticulate::py_module_available("sqlfluff"),
  "Python sqlfluff not available"
)

test_that("linting bad SQL returns data.frame with expected columns", {
  result <- sqlf_lint(sql = "SeLeCt 1")
  expect_s3_class(result, "data.frame")
  expect_s3_class(result, "sqlf_lint_results")
  expect_gt(nrow(result), 0L)
  expect_true(all(c("line_no", "line_pos", "code", "description", "name") %in%
    names(result)))
})

test_that("linting clean SQL returns zero-row data.frame", {
  result <- sqlf_lint(sql = "SELECT 1\n")
  expect_s3_class(result, "sqlf_lint_results")
  expect_equal(nrow(result), 0L)
})

test_that("linting with glue = TRUE on placeholders does not error", {
  expect_no_error(
    sqlf_lint(
      sql = "SELECT {col} FROM {tbl}\n",
      glue = TRUE,
      dialect = "ansi"
    )
  )
})

test_that("file-based linting works", {
  tmp <- tempfile(fileext = ".sql")
  on.exit(unlink(tmp))
  writeLines("SeLeCt 1", tmp)
  result <- sqlf_lint(file = tmp)
  expect_s3_class(result, "sqlf_lint_results")
  expect_gt(nrow(result), 0L)
})

test_that("print method for zero violations", {
  result <- sqlf_lint(sql = "SELECT 1\n")
  out <- capture.output(print(result))
  expect_true(any(grepl("No linting violations found", out)))
})

test_that("print method for violations", {
  result <- sqlf_lint(sql = "SeLeCt 1")
  out <- capture.output(print(result))
  expect_true(any(grepl("sqlf_lint_results: \\d+ violation", out)))
})
