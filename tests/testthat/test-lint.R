skip_if_no_sqlfluff()
withr::local_dir(withr::local_tempdir())

test_that("linting bad SQL returns data.frame with expected columns", {
  result <- sqlf_lint(sql = "SeLeCt 1")
  expect_s3_class(result, "data.frame")
  expect_s3_class(result, "sqlf_lint_results")
  expect_gt(nrow(result), 0L)
  expect_contains(
    names(result),
    c("line_no", "line_pos", "code", "description", "name")
  )
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
  tmp <- withr::local_tempfile(fileext = ".sql")
  writeLines("SeLeCt 1", tmp)
  result <- sqlf_lint(file = tmp)
  expect_s3_class(result, "sqlf_lint_results")
  expect_gt(nrow(result), 0L)
})

test_that("print method for zero violations", {
  result <- sqlf_lint(sql = "SELECT 1\n")
  expect_snapshot(print(result))
})

test_that("print method for violations", {
  result <- sqlf_lint(sql = "SeLeCt 1")
  expect_snapshot(print(result))
})
