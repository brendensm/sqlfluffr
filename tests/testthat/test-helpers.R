skip_if_no_sqlfluff()

test_that("sqlf_dialects returns data.frame with expected columns and rows", {
  result <- sqlf_dialects()
  expect_s3_class(result, "data.frame")
  expect_contains(names(result), c("label", "name", "inherits_from"))
  expect_gt(nrow(result), 0L)
})

test_that("sqlf_rules returns data.frame with expected columns and rows", {
  result <- sqlf_rules()
  expect_s3_class(result, "data.frame")
  expect_contains(names(result), c("code", "description"))
  expect_gt(nrow(result), 0L)
})
