skip_if_no_sqlfluff()

test_that("parsing SELECT 1 returns a non-null result", {
  result <- sqlf_parse(sql = "SELECT 1")
  expect_false(is.null(result))
})
