test_that("extract_glue_vars extracts {var}", {
  result <- sqlfluffr:::extract_glue_vars("SELECT {col} FROM {tbl}")
  expect_equal(sort(result), c("col", "tbl"))
})

test_that("extract_glue_vars extracts {`var`}", {
  result <- sqlfluffr:::extract_glue_vars("SELECT {`col`} FROM t")
  expect_equal(result, "col")
})

test_that("extract_glue_vars extracts {'var'}", {
  result <- sqlfluffr:::extract_glue_vars("SELECT {'col'} FROM t")
  expect_equal(result, "col")
})

test_that("extract_glue_vars extracts {*var}", {
  result <- sqlfluffr:::extract_glue_vars("SELECT {*col} FROM t")
  expect_equal(result, "col")
})

test_that("extract_glue_vars extracts {.var}", {
  result <- sqlfluffr:::extract_glue_vars("SELECT {.col} FROM t")
  expect_equal(result, "col")
})

test_that("extract_glue_vars extracts {var.name}", {
  result <- sqlfluffr:::extract_glue_vars("SELECT {my.col} FROM t")
  expect_equal(result, "my.col")
})

test_that("extract_glue_vars extracts {data$col*}", {
  result <- sqlfluffr:::extract_glue_vars("SELECT {data$values*} FROM t")
  expect_equal(result, "data$values")
})

test_that("extract_glue_vars deduplicates", {
  result <- sqlfluffr:::extract_glue_vars("SELECT {col}, {col} FROM t")
  expect_equal(result, "col")
})

test_that("extract_glue_vars returns character(0) on no matches", {
  result <- sqlfluffr:::extract_glue_vars("SELECT 1 FROM t")
  expect_identical(result, character(0))
})

test_that("build_glue_config_str contains required sections", {
  cfg <- sqlfluffr:::build_glue_config_str(vars = c("col", "tbl"))
  expect_match(cfg, "\\[sqlfluff\\]")
  expect_match(cfg, "templater = placeholder")
  expect_match(cfg, "param_regex")
  expect_match(cfg, "col = '__GLUE_1__'")
  expect_match(cfg, "tbl = '__GLUE_2__'")
})

test_that("build_glue_config_str includes dialect when provided", {
  cfg <- sqlfluffr:::build_glue_config_str(
    vars = c("col"),
    dialect = "postgres"
  )
  expect_match(cfg, "dialect = postgres")
})

test_that("build_glue_config_str merges user_config", {
  cfg <- sqlfluffr:::build_glue_config_str(
    vars = c("col"),
    user_config = list(max_line_length = "120", rules = "L001,L002")
  )
  expect_match(cfg, "max_line_length = 120")
  expect_match(cfg, "rules = L001,L002")
})
