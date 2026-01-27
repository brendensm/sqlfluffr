test_that("sqlf_config returns correct S3 class", {
  cfg <- sqlf_config(dialect = "ansi")
  expect_s3_class(cfg, "sqlf_config")
})

test_that("sqlf_config stores all fields", {
  cfg <- sqlf_config(
    dialect = "postgres",
    rules = c("LT01", "LT02"),
    exclude_rules = c("AM01"),
    max_line_length = 120,
    config_path = "/tmp/test.sqlfluff"
  )
  expect_equal(cfg$dialect, "postgres")
  expect_equal(cfg$rules, c("LT01", "LT02"))
  expect_equal(cfg$exclude_rules, "AM01")
  expect_equal(cfg$max_line_length, 120)
  expect_equal(cfg$config_path, "/tmp/test.sqlfluff")
})

test_that("sqlf_config stores extras via ...", {
  cfg <- sqlf_config(dialect = "ansi", indent_unit = "space")
  expect_equal(cfg$extra$indent_unit, "space")
})

test_that("print.sqlf_config outputs expected text", {
  cfg <- sqlf_config(
    dialect = "bigquery",
    rules = c("LT01", "LT02"),
    exclude_rules = "AM01",
    max_line_length = 80,
    config_path = "/tmp/.sqlfluff",
    indent_unit = "space"
  )
  out <- capture.output(print(cfg))
  expect_true(any(grepl("<sqlf_config>", out)))
  expect_true(any(grepl("dialect: bigquery", out)))
  expect_true(any(grepl("rules: LT01, LT02", out)))
  expect_true(any(grepl("exclude_rules: AM01", out)))
  expect_true(any(grepl("max_line_length: 80", out)))
  expect_true(any(grepl("config_path:", out)))
  expect_true(any(grepl("extra: indent_unit", out)))
})
