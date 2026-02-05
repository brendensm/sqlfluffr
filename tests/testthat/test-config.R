test_that("sqlf_config writes a config file", {
  path <- tempfile(fileext = ".sqlfluff")
  on.exit(unlink(path))

  result <- sqlf_config(dialect = "postgres", path = path)
  expect_equal(result, path)
  expect_true(file.exists(path))

  content <- readLines(path)
  expect_true("[sqlfluff]" %in% content)
  expect_true("dialect = postgres" %in% content)
})

test_that("sqlf_config writes all settings", {
  path <- tempfile(fileext = ".sqlfluff")
  on.exit(unlink(path))

  sqlf_config(
    dialect = "bigquery",
    rules = c("LT01", "LT02"),
    exclude_rules = "AM01",
    max_line_length = 80,
    path = path
  )

  content <- readLines(path)
  expect_true("dialect = bigquery" %in% content)
  expect_true("rules = LT01,LT02" %in% content)
  expect_true("exclude_rules = AM01" %in% content)
  expect_true("max_line_length = 80" %in% content)
})

test_that("sqlf_config writes glue setting", {
  path <- tempfile(fileext = ".sqlfluff")
  on.exit(unlink(path))

  sqlf_config(dialect = "ansi", glue = TRUE, path = path)

  content <- readLines(path)
  expect_true("[sqlfluffr]" %in% content)
  expect_true("glue = true" %in% content)
})

test_that("sqlf_config writes extra settings", {
  path <- tempfile(fileext = ".sqlfluff")
  on.exit(unlink(path))

  sqlf_config(dialect = "ansi", indent_unit = "space", path = path)

  content <- readLines(path)
  expect_true("indent_unit = space" %in% content)
})

test_that("sqlf_config refuses to overwrite by default", {
  path <- tempfile(fileext = ".sqlfluff")
  on.exit(unlink(path))
  writeLines("existing", path)

  expect_error(sqlf_config(dialect = "ansi", path = path), "already exists")
})

test_that("sqlf_config overwrites when asked", {
  path <- tempfile(fileext = ".sqlfluff")
  on.exit(unlink(path))
  writeLines("existing", path)

  sqlf_config(dialect = "ansi", path = path, overwrite = TRUE)
  content <- readLines(path)
  expect_true("dialect = ansi" %in% content)
})

test_that("sqlf_config_edit errors when no config exists", {
  expect_error(sqlf_config_edit(path = tempfile()), "No config file found")
})

test_that("new_sqlf_config returns correct S3 class", {
  cfg <- sqlfluffr:::new_sqlf_config(dialect = "ansi")
  expect_s3_class(cfg, "sqlf_config")
})

test_that("new_sqlf_config stores all fields", {
  cfg <- sqlfluffr:::new_sqlf_config(
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
