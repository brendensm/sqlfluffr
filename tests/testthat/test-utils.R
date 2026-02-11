test_that("resolve_sql_input errors when both sql and file provided", {
  expect_error(
    sqlfluffr:::resolve_sql_input(sql = "SELECT 1", file = "foo.sql"),
    "not both"
  )
})

test_that("resolve_sql_input errors when neither sql nor file provided", {
  expect_error(
    sqlfluffr:::resolve_sql_input(sql = NULL, file = NULL),
    "Provide either"
  )
})

test_that("resolve_sql_input errors on missing file", {
  expect_error(
    sqlfluffr:::resolve_sql_input(sql = NULL, file = "nonexistent.sql"),
    "File not found"
  )
})

test_that("resolve_sql_input reads file correctly", {
  tmp <- withr::local_tempfile(fileext = ".sql")
  writeLines(c("SELECT 1", "FROM t"), tmp)
  result <- sqlfluffr:::resolve_sql_input(sql = NULL, file = tmp)
  expect_equal(result, "SELECT 1\nFROM t")
})

test_that("resolve_sql_input passes sql string through", {
  result <- sqlfluffr:::resolve_sql_input(sql = "SELECT 1", file = NULL)
  expect_equal(result, "SELECT 1")
})

test_that("py_result_to_df returns zero-row df with correct columns on empty list", {
  result <- sqlfluffr:::py_result_to_df(list(), c("a", "b", "c"))
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0L)
  expect_equal(names(result), c("a", "b", "c"))
})

test_that("py_result_to_df converts list-of-lists properly", {
  input <- list(
    list(a = 1, b = "x"),
    list(a = 2, b = "y")
  )
  result <- sqlfluffr:::py_result_to_df(input, c("a", "b"))
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2L)
  expect_equal(result$a, c(1, 2))
  expect_equal(result$b, c("x", "y"))
})

test_that("py_result_to_df converts NULL values to NA", {
  input <- list(
    list(a = 1, b = NULL),
    list(a = NULL, b = "y")
  )
  result <- sqlfluffr:::py_result_to_df(input, c("a", "b"))
  expect_true(is.na(result$a[2]))
  expect_true(is.na(result$b[1]))
})

# --- read_project_config() ---

test_that("read_project_config returns empty list when file missing", {
  withr::local_dir(withr::local_tempdir())
  result <- sqlfluffr:::read_project_config()
  expect_equal(result, list())
})

test_that("read_project_config reads settings and ignores other sections", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  writeLines(c(
    "[sqlfluff]",
    "dialect = postgres",
    "max_line_length = 120",
    "",
    "[sqlfluffr]",
    "glue = true",
    "",
    "[sqlfluff:rules:LT01]",
    "enabled = false"
  ), ".sqlfluff")

  result <- sqlfluffr:::read_project_config()
  expect_equal(result[["dialect"]], "postgres")
  expect_equal(result[["max_line_length"]], "120")
  expect_equal(result[["glue"]], "true")
  expect_null(result[["enabled"]])
})

# --- resolve_config() ---

test_that("resolve_config returns NULL when no settings provided", {
  withr::local_dir(withr::local_tempdir())
  result <- sqlfluffr:::resolve_config(
    dialect = NULL, rules = NULL, exclude_rules = NULL,
    config = NULL, glue = NULL, sql = "SELECT 1"
  )
  expect_null(result)
})

test_that("resolve_config errors on invalid config object", {
  expect_error(
    sqlfluffr:::resolve_config(
      dialect = NULL, rules = NULL, exclude_rules = NULL,
      config = list(dialect = "ansi"), glue = NULL, sql = "SELECT 1"
    ),
    "sqlf_config"
  )
})
