skip_if_no_sqlfluff()
withr::local_dir(withr::local_tempdir())

test_that("parsing SELECT 1 returns a non-null result", {
  result <- sqlf_parse(sql = "SELECT 1")
  expect_false(is.null(result))
})

test_that("parsing with dialect works", {
  result <- sqlf_parse(sql = "SELECT TOP 10 * FROM t\n", dialect = "tsql")
  expect_false(is.null(result))
})

test_that("file-based parsing works", {
  tmp <- withr::local_tempfile(fileext = ".sql")
  writeLines("SELECT 1", tmp)
  result <- sqlf_parse(file = tmp)
  expect_false(is.null(result))
})

test_that("parsing with glue placeholders works", {
  result <- sqlf_parse(
    sql = "SELECT {col}\n",
    glue = TRUE,
    dialect = "ansi"
  )
  expect_false(is.null(result))
})

test_that("parsing with config object works", {
  dir <- withr::local_tempdir()
  sqlf_config(dialect = "ansi", path = file.path(dir, ".sqlfluff"))
  cfg <- sqlfluffr:::new_sqlf_config(config_path = dir)
  result <- sqlf_parse(sql = "SELECT 1\n", config = cfg)
  expect_false(is.null(result))
})
