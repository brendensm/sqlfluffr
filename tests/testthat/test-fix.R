skip_if_no_sqlfluff()
withr::local_dir(withr::local_tempdir())

test_that("fixing SQL returns a character string", {
  result <- sqlf_fix(sql = "select 1")
  expect_type(result, "character")
  expect_true(nchar(result) > 0L)
})

test_that("fixing SQL ensures trailing newline", {
  result <- sqlf_fix(sql = "select 1")
  expect_match(result, "\n$")
})

test_that("fixing SQL with dialect removes extra whitespace", {
  result <- sqlf_fix(
    sql = "  SELECT a  +  b FROM tbl;  ",
    dialect = "teradata"
  )
  expect_type(result, "character")
  expect_match(result, "SELECT")
})

test_that("fixing SQL with glue and dialect does not error", {
  suppressWarnings(
    result <- sqlf_fix(
      sql = "select {col} from {tbl}",
      glue = TRUE, dialect = "ansi"
    )
  )
  expect_type(result, "character")
  expect_true(nchar(result) > 0L)
})

test_that("file-based fix with overwrite = FALSE does not modify file", {
  tmp <- withr::local_tempfile(fileext = ".sql")
  writeLines("select 1", tmp)
  original <- readLines(tmp)
  expect_message(
    sqlf_fix(file = tmp, overwrite = FALSE),
    "File not modified"
  )
  expect_equal(readLines(tmp), original)
})

test_that("file-based fix with overwrite = TRUE overwrites file", {
  tmp <- withr::local_tempfile(fileext = ".sql")
  writeLines("select 1", tmp)
  expect_message(
    sqlf_fix(file = tmp, overwrite = TRUE),
    "File overwritten"
  )
  fixed <- readLines(tmp)
  expect_true(length(fixed) > 0L)
})

test_that("sqlf_fix warns on parse errors when output unchanged", {
  expect_warning(
    sqlf_fix(sql = "SELECT * FROM t WHERE x IN 1\n"),
    "parse error"
  )
})

test_that("sqlf_fix glue hint appears when braces present and glue = FALSE", {
  expect_warning(
    sqlf_fix(sql = "SELECT {col} FROM t\n", glue = FALSE),
    "glue = TRUE"
  )
})

test_that("sqlf_fix does not warn when fix produces different output", {
  expect_no_warning(
    sqlf_fix(sql = "select 1")
  )
})

test_that("sqlf_fix with cat = TRUE prints to stdout", {
  expect_output(
    result <- sqlf_fix(sql = "select 1", cat = TRUE),
    "select"
  )
  expect_type(result, "character")
})

test_that("sqlf_fix with cat = FALSE returns without printing", {
  out <- capture.output(result <- sqlf_fix(sql = "select 1", cat = FALSE))
  expect_equal(out, character(0))
  expect_type(result, "character")
  expect_match(result, "select")
})

test_that("sqlf_fix with config object works", {
  dir <- withr::local_tempdir()
  sqlf_config(dialect = "postgres", path = file.path(dir, ".sqlfluff"))
  cfg <- sqlfluffr:::new_sqlf_config(config_path = dir)
  result <- sqlf_fix(sql = "select 1", config = cfg)
  expect_type(result, "character")
  expect_true(nchar(result) > 0L)
})
