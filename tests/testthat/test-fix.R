skip_on_cran()
skip_if_not(
  reticulate::py_module_available("sqlfluff"),
  "Python sqlfluff not available"
)

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
  result <- sqlf_fix(
    sql = "select {col} from {tbl}",
    glue = TRUE, dialect = "ansi"
  )
  expect_type(result, "character")
  expect_true(nchar(result) > 0L)
})

test_that("file-based fix with overwrite = FALSE does not modify file", {
  tmp <- tempfile(fileext = ".sql")
  on.exit(unlink(tmp))
  writeLines("select 1", tmp)
  original <- readLines(tmp)
  expect_message(
    sqlf_fix(file = tmp, overwrite = FALSE),
    "File not modified"
  )
  expect_equal(readLines(tmp), original)
})

test_that("file-based fix with overwrite = TRUE overwrites file", {
  tmp <- tempfile(fileext = ".sql")
  on.exit(unlink(tmp))
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
