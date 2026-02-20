# sqlfluffr

<!-- badges: start -->
  [![R-CMD-check](https://github.com/brendensm/sqlfluffr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/brendensm/sqlfluffr/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->

**sqlfluffr** is an R wrapper around [SQLFluff](https://github.com/sqlfluff/sqlfluff), the popular SQL linter and formatter. Lint, fix, and parse SQL directly from R with support for multiple dialects and [`glue::glue_sql()`](https://glue.tidyverse.org/reference/glue_sql.html) syntax.

## Installation

```r
remotes::install_github("brendensm/sqlfluffr")
```

Then run the one-time Python setup:

```r
library(sqlfluffr)
sqlf_install()
```

This creates a dedicated virtual environment and installs Python and sqlfluff via [reticulate](https://rstudio.github.io/reticulate/). You only need to run `sqlf_install()` once.

## Basic usage

### Lint

Check SQL for style violations:

```r
sqlf_lint(sql = "SELECT  a,b from t where x=1\n")
#> # sqlf_lint_results: 4 violations
#>   line_no line_pos code description
#>   <int>   <int>    <chr> <chr>
#> 1 1       8        LT01  Expected single whitespace...
#> 2 1       10       LT04  Keywords must be consistently...
#> ...
```

### Fix

Auto-fix style issues:

```r
sqlf_fix(sql = "SELECT  a,b from t where x=1\n")
#> SELECT
#>     a,
#>     b
#> FROM t
#> WHERE x = 1
```

### Parse

Get the syntax tree:

```r
sqlf_parse(sql = "SELECT 1\n")
```

### Lint or fix a file

All functions accept a `file` argument:

```r
sqlf_lint(file = "query.sql")
sqlf_fix(file = "query.sql", overwrite = TRUE)  # overwrites the file
```

## Dialects

Specify a SQL dialect with the `dialect` argument:

```r
sqlf_lint(sql = "SELECT TOP 10 * FROM t\n", dialect = "tsql")
```

See all available dialects:

```r
sqlf_dialects()
```

## Project configuration

Instead of passing `dialect` and other options on every call, write a `.sqlfluff` config file for the project:

```r
sqlf_config(dialect = "postgres", max_line_length = 120)
```

All subsequent `sqlf_lint()`, `sqlf_fix()`, and `sqlf_parse()` calls in that directory will use those settings automatically.

You can replace an existing config file with the argument `overwrite`, or edit the config file manually:

```r
sqlf_config_edit()
```

## `glue_sql()` support

SQL containing `{var}` placeholders from `glue::glue_sql()` would normally cause parsing errors. Pass `glue = TRUE` to handle them:

```r
sql <- "SELECT {`col`} FROM {`tbl`} WHERE id = {id}\n"

sqlf_lint(sql = sql, glue = TRUE)
sqlf_fix(sql = sql, glue = TRUE)
```

To enable glue support project-wide, include it in the config:

```r
sqlf_config(dialect = "postgres", glue = TRUE)
```

## Exploring rules

List all available linting rules:

```r
sqlf_rules()
```

Apply or exclude specific rules:

```r
sqlf_lint(sql = "SELECT a FROM t\n", rules = c("LT01", "LT02"))
sqlf_lint(sql = "SELECT a FROM t\n", exclude_rules = "AM01")
```
