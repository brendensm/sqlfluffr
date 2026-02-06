## R CMD check results

0 errors | 0 warnings | 0 notes

## Package notes

This package wraps the Python 'sqlfluff' SQL linter/formatter via
'reticulate'. Python and 'sqlfluff' are provisioned automatically on
first use with explicit user consent (interactive prompt).

- All examples use `\dontrun{}` because they require the external Python
  package 'sqlfluff', which cannot be assumed present on CRAN machines.
- Tests that require Python use `skip_on_cran()`. Pure R tests
  (input validation, config file writing, glue variable extraction)
  run on CRAN normally.
- The package loads and passes all checks without Python installed.
