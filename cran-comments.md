## R CMD check results

0 errors | 0 warnings | 1 notes

* This is a new submission.

## Package notes

This package wraps the Python 'sqlfluff' SQL linter/formatter via
'reticulate'. Python and 'sqlfluff' are provisioned automatically on
first use with explicit user consent (interactive prompt).

- Examples for Python-dependent functions use `@examplesIf` to only run
  when 'sqlfluff' is available. Pure R functions have unconditional examples.
- Tests that require Python use `skip_on_cran()`. Pure R tests
  (input validation, config file writing, glue variable extraction)
  run on CRAN normally.
- The package loads and passes all checks without Python installed.

## Downstream dependencies

There are currently no downstream dependencies for this package.
