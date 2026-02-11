describe("extract_glue_vars()", {
  it("extracts {var} syntax", {
    result <- sqlfluffr:::extract_glue_vars("SELECT {col} FROM {tbl}")
    expect_equal(sort(result), c("col", "tbl"))
  })

  it("extracts {`var`} backtick syntax", {
    result <- sqlfluffr:::extract_glue_vars("SELECT {`col`} FROM t")
    expect_equal(result, "col")
  })

  it("extracts {'var'} quote syntax", {
    result <- sqlfluffr:::extract_glue_vars("SELECT {'col'} FROM t")
    expect_equal(result, "col")
  })

  it("extracts {*var} splice syntax", {
    result <- sqlfluffr:::extract_glue_vars("SELECT {*col} FROM t")
    expect_equal(result, "col")
  })

  it("extracts {.var} dot-prefix syntax", {
    result <- sqlfluffr:::extract_glue_vars("SELECT {.col} FROM t")
    expect_equal(result, "col")
  })

  it("extracts {var.name} with dots in name", {
    result <- sqlfluffr:::extract_glue_vars("SELECT {my.col} FROM t")
    expect_equal(result, "my.col")
  })

  it("extracts {data$col*} dollar syntax", {
    result <- sqlfluffr:::extract_glue_vars("SELECT {data$values*} FROM t")
    expect_equal(result, "data$values")
  })

  it("deduplicates repeated variables", {
    result <- sqlfluffr:::extract_glue_vars("SELECT {col}, {col} FROM t")
    expect_equal(result, "col")
  })

  it("returns character(0) when no placeholders found", {
    result <- sqlfluffr:::extract_glue_vars("SELECT 1 FROM t")
    expect_identical(result, character(0))
  })
})

describe("build_glue_config_str()", {
  it("contains required sections and templater setting", {
    cfg <- sqlfluffr:::build_glue_config_str(vars = c("col", "tbl"))
    expect_match(cfg, "\\[sqlfluff\\]")
    expect_match(cfg, "templater = placeholder")
    expect_match(cfg, "param_regex")
    expect_match(cfg, "col = '__GLUE_1__'")
    expect_match(cfg, "tbl = '__GLUE_2__'")
  })

  it("includes dialect when provided", {
    cfg <- sqlfluffr:::build_glue_config_str(
      vars = c("col"),
      dialect = "postgres"
    )
    expect_match(cfg, "dialect = postgres")
  })

  it("merges user_config settings", {
    cfg <- sqlfluffr:::build_glue_config_str(
      vars = c("col"),
      user_config = list(max_line_length = "120", rules = "L001,L002")
    )
    expect_match(cfg, "max_line_length = 120")
    expect_match(cfg, "rules = L001,L002")
  })

  it("does not duplicate dialect from user_config", {
    cfg <- sqlfluffr:::build_glue_config_str(
      vars = c("col"),
      dialect = "ansi",
      user_config = list(dialect = "postgres")
    )
    matches <- gregexpr("dialect", cfg)[[1]]
    expect_equal(length(matches), 1L)
  })
})
