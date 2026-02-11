describe("sqlf_reset()", {
  it("removes the marker file when it exists", {
    dir <- withr::local_tempdir()
    local_mocked_bindings(
      marker_path = function() file.path(dir, "installed"),
      .package = "sqlfluffr"
    )
    file.create(file.path(dir, "installed"))

    expect_message(sqlf_reset(), "reset")
    expect_false(file.exists(file.path(dir, "installed")))
  })

  it("prints 'nothing to reset' when no marker exists", {
    dir <- withr::local_tempdir()
    local_mocked_bindings(
      marker_path = function() file.path(dir, "installed"),
      .package = "sqlfluffr"
    )

    expect_message(sqlf_reset(), "Nothing to reset")
  })
})

describe("prompt_install()", {
  it("errors in non-interactive sessions", {
    local_mocked_bindings(
      is_interactive = function() FALSE,
      .package = "sqlfluffr"
    )
    expect_error(
      sqlfluffr:::prompt_install(),
      "interactive session"
    )
  })

  it("errors when user declines installation", {
    local_mocked_bindings(
      is_interactive = function() TRUE,
      .package = "sqlfluffr"
    )
    local_mocked_bindings(
      menu = function(...) 2L,
      .package = "utils"
    )
    expect_error(
      sqlfluffr:::prompt_install(),
      "cancelled"
    )
  })
})
