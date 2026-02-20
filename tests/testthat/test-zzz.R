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
