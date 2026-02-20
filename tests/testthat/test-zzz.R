describe("ensure_sqlfluff()", {
  it("errors when virtualenv does not exist", {
    local_mocked_bindings(
      virtualenv_exists = function(...) FALSE,
      .package = "reticulate"
    )
    expect_error(
      sqlfluffr:::ensure_sqlfluff(),
      "sqlf_install"
    )
  })
})
