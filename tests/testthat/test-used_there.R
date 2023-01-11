test_that("First 3 lines of the first link", {
  expect_equal(
    used_there("https://quantumjitter.com/project", 1) |> head(3),
    tibble(
      Package = c("base", "base", "base"),
      Function = c("as.character", "as.integer", "c"),
      n = c(1, 1, 8),
      url = c(
        "/project/footnote/",
        "/project/footnote/",
        "/project/footnote/"
      )
    )
  )
})
