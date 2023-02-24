test_that("First 3 lines of the first link", {
  expect_equal(
    used_there("https://quantumjitter.com/project", 1) |> head(3),
    tibble(
      Package = c("base", "base", "base"),
      Function = c("as.Date", "as.numeric", "c"),
      n = c(1, 1, 2),
      url = c(
        "https://quantumjitter.com/project/jitter/",
        "https://quantumjitter.com/project/jitter/",
        "https://quantumjitter.com/project/jitter/"
      )
    )
  )
})
