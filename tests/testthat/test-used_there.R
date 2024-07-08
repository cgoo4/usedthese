test_that("Resource unavailable", {
  expect_snapshot(
    used_there("https://quantumjitter.com/project2", 1),
    error = TRUE
  )
  expect_snapshot(
    used_there("https://quantumjitter.com/project", 1:3),
    error = TRUE
  )
})

test_that("First 3 lines of the first link", {
  local_mocked_bindings(
    used_there = extract_usage
  )
  expect_equal(
    used_there("jitter.html") |> head(3),
    tibble(
      Package = c("base", "base", "base"),
      Function = c("as.Date", "as.numeric", "c"),
      n = c(1, 1, 2),
      url = c(
        "jitter.html",
        "jitter.html",
        "jitter.html"
      )
    )
  )
})

test_that("Get links", {
  html <- rvest::read_html("https://www.quantumjitter.com/project/")
  expect_snapshot(
    get_links(html, "https://www.quantumjitter.com/project/", 5)
  )
})
