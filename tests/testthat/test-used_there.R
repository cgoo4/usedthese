test_that("Resource unavailable", {
  local_mocked_bindings(
    read_html = function(x) stop("cannot open the connection")
  )
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
  html <- rvest::read_html('
    <html><body>
      <a class="quarto-grid-link" href="/project/planning/"></a>
      <a class="quarto-default-link" href="/project/sets/"></a>
      <a class="quarto-table-link" href="/project/jitter/"></a>
    </body></html>')
  expect_snapshot(
    get_links(html, "https://www.quantumjitter.com/project/", 5)
  )
})
