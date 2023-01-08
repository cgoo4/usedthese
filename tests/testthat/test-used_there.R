test_that("first 3 lines", {
  expect_equal(
    used_there("https://quantumjitter.com/project") |> head(3),
    tibble(
      Package = c("DT", "R.utils", "RColorBrewer"),
      Function = c("datatable", "gunzip", "brewer.pal"),
      url = c(
        "https://quantumjitter.com/project/planning/",
        "https://quantumjitter.com/project/stories/",
        "https://quantumjitter.com/project/bands/"
      ),
      n = c(1, 1, 1)
    )
  )
})
