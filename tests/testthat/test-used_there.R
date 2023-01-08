test_that("first 3 lines", {
  expect_equal(used_there("https://quantumjitter.com/project") |> head(3),
               tibble(package = c("DT", "R.utils", "RColorBrewer"),
                      functn = c("datatable", "gunzip", "brewer.pal"),
                      n_url = c(20, 20, 20),
                      n = c(1, 1, 1)))
})
