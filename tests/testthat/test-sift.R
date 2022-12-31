library(dplyr)
library(tibble)

test_that("sift('example.R') counts function usage", {
  expect_equal(sift("example.R"), tibble::tibble(
    Package = c("base", "dplyr", "tibble"),
    Function = c(
      "c[1];  library[2];  sum[1]",
      "summarise[1]", "tibble[1]"
    )
  ))
})
