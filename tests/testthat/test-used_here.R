library(dplyr)
library(tibble)

test_that("used_here('library(tibble)\nlibrary(dplyr)\ntibble(x = c(1, 2, 3)) |>\nsummarise(x = sum(x))') counts function usage", {
  expect_equal(
    used_here("library(tibble)\nlibrary(dplyr)\ntibble(x = c(1, 2, 3)) |>\nsummarise(x = sum(x))"),
    tibble::tibble(
      Package = c("base", "dplyr", "tibble"),
      Function = c(
        "c[1];  library[2];  sum[1]",
        "summarise[1]", "tibble[1]"
      )
    ) |>
      knitr::kable(format = "html", table.attr = "class = 'usethese'") |>
      kableExtra::kable_styling("striped")
  )
})
