
test_that("File Error", {
  expect_error(
    used_here(),
    "If you are knitting the current document, i.e. you clicked the Render button, then leave fil unspecified. If you are running the code chunks, then ensure you library the packages first in a fresh R session and specify the saved filename.",
  )
})

# test_that("Quoted File", {
#   expect_equal(
#     usedthese::used_here("~/R Projects/usedthese/vignettes/usedthese.Rmd"),
#     tibble::tibble(
#       Package = c("base", "stats", "usedthese"),
#       Function = c("library[4], options[1]",
#                    "filter[1]",
#                    "used_here[1]")
#     ) |>
#       knitr::kable(format = "html", table.attr = "class = 'usedthese'") |>
#       kableExtra::kable_styling("striped")
#   )
# })

test_that("No Conflicts", {
  expect_equal(
    used_here("mean(c(1, 2, 3))\nsum(c(1, 2, 3))"),
    tibble::tibble(
      Package = c("base"),
      Function = c("c[2], mean[1], sum[1]")
    ) |>
      knitr::kable(format = "html", table.attr = "class = 'usedthese'") |>
      kableExtra::kable_styling("striped")
  )
})

library(tidyverse)
library(xts)

test_that("Conflicts", {
  expect_equal(
    used_here("library(tidyverse)\nlibrary(xts, exclude = 'first')\ntribble(~group, ~a1, ~a2, ~b1,\n'x', 1, 2, 3,\n'x', 4, 5, 6,\n'y', 7, 8, 9) |>\nselect(-starts_with('b')) |>\nfilter(group == 'x') |>\nmutate(first_a1 = first(a1), last_a2 = last(a2))"),
    tibble::tibble(
      Package = c("base", "dplyr", "dplyr, stats", "tibble", "tidyselect", "xts, dplyr"),
      Function = c(
        "library[2]",
        "mutate[1], select[1]",
        "filter[1]",
        "tribble[1]",
        "starts_with[1]",
        "first[1], last[1]"
      )
    ) |>
      knitr::kable(format = "html", table.attr = "class = 'usedthese'") |>
      kableExtra::kable_styling("striped")
  )
})

# unlink("~/R Projects/usedthese/tests/testthat/usedthese.R")
