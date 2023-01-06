library(tidyverse)
library(xts, exclude = 'first')

test_that("Basic Test", {
  expect_equal(
    used_here("library(tidyverse)\nlibrary(xts, exclude = 'first')\ntribble(~group, ~a1, ~a2, ~b1,\n'x', 1, 2, 3,\n'x', 4, 5, 6,\n'y', 7, 8, 9) |>\nselect(-starts_with('b')) |>\nfilter(group == 'x') |>\nmutate(first_a1 = first(a1), last_a2 = last(a2))"),
    tibble::tibble(
      Package = c("[DUPE]dplyr", "[DUPE]xts", "base", "dplyr", "tibble", "tidyselect"),
      Function = c(
        "last[1]",
        "last[1]",
        "library[2]",
        "filter[1];  first[1];  mutate[1];  select[1]",
        "tribble[1]",
        "starts_with[1]"
      )
    ) |>
      knitr::kable(format = "html", table.attr = "class = 'usedthese'") |>
      kableExtra::kable_styling("striped")
  )
})
