library(tibble)
library(dplyr)

tibble(x = c(1, 2, 3)) |>
  summarise(x = sum(x))
