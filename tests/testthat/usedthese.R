knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

options(tidyverse.quiet = TRUE)
library(conflicted)
library(tidyverse)
conflict_prefer_all("dplyr", quiet = TRUE)
library(usedthese)
library(xts)

conflict_scout()

tribble(~group, ~a1, ~a2, ~b1,
        "x", 1, 2, 3,
        "x", 4, 5, 6,
        "y", 7, 8, 9) |> 
  select(-starts_with("b")) |> 
  filter(group == "x") |> 
  mutate(first_a1 = first(a1),
         last_a2 = last(a2))

used_here()
