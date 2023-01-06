
<!-- README.md is generated from README.Rmd. Please edit that file -->

# usedthese

<!-- badges: start -->
<!-- badges: end -->

## Overview

usedthese is a family of functions to summarise package & function usage
consistent with knitr’s syntax highlighting:

1.  `used_here()` adds a summary table of usage to the foot of a
    rendered Quarto or R Markdown document.

2.  `used_there()` aggregates usage for your website ready for analysis.
    \[Function planned.\]

3.  `used_elsewhere()` aggregates usage for any opt-in website to
    support a broader view of R package & function usage. \[Function
    planned.\]

4.  `used_where()` finds pages on your website (or opt-in websites) that
    use a specified R function. \[Function planned.\]

## Example Use Case

Each “little project” on
[quantumjitter.com](https://www.quantumjitter.com/project/) focuses on a
data science technique or machine learning model to analyse real-world
data. At the foot of each of these is a concise quantified view of the R
packages and functions used in the associated code. This is achieved by
including `used_here()` in each Quarto document.

`used_there()` harvests all the tables created by `used_here()` to
enable a [Favourite Things](https://www.quantumjitter.com/project/box/)
article on how R packages and functions are used right across
quantumjitter.com.

Using these functions makes it easier to check for consistency of usage
and spot use of superseded functions ripe for an update to the latest
and greatest.

When an updated version of dplyr introduced the `.by` argument for
mutate and friends, for example, it was helpful to see how many times
`group_by()` and `ungroup()` had been used across all projects. And
after updating the projects where it made sense to do so, it was
interesting to see the overall impact of the changes on usage.

The aggregated view in [Favourite
Things](https://www.quantumjitter.com/project/box/) also identifies the
most important packages to keep up-to-date, or go deeper, on.

## Installation

You can install the development version of usedthese from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("cgoo4/usedthese")
```